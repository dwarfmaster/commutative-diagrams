
module Make(PA: Pa.ProofAssistant) = struct
  module St = Hyps.Make(PA.M)
  module Gr = Graph.Make(PA)
  module Sd = Serde.Make(PA)
  module R = Real.Make(PA)
  module Builder = Graphbuilder.Make(PA)
  type 'a m = ('a,PA.t) St.t
  open St.Combinators
  let fail msg = msg |> PA.fail |> lift
  let message msg = msg |> PA.message |> lift
  let warning msg = msg |> PA.warning |> lift

  let rec whileM (cond : unit -> bool m) (body : unit -> unit m) : unit m =
    let* c = cond () in
    if c then
      let* _ = body () in whileM cond body
    else ret ()

  type goal =
    | GGraph of Gr.graph
    | GPrint of string * Gr.graph
    | GNormalize of PA.t Data.morphism * PA.t Data.morphism
    | GSolve of int * Gr.graph * PA.t Data.morphism * PA.t Data.morphism
  type remote = 
    { stdin: out_channel
    ; stdout: in_channel
    ; stderr: in_channel
    ; goal: goal
    }

  (* TODO find a better way to discover it, instead of hardcoding the path *)
  let engine_path = "/home/luc/repos/coq-commutative-diagrams/engine/target/debug/commutative-diagrams-engine"

  let start_remote (goal : goal) : remote m =
    let args =
      match goal with
      | GGraph _ -> [ "embed" ]
      | GPrint (path,_) -> [ "embed"; "--print"; path ]
      | GNormalize _ -> [ "embed"; "--normalize" ]
      | GSolve (level,_,_,_) -> [ "embed"; Printf.sprintf "--autosolve=%d" level ]
      in
    let (stdout,stdin,stderr) =
      Unix.open_process_args_full
        engine_path
        (Array.of_list ("commutative-diagrams-engine" :: args))
        (Unix.environment ()) in
    ret { stdin = stdin; stdout = stdout; stderr = stderr; goal = goal }

  let is_none opt : bool =
    match opt with
    | Some _ -> true
    | None -> false

  let invalid_message (_ : unit) : 'a m =
    let* _ = fail "Incoming message ill formed" in assert false

  type [@warning "-37"] handler_ret =
    | HError of string
    | HRet of Msgpack.t
    | HFinish
    | HFail
    | HNil
  type handler = Msgpack.t list -> handler_ret m

  (* Credits ChatGPT *)
  let mapiM_list (init : 'b list) (f: int -> 'a -> 'b option m) (arr: 'a array) : 'b list m =
    let len = Array.length arr in
    let res = ref init in
    let rec loop i =
      if i < 0 then ret !res
      else begin
        let elt = Array.get arr i in
        let* b = f i elt in
        begin match b with
        | Some b -> res := b :: !res
        | None -> ()
        end;
        loop (i - 1)
      end
    in
    loop (len - 1)

  let serialize_obj_on (tl : Msgpack.t list) (objs : 'a array)
                       (data : 'a -> PA.t option) (mk_id : int -> int)
                     : Msgpack.t list m =
    objs
    |> mapiM_list tl 
         (fun id obj ->
           let obj = data obj in
           match obj with
           | None -> ret None
           | Some obj ->
               let* pcat = lift (PA.print obj) in
               ret (Some (Msgpack.Array [ Msgpack.Integer (mk_id id); Msgpack.String pcat ])))

  let prepare_atom a =
    let open Data in
    match a with
    | Ctx (_,e) -> Some e
    | Evar _ -> None

  let handle_hyps (args : Msgpack.t list) : handler_ret m =
    let* cats = St.getCategories () in
    let* rt =
      serialize_obj_on [] cats 
        (fun cat -> prepare_atom cat.Data.cat_atom) Sd.mk_cat_id in
    let* functs = St.getFunctors () in
    let* rt =
      serialize_obj_on rt functs
        (fun funct -> prepare_atom funct.Data.funct_atom) Sd.mk_funct_id in
    let* elems = St.getElems () in
    let* rt =
      serialize_obj_on rt elems
        (fun elem -> prepare_atom elem.Data.elem_atom) Sd.mk_elem_id in
    let* mphs = St.getMorphisms () in
    let* rt =
      serialize_obj_on rt mphs
        (fun mph -> prepare_atom mph.Data.mph_atom) Sd.mk_mph_id in
    let* eqs = St.getEqs () in
    let* rt =
      serialize_obj_on rt eqs
        (fun eq -> prepare_atom eq.Data.eq_atom) Sd.mk_eq_id in
    ret (HRet (Msgpack.Array rt))

  let handle_goal (goal: goal) (args : Msgpack.t list) : handler_ret m =
    let* _ = message "Sending goal" in
    match goal with
    | GGraph goal ->
        let* goal_mp = Gr.Serde.pack goal in
        ret (HRet goal_mp)
    | GPrint (_,goal) ->
        let* goal_mp = Gr.Serde.pack goal in
        ret (HRet goal_mp)
    | GNormalize (m1,m2) ->
        let* p1 = Sd.Mph.pack m1 in
        let* p2 = Sd.Mph.pack m2 in
        ret (HRet (Msgpack.Array [ p1; p2 ]))
    | GSolve (_,goal,_,_) ->
        let* goal_mp = Gr.Serde.pack goal in
        ret (HRet goal_mp)

  let add_universes_constraints (env : Environ.env) (c : EConstr.t) (sigma : Evd.evar_map) : Evd.evar_map * EConstr.t =
    Typing.solve_evars env sigma c

  let handle_refine (goal: goal) (args : Msgpack.t list) : handler_ret m =
    match goal with
    | GNormalize _ -> ret (HError "Current goal is normalization")
    | GPrint _ -> ret (HError "Current goal is print")
    | GSolve _ -> ret (HError "Current goal is solve")
    | GGraph _ -> begin
      (* TODO do something *)
      ret HFinish
    end

  let handle_norm (goal: goal) (args: Msgpack.t list) : handler_ret m =
    match goal with
    | GGraph _ -> ret (HError "Current goal is graph")
    | GPrint _ -> ret (HError "Current goal is print")
    | GSolve _ -> ret (HError "Current goal is solve")
    | GNormalize _ -> begin
      let* (mph1,eq1,mph2,eq2) =
        match args with
        | [ Msgpack.Array [ mph1_pk; eq1_pk ]; Msgpack.Array [ mph2_pk; eq2_pk ] ] -> begin
          let* mph1 = Sd.Mph.unpack mph1_pk in
          let* eq1 = Sd.Eq.unpack eq1_pk in
          let* mph2 = Sd.Mph.unpack mph2_pk in
          let* eq2 = Sd.Eq.unpack eq2_pk in
          match mph1,eq1,mph2,eq2 with
          | Some mph1, Some eq1, Some mph2, Some eq2 -> ret (mph1,eq1,mph2,eq2)
          | _ -> let* _ = fail "Expected a morphism and an equality from engine" in assert false
        end
        | _ -> let* _ = fail "Expected 2 arguments from engine" in assert false in
      let* evar = St.newEvar () in
      let hole = let open Data in
        { eq_atom = Evar (evar,None)
        ; eq_right_ = mph1
        ; eq_left_ = mph2
        ; eq_cat_ = morphism_cat mph1
        ; eq_src_ = morphism_src mph1
        ; eq_dst_ = morphism_dst mph1
        } in
      let eq = Data.Concat (eq1, Data.Concat (Data.AtomicEq hole, Data.InvEq eq2)) in
      let* eq = R.realizeEq (SimplEq.simpl eq) in
      let* env = lift (PA.env ()) in
      let* _ =
        lift (PA.lift_tactic (Refine.refine ~typecheck:false
          (add_universes_constraints env (PA.to_econstr eq)))) in
      ret HFinish
    end

  let handle_tosolve (goal: goal) (args: Msgpack.t list) : handler_ret m =
    match goal with
    | GSolve (_,_,left,right) ->
        let* lmp = Sd.Mph.pack left in
        let* rmp = Sd.Mph.pack right in
        ret (HRet (Msgpack.Array [ lmp; rmp ]))
    | _ -> ret (HError "Invalid current goal")

  let handle_printed _ : handler_ret m =
    ret HFinish

  let handle_unsolvable _ : handler_ret m =
    ret HFail

  let handle_failed _ : handler_ret m =
    ret HFail

  let handle_solved (args: Msgpack.t list) : handler_ret m =
    match args with
    | [ eq ] ->
        let* eq = Sd.Eq.unpack eq in
        begin match eq with
        | Some eq ->
            let* eq = R.realizeEq (SimplEq.simpl eq) in
            let* env = lift (PA.env ()) in
            let* _ = lift (PA.lift_tactic (Refine.refine ~typecheck:false
              (add_universes_constraints env (PA.to_econstr eq)))) in
            ret HFinish
        | None -> ret (HError "solved expect one equality")
        end
    | _ -> ret (HError "solved expects exactly one argument")

  let run_handler (rm : remote) (msgid : int) (args : Msgpack.t list) (h : handler) : (bool * bool) m =
    let* rt = h args in
    let finish = rt = HFinish || rt = HFail in
    let fail = rt = HFail in
    let (err,rt) =
      match rt with
      | HError err -> (Msgpack.String err, Msgpack.Nil)
      | HRet rt -> (Msgpack.Nil, rt)
      | HFinish -> (Msgpack.Nil, Msgpack.Nil)
      | HFail -> (Msgpack.Nil, Msgpack.Nil)
      | HNil -> (Msgpack.Nil, Msgpack.Nil) in
    let response = Msgpack.Array [ Msgpack.Integer 1; Msgpack.Integer msgid; err; rt ] in
    let response = Msgpack.serialize response in
    Out_channel.output_bytes rm.stdin response;
    Out_channel.flush rm.stdin;
    ret (finish, fail)

  let count = ref 0

  let handle_message (rm : remote) (msg : Msgpack.t) : (bool * bool) m =
    Out_channel.with_open_text
      (Printf.sprintf "received_%d.json" !count)
      (fun out -> Msgpack.to_json out msg);
    count := !count + 1;
    match msg with
    | Msgpack.Array [ Msgpack.Integer 0
                    ; Msgpack.Integer msgid
                    ; Msgpack.String mtd
                    ; params ] -> begin
      let* params = match params with
        | Msgpack.Array params -> ret params
        | Msgpack.Nil -> ret []
        | _ -> let* _ = fail "Params is not a vector" in assert false in
      match mtd with
      | "goal" -> run_handler rm msgid params (handle_goal rm.goal)
      | "hyps" -> run_handler rm msgid params handle_hyps
      | "refine" -> run_handler rm msgid params (handle_refine rm.goal)
      | "failed" -> run_handler rm msgid params handle_failed
      | "normalized" -> run_handler rm msgid params (handle_norm rm.goal)
      | "printed" -> run_handler rm msgid params handle_printed
      | "tosolve" -> run_handler rm msgid params (handle_tosolve rm.goal)
      | "unsolvable" -> run_handler rm msgid params handle_unsolvable
      | "solved" -> run_handler rm msgid params handle_solved
      | _ -> let* _ = fail "Unknown method" in assert false
    end
    | _ -> let* _ = fail "Ill formed rpc message" in assert false

  type action =
    | Graph of PA.t Data.morphism * PA.t Data.morphism
    | Normalize of PA.t Data.morphism * PA.t Data.morphism
    | Print of string
    | Solve of int * PA.t Data.morphism * PA.t Data.morphism

  let run (act: action) : unit m =
    let* goal = 
      match act with
      | Graph (left,right) -> 
          let goal = Builder.empty () in
          let* goal = Builder.import_hyps goal in
          let* evar = St.newEvar () in
          let hole = let open Data in 
            { eq_atom = Evar (evar,None)
            ; eq_left_ = left
            ; eq_right_ = right
            ; eq_cat_ = morphism_cat right
            ; eq_src_ = morphism_src right
            ; eq_dst_ = morphism_dst right
            } in
          let goal = Builder.add_face (Data.AtomicEq hole) goal in
          let goal = Builder.build goal in
          GGraph goal |> ret
      | Normalize (left,right) ->
          ret (GNormalize (left,right))
      | Print path -> 
          let goal = Builder.empty () in
          let* goal = Builder.import_hyps goal in
          ret (GPrint (path, Builder.build goal))
      | Solve (level,left,right) ->
          let goal = Builder.empty () in
          let* goal = Builder.import_hyps goal in
          let goal = Builder.build goal in
          ret (GSolve (level, goal, left, right))
      in
    let* rm = start_remote goal in
    let parser = Msgpack.make_parser rm.stdout in
    let finish = ref false in
    let failed = ref false in
    let* _ = whileM (fun () -> ret (not !finish)) (fun () ->
      match Msgpack.parse parser with
      | Msgpack.Eof ->
          (* Failed to finish *)
          finish := true;
          let err = In_channel.input_line rm.stderr in
          let* _ = match err with
          | Some err -> warning err
          | None -> ret () in
          fail "Connection interrupted"
      | Msgpack.Error err ->
          let* _ = warning err in
          let* _ = fail "Error in engine" in
          ret ()
      | Msgpack.Msg msg -> 
          let* (f,fl) = handle_message rm msg in
          finish := f;
          failed := fl;
          ret ()
      ) in
    if !failed then fail "Unsolvable goal" else ret ()
end
