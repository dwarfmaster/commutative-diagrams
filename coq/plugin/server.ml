
type 'a m = 'a Hyps.t
open Hyps.Combinators

let rec whileM (cond : unit -> bool m) (body : unit -> unit m) : unit m =
  let* c = cond () in
  if c then
    let* _ = body () in whileM cond body
  else ret ()

let rec forM (lst: 'a list) (f : 'a -> 'b m) : 'b list m =
  match lst with
  | [] -> ret []
  | x :: xs ->
      let* x = f x in
      let* xs = forM xs f in
      ret (x :: xs)

type goal =
  | GGraph of string option * bool * int * Graph.graph * Lemmas.t list
  | GPrint of string * Graph.graph
  | GNormalize of Data.morphism * Data.morphism
  | GSolve of int * Graph.graph * Data.morphism * Data.morphism
type remote = 
  { stdin: out_channel
  ; stdout: in_channel
  ; stderr: in_channel
  ; goal: goal
  }

let start_remote (goal : goal) : remote m =
  let args =
    match goal with
    | GGraph (file,_,_,_,_) -> List.concat [
      [ "embed" ];
      (match file with
      | Some file -> [ "--state"; file ]
      | None -> [ ]);
    ]
    | GPrint (path,_) -> [ "embed"; "--print"; path ]
    | GNormalize _ -> [ "embed"; "--normalize" ]
    | GSolve (level,_,_,_) -> [ "embed"; Printf.sprintf "--autosolve=%d" level ]
    in
  let engine_path = Sys.getenv_opt "COMDIAG_ENGINE" in
  match engine_path with
  | Some engine_path ->
      let (stdout,stdin,stderr) =
        Unix.open_process_args_full
          engine_path
          (Array.of_list ("commutative-diagrams-engine" :: args))
          (Unix.environment ()) in
      ret { stdin = stdin; stdout = stdout; stderr = stderr; goal = goal }
  | None -> fail "$COMDIAG_ENGINE not found in environment"

let is_none opt : bool =
  match opt with
  | Some _ -> true
  | None -> false

let invalid_message (_ : unit) : 'a m =
  fail "Incoming message ill formed"

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
                     (data : 'a -> EConstr.t option) (mk_id : int -> int)
                   : Msgpack.t list m =
  objs
  |> mapiM_list tl 
       (fun id obj ->
         let obj = data obj in
         match obj with
         | None -> ret None
         | Some obj ->
             let* pcat = print obj in
             ret (Some (Msgpack.Array [ Msgpack.Integer (mk_id id); Msgpack.String pcat ])))

let prepare_atom a =
  let open Data in
  match a with
  | Ctx (_,e) -> Some e
  | _ -> None

let handle_hyps (args : Msgpack.t list) : handler_ret m =
  let* cats = Hyps.getCategories () in
  let* rt =
    serialize_obj_on [] cats 
      (fun cat -> prepare_atom cat.Data.cat_atom) Hyps.catFromIndex in
  let* functs = Hyps.getFunctors () in
  let* rt =
    serialize_obj_on rt functs
      (fun funct -> prepare_atom funct.Data.funct_atom) Hyps.functorFromIndex in
  let* elems = Hyps.getElems () in
  let* rt =
    serialize_obj_on rt elems
      (fun (elem,_) -> prepare_atom elem.Data.elem_atom) Hyps.elemFromIndex in
  let* mphs = Hyps.getMorphisms () in
  let* rt =
    serialize_obj_on rt mphs
      (fun (mph,_) -> prepare_atom mph.Data.mph_atom) Hyps.mphFromIndex in
  let* eqs = Hyps.getEqs () in
  let* rt =
    serialize_obj_on rt eqs
      (fun (eq,_) -> prepare_atom eq.Data.eq_atom) Hyps.eqFromIndex in
  ret (HRet (Msgpack.Array rt))

let handle_goal (goal: goal) (args : Msgpack.t list) : handler_ret m =
  let* _ = message "Sending goal" in
  match goal with
  | GGraph (_,_,_,goal,_) ->
      let* goal_mp = Graph.Serde.pack goal in
      ret (HRet goal_mp)
  | GPrint (_,goal) ->
      let* goal_mp = Graph.Serde.pack goal in
      ret (HRet goal_mp)
  | GNormalize (m1,m2) ->
      let* p1 = Serde.Mph.pack m1 in
      let* p2 = Serde.Mph.pack m2 in
      ret (HRet (Msgpack.Array [ p1; p2 ]))
  | GSolve (_,goal,_,_) ->
      let* goal_mp = Graph.Serde.pack goal in
      ret (HRet goal_mp)

let handle_lemmas (goal: goal) (args : Msgpack.t list) : handler_ret m =
  match goal with
  | GGraph (_,_,_,_,lemmas) ->
      let* lemmas_mp =
        (fun arr -> Msgpack.Array arr) <$>
          (forM lemmas
            (fun lemma -> 
              let* graph = Lemmas.instantiate lemma in
              let* graph = Graph.Serde.pack graph in
              Msgpack.Array [ Msgpack.String (Lemmas.name lemma); graph ] |> ret)) in
      ret (HRet lemmas_mp)
  | _ -> ret (HError "Lemmas only available on graph goal")

let add_universes_constraints (env : Environ.env) (c : EConstr.t) (sigma : Evd.evar_map) : Evd.evar_map * EConstr.t =
  Typing.solve_evars env sigma c

let handle_refine (goal: goal) (args : Msgpack.t list) : handler_ret m =
  match goal with
  | GNormalize _ -> ret (HError "Current goal is normalization")
  | GPrint _ -> ret (HError "Current goal is print")
  | GSolve _ -> ret (HError "Current goal is solve")
  | GGraph (_,_,evar,_,_) -> begin
    let subst = ref None in
    let rec process_arguments lst =
      match lst with
      | [] -> ret false
      | (Msgpack.Array [Msgpack.Integer id; rf]) :: t ->
          let* r = process_arguments t in
          if id = evar then begin
            let* eq = Serde.Eq.unpack rf in
            match eq with
            | Some eq -> subst := Some eq; ret true
            | None ->
                fail "Couldn't parse refinement"
          end else ret r
      | _ :: _ ->
          fail "Refinement is not a pair of an evar and a value"
      in
    let* res = process_arguments args in
    if res then begin
      match !subst with
      | Some eq ->
          let* eq = Real.realizeEq eq in
          let* env = env () in
          let* _ = lift (Refine.refine ~typecheck:false
            (add_universes_constraints env eq)) in
          ret HFinish
      | None -> ret (HError "No refinement found for goal variable")
    end else ret (HError "Invalid refine arguments")
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
        let* mph1 = Serde.Mph.unpack mph1_pk in
        let* eq1 = Serde.Eq.unpack eq1_pk in
        let* mph2 = Serde.Mph.unpack mph2_pk in
        let* eq2 = Serde.Eq.unpack eq2_pk in
        match mph1,eq1,mph2,eq2 with
        | Some mph1, Some eq1, Some mph2, Some eq2 -> ret (mph1,eq1,mph2,eq2)
        | _ -> fail "Expected a morphism and an equality from engine"
      end
      | _ -> fail "Expected 2 arguments from engine" in
    let* evar = Hyps.newEvar () in
    let hole = let open Data in
      { eq_atom = Evar (evar,None)
      ; eq_left_ = mph1
      ; eq_right_ = mph2
      ; eq_cat_ = morphism_cat mph1
      ; eq_src_ = morphism_src mph1
      ; eq_dst_ = morphism_dst mph1
      } in
    let eq = Data.Concat (eq1, Data.Concat (Data.AtomicEq hole, Data.InvEq eq2)) in
    let* eq = Real.realizeEq eq in
    let* env = env () in
    let* _ =
      lift (Refine.refine ~typecheck:false
        (add_universes_constraints env eq)) in
    ret HFinish
  end

let handle_tosolve (goal: goal) (args: Msgpack.t list) : handler_ret m =
  match goal with
  | GSolve (_,_,left,right) ->
      let* lmp = Serde.Mph.pack left in
      let* rmp = Serde.Mph.pack right in
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
      let* eq = Serde.Eq.unpack eq in
      begin match eq with
      | Some eq ->
          let* eq = Real.realizeEq eq in
          let* env = env () in
          let* _ = lift (Refine.refine ~typecheck:false
            (add_universes_constraints env eq)) in
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
      | _ -> fail "Params is not a vector" in
    match mtd with
    | "goal" -> run_handler rm msgid params (handle_goal rm.goal)
    | "hyps" -> run_handler rm msgid params handle_hyps
    | "lemmas" -> run_handler rm msgid params (handle_lemmas rm.goal)
    | "refine" -> run_handler rm msgid params (handle_refine rm.goal)
    | "failed" -> run_handler rm msgid params handle_failed
    | "normalized" -> run_handler rm msgid params (handle_norm rm.goal)
    | "printed" -> run_handler rm msgid params handle_printed
    | "tosolve" -> run_handler rm msgid params (handle_tosolve rm.goal)
    | "unsolvable" -> run_handler rm msgid params handle_unsolvable
    | "solved" -> run_handler rm msgid params handle_solved
    | _ -> fail "Unknown method"
  end
  | _ -> fail "Ill formed rpc message"

type action =
  | Graph of string option * bool * Data.morphism * Data.morphism * Lemmas.t list
  | Normalize of Data.morphism * Data.morphism
  | Print of string
  | Solve of int * Data.morphism * Data.morphism

let run (act: action) : unit m =
  let* goal = 
    match act with
    | Graph (file,force,left,right,lemmas) -> 
        let goal = Graphbuilder.empty () in
        (* TODO add hypothesis to goal graph *)
        let* evar = Hyps.newEvar () in
        (* let hole = let open Data in  *)
        (*   { eq_atom = Evar (evar,None) *)
        (*   ; eq_left_ = left *)
        (*   ; eq_right_ = right *)
        (*   ; eq_cat_ = morphism_cat right *)
        (*   ; eq_src_ = morphism_src right *)
        (*   ; eq_dst_ = morphism_dst right *)
        (*   } in *)
        (* let goal = Graphbuilder.add_face ~important:true (Data.AtomicEq hole) goal in *)
        (* TODO build goal face *)
        let goal = Graphbuilder.build goal in
        begin match goal with
        | Some goal -> GGraph (file,force,evar,goal,lemmas) |> ret
        | None -> fail "Couldn't build goal graph"
        end
    | Normalize (left,right) ->
        ret (GNormalize (left,right))
    | Print path -> 
        let goal = Graphbuilder.empty () in
        (* let* goal = Graphbuilder.import_hyps goal in *)
        ret (GPrint (path, Graphbuilder.build_unsafe goal))
    | Solve (level,left,right) ->
        let goal = Graphbuilder.empty () in
        (* let* goal = Graphbuilder.import_hyps goal in *)
        let goal = Graphbuilder.build_unsafe goal in
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
        let lines = Seq.unfold (fun () -> In_channel.input_line rm.stderr
                                       |> Option.map (fun x -> (x,()))) () in
        let* _ = Seq.fold_left (fun m line -> m >>= (fun () -> warning line)) (ret ()) lines in
        fail "Connection interrupted"
    | Msgpack.Error err ->
        let* _ = warning err in
        fail "Error in engine"
    | Msgpack.Msg msg -> 
        let* (f,fl) = handle_message rm msg in
        finish := f;
        failed := fl;
        ret ()
    ) in
  if !failed then fail "Unsolvable goal" else ret ()
