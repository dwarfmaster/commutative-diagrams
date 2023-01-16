
module Make(PA: Pa.ProofAssistant) = struct
  module St = Hyps.Make(PA.M)
  module Gr = Graph.Make(PA)
  module Sd = Serde.Make(PA)
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
    | GPrint of string
    | GNormalize of PA.t Data.morphism * PA.t Data.morphism
  type remote = 
    { stdin: out_channel
    ; stdout: in_channel
    ; goal: goal
    }

  (* TODO find a better way to discover it, instead of hardcoding the path *)
  let engine_path = "/home/luc/repos/coq-commutative-diagrams/engine/target/debug/commutative-diagrams-engine"

  let start_remote (goal : goal) : remote m =
    let (stdout,stdin) =
      match goal with
      | GGraph _ -> Unix.open_process_args engine_path [| "commutative-diagrams-engine"; "embed" |]
      | GPrint path -> Unix.open_process_args engine_path [| "commutative-diagrams-engine"; "embed"; "--print"; path |]
      | GNormalize _ ->
          Unix.open_process_args engine_path [| "commutative-diagrams-engine"; "embed"; "--normalize" |] in
    ret { stdin = stdin; stdout = stdout; goal = goal }

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
    | HNil
  type handler = Msgpack.t list -> handler_ret m

  (* Credits ChatGPT *)
  let mapiM_list (init : 'b list) (f: int -> 'a -> 'b m) (arr: 'a array) : 'b list m =
    let len = Array.length arr in
    let res = ref init in
    let rec loop i =
      if i < 0 then ret !res
      else begin
        let elt = Array.get arr i in
        let* b = f i elt in
        res := b :: !res;
        loop (i - 1)
      end
    in
    loop (len - 1)

  let serialize_obj_on (tl : Msgpack.t list) (objs : 'a array)
                       (data : 'a -> PA.t) (mk_id : int -> int)
                     : Msgpack.t list m =
    objs
    |> mapiM_list tl 
         (fun id obj ->
           let* pcat = lift (PA.print (data obj)) in
           ret (Msgpack.Array [ Msgpack.Integer (mk_id id); Msgpack.String pcat ]))

  let handle_hyps (args : Msgpack.t list) : handler_ret m =
    let* cats = St.getCategories () in
    let* rt =
      serialize_obj_on [] cats 
        (fun cat -> cat.Data.cat_obj) Sd.mk_cat_id in
    let* functs = St.getFunctors () in
    let* rt =
      serialize_obj_on rt functs
        (fun funct -> funct.Data.funct_obj) Sd.mk_funct_id in
    let* elems = St.getElems () in
    let* rt =
      serialize_obj_on rt elems
        (fun elem -> elem.Data.elem_obj) Sd.mk_elem_id in
    let* mphs = St.getMorphisms () in
    let* rt =
      serialize_obj_on rt mphs
        (fun mph -> mph.Data.mph_obj) Sd.mk_mph_id in
    let* eqs = St.getEqs () in
    let* rt =
      serialize_obj_on rt eqs
        (fun eq -> eq.Data.eq_obj) Sd.mk_eq_id in
    ret (HRet (Msgpack.Array rt))

  let handle_goal (goal: goal) (args : Msgpack.t list) : handler_ret m =
    let* _ = message "Sending goal" in
    match goal with
    | GGraph goal ->
        let* goal_mp = Gr.Serde.pack goal in
        ret (HRet goal_mp)
    | GPrint _ ->
        ret (HError "No goal for printing")
    | GNormalize (m1,m2) ->
        let* p1 = Sd.Mph.pack m1 in
        let* p2 = Sd.Mph.pack m2 in
        ret (HRet (Msgpack.Array [ p1; p2 ]))

  let add_universes_constraints (env : Environ.env) (c : EConstr.t) (sigma : Evd.evar_map) : Evd.evar_map * EConstr.t =
    Typing.solve_evars env sigma c

  let handle_refine (goal: goal) (args : Msgpack.t list) : handler_ret m =
    match goal with
    | GNormalize _ -> ret (HError "Current goal is normalization")
    | GGraph _ -> begin
      (* TODO do something *)
      ret HFinish
    end
    | GPrint _ -> ret HFinish

  let handle_norm (goal: goal) (args: Msgpack.t list) : handler_ret m =
    match goal with
    | GGraph _ -> ret (HError "Current goal is graph")
    | GPrint _ -> ret (HError "Current goal is print")
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
      let eq = Data.Concat (eq1, Data.Concat (Hole (mph1,mph2), Data.InvEq eq2)) in
      let* eq = lift (PA.realizeEq (SimplEq.simpl eq)) in
      let* env = lift (PA.env ()) in
      let* _ =
        lift (PA.lift_tactic (Refine.refine ~typecheck:false
          (add_universes_constraints env (PA.to_econstr eq)))) in
      ret HFinish
    end

  let handle_printed _ : handler_ret m =
    ret HFinish

  let run_handler (rm : remote) (msgid : int) (args : Msgpack.t list) (h : handler) : bool m =
    let* rt = h args in
    let finish = rt = HFinish in
    let (err,rt) =
      match rt with
      | HError err -> (Msgpack.String err, Msgpack.Nil)
      | HRet rt -> (Msgpack.Nil, rt)
      | HFinish -> (Msgpack.Nil, Msgpack.Nil)
      | HNil -> (Msgpack.Nil, Msgpack.Nil) in
    let response = Msgpack.Array [ Msgpack.Integer 1; Msgpack.Integer msgid; err; rt ] in
    let response = Msgpack.serialize response in
    Out_channel.output_bytes rm.stdin response;
    Out_channel.flush rm.stdin;
    ret finish

  let count = ref 0

  let handle_message (rm : remote) (msg : Msgpack.t) : bool m =
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
      | "normalized" -> run_handler rm msgid params (handle_norm rm.goal)
      | "printed" -> run_handler rm msgid params handle_printed
      | _ -> let* _ = fail "Unknown method" in assert false
    end
    | _ -> let* _ = fail "Ill formed rpc message" in assert false

  type action =
    | Graph of PA.t Data.morphism * PA.t Data.morphism
    | Normalize of PA.t Data.morphism * PA.t Data.morphism
    | Print of string

  let run (act: action) : unit m =
    let goal = 
      match act with
      | Graph (left,right) -> 
          GGraph {
            Gr.gr_nodes = [| Data.morphism_src left; Data.morphism_dst left |];
            Gr.gr_edges = [| [ (1, left); (1, right) ]; [] |];
            Gr.gr_faces = [ ];
          }
      | Normalize (left,right) ->
          GNormalize (left,right)
      | Print path -> 
          GPrint path
      in
    let* rm = start_remote goal in
    let parser = Msgpack.make_parser rm.stdout in
    let finish = ref false in
    let* _ = whileM (fun () -> ret (not !finish)) (fun () ->
      match Msgpack.parse parser with
      | Msgpack.Eof ->
          (* Failed to finish *)
          finish := true;
          fail "Connection interrupted"
      | Msgpack.Error err ->
          let* _ = warning err in
          let* _ = fail "Error in engine" in
          ret ()
      | Msgpack.Msg msg -> 
          let* f = handle_message rm msg in
          finish := f;
          ret ()
      ) in
    ret ()
end
