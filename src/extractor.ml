
let (++) = Pp.(++)


(*  _____                 _                _ *)
(* |_   _|__  _ __       | | _____   _____| | *)
(*   | |/ _ \| '_ \ _____| |/ _ \ \ / / _ \ | *)
(*   | | (_) | |_) |_____| |  __/\ V /  __/ | *)
(*   |_|\___/| .__/      |_|\___| \_/ \___|_| *)
(*           |_| *)
(* Top-level *)
module HP = HypsPrinter
let (let*) = Proofview.tclBIND
let ret = Proofview.tclUNIT

let extract_hyp : Environ.env -> Hyps.t
  -> EConstr.named_declaration -> Hyps.t Proofview.tactic =
  fun env store dec ->
  let* sigma = Proofview.tclEVARMAP in
  let name,tp = match dec with
    | Context.Named.Declaration.LocalAssum (name,tp) -> (name.binder_name, tp)
    | Context.Named.Declaration.LocalDef (name,_,tp) -> (name.binder_name, tp) in
  let* (store,_) = Hyps.parse_cat  name tp store in
  let* (store,_) = Hyps.parse_elem name tp store in
  let* (store,_) = Hyps.parse_mph  name tp store in
  let* (store,_) = Hyps.parse_face name tp store in
  let* (store,_) = Hyps.parse_mono name tp store in
  let* (store,_) = Hyps.parse_epi  name tp store in
  let* (store,_) = Hyps.parse_iso  name tp store in
  (* The evar map may have been changed by the previous call, so we update it *)
  let* sigma = Proofview.tclEVARMAP in
  Proofview.tclUNIT store


let name : string -> Names.Name.t = fun s -> Names.Name.mk_name (Names.Id.of_string s)

let rec fold_leftM : ('a -> 'b -> 'a Proofview.tactic) -> 'a -> 'b list
  -> 'a Proofview.tactic =
  fun pred acc -> function
    | [ ] -> Proofview.tclUNIT acc
    | x :: xs ->
      let* acc = pred acc x in
      let* acc = fold_leftM pred acc xs in
      Proofview.tclUNIT acc

let add_universes_constraints : Environ.env -> EConstr.t -> EConstr.t Proofview.tactic =
  fun env c ->
  let* sigma = Proofview.tclEVARMAP in
  let (sigma,_) = Typing.solve_evars env sigma c in
  Proofview.tclTHEN
    (Proofview.Unsafe.tclEVARS sigma)
    (Proofview.tclUNIT c)

let extract_hyps : Proofview.Goal.t
  -> (Hyps.t * (Hyps.path*Hyps.path) option) Proofview.tactic = fun goal ->
  let store = Hyps.empty_context in
  let env   = Proofview.Goal.env goal in
  let context = Proofview.Goal.hyps goal in
  let goal = Proofview.Goal.concl goal in
  let* store = fold_leftM (extract_hyp env) store context in
  Hyps.read_face goal store

let extract' : string -> Proofview.Goal.t -> unit Proofview.tactic = fun path goal ->
  let* (store,_) = extract_hyps goal in
  let* sigma = Proofview.tclEVARMAP in
  let* env = Proofview.tclENV in
  let pp = HP.to_graphviz sigma env store in
  let oc = open_out path in
  Printf.fprintf oc "%s\n" (Pp.string_of_ppcmds pp);
  flush oc;
  close_out oc;
  Tacticals.tclIDTAC
let extract : string -> unit Proofview.tactic = fun path -> Proofview.Goal.enter_one (extract' path)

let normalize' : Proofview.Goal.t -> unit Proofview.tactic = fun goal ->
  let* (store,obj) = extract_hyps goal in
  match obj with
  | None -> Tacticals.tclFAIL 0 (Pp.str "Goal is not a face")
  | Some (side1,side2) ->
    let env = Proofview.Goal.env goal in
    let* eq1 = Hyps.real_eq side1.eq in
    let* eq1 = add_universes_constraints env eq1 in
    let* eq2 = Hyps.real_eq side2.eq in
    let* eq2 = add_universes_constraints env eq2 in
    let* _ = Tactics.pose_tac (name "H1") eq1 in
    let* _ = Tactics.pose_tac (name "H2") eq2 in
    Tacticals.tclIDTAC
let normalize : unit -> unit Proofview.tactic = fun _ -> Proofview.Goal.enter_one normalize'

let solve' : int -> Proofview.Goal.t -> unit Proofview.tactic = fun level goal ->
  let* (store,obj) = extract_hyps goal in
  match obj with
  | None -> Tacticals.tclFAIL 0 (Pp.str "Goal is not a face")
  | Some (side1,side2) ->
    let* commuter = Commutation.build store level in
    let* sol = Commutation.query side1 side2 commuter in
    match sol with
    | None -> Tacticals.tclFAIL 0 (Pp.str "Couldn't make goal commute")
    | Some eq ->
      let env = Proofview.Goal.env goal in
      let* eq = Hyps.real_eq eq in
      let* eq = add_universes_constraints env eq in
      let* _ = Tactics.pose_tac (name "Hsolv") eq in
      Tacticals.tclIDTAC
let solve : int -> unit Proofview.tactic = fun level -> Proofview.Goal.enter_one (solve' level)
