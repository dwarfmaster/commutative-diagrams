
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

let print_hyp : Environ.env -> Hyps.t
  -> EConstr.named_declaration -> (Hyps.t * Pp.t) Proofview.tactic =
  fun env store dec ->
  let* sigma = Proofview.tclEVARMAP in
  let name,tp = match dec with
    | Context.Named.Declaration.LocalAssum (name,tp) -> (name.binder_name, tp)
    | Context.Named.Declaration.LocalDef (name,_,tp) -> (name.binder_name, tp) in
  let ppconstr = Printer.pr_econstr_env env sigma in
  let* (store,is_cat) = Hyps.parse_cat  name tp store in
  let* (store,is_elm) = Hyps.parse_elem name tp store in
  let* (store,is_mph) = Hyps.parse_mph  name tp store in
  let* (store,is_fce) = Hyps.parse_face name tp store in
  let* (store,_)      = Hyps.parse_mono name tp store in
  let* (store,_)      = Hyps.parse_epi  name tp store in
  let* (store,_)      = Hyps.parse_iso  name tp store in
  (* The evar map may have been changed by the previous call, so we update it *)
  let* sigma = Proofview.tclEVARMAP in
  let cat = match is_cat with
    | None -> Pp.str ""
    | Some id -> HP.cat sigma env store.categories.(id) ++ Pp.str " " in
  let elm = match is_elm with
    | None -> Pp.str ""
    | Some id -> HP.elem sigma env store.elems.(id) ++ Pp.str " " in
  let mph = match is_mph with
    | None -> Pp.str ""
    | Some id ->
      let mph = store.morphisms.(id) in HP.mph sigma env mph ++ Pp.str " " in
  let fce = match is_fce with
    | None -> Pp.str ""
    | Some id ->
      let fce = store.faces.(id) in HP.face sigma env fce ++ Pp.str " " in
  Proofview.tclUNIT
    (store,
     Names.Id.print name ++ Pp.str " : " ++ ppconstr tp
     ++ Pp.str " [ " ++ cat ++ elm ++ mph ++ fce ++ Pp.str "]")


let name : string -> Names.Name.t = fun s -> Names.Name.mk_name (Names.Id.of_string s)

let rec fold_left_mapM : ('a -> 'b -> ('a * 'c) Proofview.tactic) -> 'a -> 'b list
  -> ('a * 'c list) Proofview.tactic =
  fun pred acc -> function
    | [ ] -> Proofview.tclUNIT (acc, [])
    | x :: xs ->
      let* (acc,y) = pred acc x in
      let* (acc,ls) = fold_left_mapM pred acc xs in
      Proofview.tclUNIT (acc, y :: ls)

let add_universes_constraints : Environ.env -> EConstr.t -> EConstr.t Proofview.tactic =
  fun env c ->
  let* sigma = Proofview.tclEVARMAP in
  let (sigma,_) = Typing.solve_evars env sigma c in
  Proofview.tclTHEN
    (Proofview.Unsafe.tclEVARS sigma)
    (Proofview.tclUNIT c)

let extract_goal : Proofview.Goal.t -> Pp.t Proofview.tactic = fun goal ->
  let store = Hyps.empty_context in
  let sigma = Tacmach.project goal in
  let env   = Proofview.Goal.env goal in
  let ppconstr = Printer.pr_econstr_env env sigma in
  let context = Proofview.Goal.hyps goal in
  let goal = Proofview.Goal.concl goal in
  let* (store,ctx) = fold_left_mapM (print_hyp env) store context in
  let* (store,obj) = Hyps.read_face goal store in
  let ppconcl = Pp.str "Focusing goal" ++ ppconstr goal ++ Pp.fnl () in
  let pp = ppconcl ++ Pp.pr_vertical_list (fun h -> h) ctx ++ HP.to_graphviz sigma env store in
  let* commuter = Commutation.build store 6 in
  match obj with
  | None -> Proofview.tclUNIT pp
  | Some (side1,side2) ->
    let* sol = Commutation.query side1 side2 commuter in
    match sol with
    | None ->
      let* eq1 = Hyps.real_eq side1.eq in
      let* eq1 = add_universes_constraints env eq1 in
      let* eq2 = Hyps.real_eq side2.eq in
      let* eq2 = add_universes_constraints env eq2 in
      Proofview.tclTHEN
        (Tactics.pose_tac (name "H1") eq1)
        (Proofview.tclTHEN
           (Tactics.pose_tac (name "H2") eq2)
           (Proofview.tclUNIT pp))
    | Some eq ->
      let* eq = Hyps.real_eq eq in
      let* eq = add_universes_constraints env eq in
      Proofview.tclTHEN
        (Tactics.pose_tac (name "Hsolv") eq)
        (Proofview.tclUNIT pp)


let extract : string -> unit Proofview.tactic = fun path ->
  let oc = open_out path in
  let* pp = Proofview.Goal.enter_one extract_goal in
  Printf.fprintf oc "%s\n" (Pp.string_of_ppcmds pp);
  flush oc;
  close_out oc;
  Tacticals.tclIDTAC
