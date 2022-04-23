
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

(* let normalize_goal : Hyps.path -> Hyps.path -> Environ.env -> Evd.evar_map -> Evd.evar_map * EConstr.t = *)
(*   fun side1 side2 env sigma -> *)
(*   (\* let src = Hyps.rpath sigma side1 in *\) *)
(*   (\* let dst = Hyps.rpath sigma side2 in *\) *)
(*   let (sigma,hole) = Evarutil.new_evar ~principal:true env sigma (Hyps.eqT sigma side1.mph side2.mph) in *)
(*   (\* let hole : Hyps.eq = *\) *)
(*   (\*   { src = src *\) *)
(*   (\*   ; dst = dst *\) *)
(*   (\*   ; tp  = src.tp *\) *)
(*   (\*   ; eq  = hole } in *\) *)
(*   (\* let eq = Hyps.concat sigma side1.eq (Hyps.concat sigma hole (Hyps.inv sigma side2.eq)) in *\) *)
(*   (sigma,hole) *)

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

let extract_goal : Pp.t ref -> Proofview.Goal.t -> unit Proofview.tactic = fun pp goal ->
  let store = Hyps.empty_context in
  let sigma = Tacmach.project goal in
  let env   = Proofview.Goal.env goal in
  let ppconstr = Printer.pr_econstr_env env sigma in
  let context = Proofview.Goal.hyps goal in
  let goal = Proofview.Goal.concl goal in
  let* (store,ctx) = fold_left_mapM (print_hyp env) store context in
  let* (store,obj) = Hyps.read_face goal store in
  let ppconcl = Pp.str "Focusing goal" ++ ppconstr goal ++ Pp.fnl () in
  pp := !pp ++ ppconcl ++ Pp.pr_vertical_list (fun h -> h) ctx ++ HP.to_graphviz sigma env store;
  match obj with
  | None -> Proofview.tclUNIT ()
  | Some (side1,side2) ->
    let* eq = add_universes_constraints env side2.eq.eq in
    Tactics.pose_tac (name "Hnorm") eq
  (* | Some (side1,side2) -> Refine.refine ~typecheck:true (normalize_goal side1 side2 env) *)

let extract : string -> unit Proofview.tactic = fun path ->
  let oc = open_out path in
  let pp = ref (Pp.str "") in
  Proofview.tclTHEN
    (Proofview.Goal.enter (extract_goal pp))
    (Printf.fprintf oc "%s\n" (Pp.string_of_ppcmds !pp);
     flush oc;
     close_out oc;
     Tacticals.tclIDTAC)
