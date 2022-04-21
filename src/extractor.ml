
let (++) = Pp.(++)
let (let*) = Proofview.tclBIND


(*  _____                 _                _ *)
(* |_   _|__  _ __       | | _____   _____| | *)
(*   | |/ _ \| '_ \ _____| |/ _ \ \ / / _ \ | *)
(*   | | (_) | |_) |_____| |  __/\ V /  __/ | *)
(*   |_|\___/| .__/      |_|\___| \_/ \___|_| *)
(*           |_| *)
(* Top-level *)

let print_hyp : Evd.evar_map -> Environ.env -> Hyps.t -> EConstr.named_declaration -> Hyps.t * Pp.t =
  fun sigma env store dec ->
  let name,tp = match dec with
    | Context.Named.Declaration.LocalAssum (name,tp) -> (name.binder_name, tp)
    | Context.Named.Declaration.LocalDef (name,_,tp) -> (name.binder_name, tp) in
  let ppconstr = Printer.pr_econstr_env env sigma in
  let (store,is_cat) = Hyps.parse_cat  sigma name tp store in
  let (store,is_elm) = Hyps.parse_elem sigma name tp store in
  let (store,is_mph) = Hyps.parse_mph  sigma name tp store in
  let (store,is_fce) = Hyps.parse_face sigma name tp store in
  let cat = match is_cat with | Some _ -> Pp.str "category " | None -> Pp.str "" in
  let elm = match is_elm with
    | None -> Pp.str ""
    | Some id -> Pp.str "object(" ++ ppconstr (store.elems.(id).category.obj) ++ Pp.str ") " in
  let mph = match is_mph with
    | None -> Pp.str ""
    | Some id ->
      let mph = store.morphisms.(id) in
      Pp.str "morphism(" ++ ppconstr mph.data.tp.category.obj ++ Pp.str ";"
      ++ ppconstr mph.data.tp.src.obj ++ Pp.str " -> "
      ++ ppconstr mph.data.tp.dst.obj ++ Pp.str ") " in
  let fce = match is_fce with
    | None -> Pp.str ""
    | Some id ->
      let fce = store.faces.(id) in
      Pp.str "face(" ++ ppconstr fce.tp.category.obj ++ Pp.str ";"
      ++ ppconstr fce.tp.src.obj ++ Pp.str " -> " ++ ppconstr fce.tp.dst.obj ++ Pp.str ")" in
  (store,
   Names.Id.print name ++ Pp.str " : " ++ ppconstr tp
   ++ Pp.str " [ " ++ cat ++ elm ++ mph ++ fce ++ Pp.str "]")

let extract_goal : Pp.t ref -> Proofview.Goal.t -> unit Proofview.tactic = fun pp goal ->
  let store = Hyps.empty_context in
  let* sigma = Proofview.tclEVARMAP in
  let* env   = Proofview.tclENV in
  let ppconstr = Printer.pr_econstr_env env sigma in
  let context = Proofview.Goal.hyps goal in
  let (store,ctx) = List.fold_left_map (print_hyp sigma env) store context in
  let ppconcl = Pp.str "Focusing goal" ++ ppconstr (Proofview.Goal.concl goal) ++ Pp.fnl () in
  pp := !pp ++ ppconcl ++ Pp.pr_vertical_list (fun h -> h) ctx;
  Proofview.tclUNIT ()

let extract : string -> unit Proofview.tactic = fun path ->
  let oc = open_out path in
  let* num = Proofview.numgoals in
  let pp = ref (Pp.str "Goals: " ++ Pp.int num ++ Pp.fnl ()) in
  let* _ = Proofview.Goal.enter (extract_goal pp) in
  (* TODO: doesn't work *)
  (* Pp.pp_with (Stdlib.Format.formatter_of_out_channel oc) (Pp.strbrk "test"); *)
  Printf.fprintf oc "%s\n" (Pp.string_of_ppcmds !pp);
  flush oc;
  close_out oc;
  Proofview.tclEXTEND [] Tacticals.tclIDTAC []
