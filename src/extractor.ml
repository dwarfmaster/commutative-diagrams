
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
  -> (Hyps.t * (Data.path*Data.path) option) Proofview.tactic = fun goal ->
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

let hole : Environ.env -> EConstr.t -> EConstr.t Proofview.tactic = fun env tp ->
  let* sigma = Proofview.tclEVARMAP in
  let (sigma,hole) = Evarutil.new_evar ~principal:true env sigma tp in
  let* _ = Proofview.Unsafe.tclEVARS sigma in
  ret hole

let eqT : Data.path -> Data.path -> EConstr.t Proofview.tactic = fun side1 side2 ->
  let* side1 = Hyps.rpath side1 in
  let* side2 = Hyps.rpath side2 in
  Hyps.eqT side1 side2

let eqHole : Environ.env -> Data.path -> Data.path -> Data.eq Proofview.tactic = fun env side1 side2 ->
  let* pth1 = Hyps.rpath side1 in
  let* pth2 = Hyps.rpath side2 in
  let* tp = Hyps.eqT pth1 pth2 in
  let* hl = hole env tp in
  let hl = Hyps.atom_eq hl in
  ret { Data.src = pth1
      ; dst      = pth2
      ; tp       = side1.mph.tp
      ; eq       = hl
      }

let normalize' : Proofview.Goal.t -> unit Proofview.tactic = fun goal ->
  let* (store,obj) = extract_hyps goal in
  match obj with
  | None -> Tacticals.tclFAIL 0 (Pp.str "Goal is not a face")
  | Some (side1,side2) ->
    let env = Proofview.Goal.env goal in
    let* side1 = Commutation.normalize_iso_in_path side1 in
    let* side2 = Commutation.normalize_iso_in_path side2 in
    let* eq2 = Hyps.inv side2.eq in
    let* ngl = eqHole env side1 side2 in
    let* ngl = Hyps.concat side1.eq ngl in
    let* ngl = Hyps.concat ngl eq2 in
    let* ngl = Hott.real_eq (Hyps.simpl_eq ngl) in
    let* ngl = add_universes_constraints env ngl in
    Refine.refine ~typecheck:false (fun sigma -> (sigma, ngl))
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
      let* eq = Hott.real_eq (Hyps.simpl_eq eq) in
      let* eq = add_universes_constraints env eq in
      Refine.refine ~typecheck:false (fun sigma -> (sigma, eq))
let solve : int -> unit Proofview.tactic = fun level -> Proofview.Goal.enter_one (solve' level)

let pretype env sigma (t : EConstr.t) : EConstr.t =
  let (_,tp) = Typing.type_of env sigma t in tp
let rec inspect_kind env sigma (t : EConstr.t) : Pp.t =
  match EConstr.kind sigma t with 
  | Rel i -> Pp.str "rel<" ++ Pp.int i ++ Pp.str ">"
  | Var n -> Names.Id.print n
  | Const (n,_) -> Pp.str "const(" ++ Names.Constant.print n ++ Pp.str ")"
  | App (f, args) -> inspect_kind env sigma f ++ Pp.str "("
                  ++ Pp.prvect_with_sep (fun _ -> Pp.str ", ") (inspect_kind env sigma) args
                  ++ Pp.str ")"
  | Proj (p,ec) ->
      let tp = pretype env sigma ec in
      inspect_kind env sigma ec ++ Pp.str ":" ++ inspect_kind env sigma tp  
                                ++ Pp.str "->" ++ Names.Projection.print p
  | Evar _ -> Pp.str "<?E>"
  | Sort _ -> Pp.str "<?s>"
  | Cast _ -> Pp.str "<?C>"
  | Ind ((n,_),_) -> Pp.str "ind(" ++ Names.MutInd.print n ++ Pp.str ")"
  | _ -> Pp.str "<?>"

let debug_print' : string -> Proofview.Goal.t -> unit Proofview.tactic = fun name goal ->
  let* sigma = Proofview.tclEVARMAP in
  let env = Proofview.Goal.env goal in
  let hyps = Proofview.Goal.hyps goal in
  let with_name name hyp =
    let target = Names.Id.of_string name in
    match hyp with
    | Context.Named.Declaration.LocalAssum (name,_) -> None
    | Context.Named.Declaration.LocalDef (name,def,_) ->
        if Names.Id.equal name.binder_name target then Some def else None in
  let hyp = List.find_map (with_name name) hyps in
  match hyp with
  | None -> Feedback.msg_warning (Pp.str "No hypothesis found : " ++ Pp.str name); Proofview.tclUNIT ()
  | Some def -> Feedback.msg_notice (inspect_kind env sigma def); Proofview.tclUNIT ()
let debug_print (name : string) : unit Proofview.tactic = Proofview.Goal.enter_one (debug_print' name)
