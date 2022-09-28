
let (++) = Pp.(++)

(*  _____                 _                _ *)
(* |_   _|__  _ __       | | _____   _____| | *)
(*   | |/ _ \| '_ \ _____| |/ _ \ \ / / _ \ | *)
(*   | | (_) | |_) |_____| |  __/\ V /  __/ | *)
(*   |_|\___/| .__/      |_|\___| \_/ \___|_| *)
(*           |_| *)
(* Top-level *)
module St = Store.Make(Hott.M)
type 'a m = ('a,Hott.t) St.t
open St.Combinators
let (let$) = Proofview.tclBIND
let evars () = lift (Hott.M.lift Proofview.tclEVARMAP)
let env () = lift (Hott.M.env ())
let fail msg = lift (Hott.M.lift (Tacticals.tclFAIL 0 (Pp.str msg)))
let print str = Feedback.msg_info (Pp.str str)
let runWithGoal goal act =
  let env = Proofview.Goal.env goal in
  Hott.M.run env (run act)

let extract_hyp (env : Environ.env) (dec : EConstr.named_declaration) : unit m =
  let* sigma = evars () in
  let name,tp = match dec with
    | Context.Named.Declaration.LocalAssum (name,tp) -> (name.binder_name, tp)
    | Context.Named.Declaration.LocalDef (name,_,tp) -> (name.binder_name, tp) in
  let* _ = Hott.parseCategory (EConstr.mkVar name) tp in
  let* _ = Hott.parseFunctor  (EConstr.mkVar name) tp in
  let* _ = Hott.parseElem     (EConstr.mkVar name) tp in
  let* _ = Hott.parseMorphism (EConstr.mkVar name) tp in
  let* _ = Hott.parseEq       (EConstr.mkVar name) tp in
  ret ()

let name : string -> Names.Name.t = fun s -> Names.Name.mk_name (Names.Id.of_string s)

let rec forM (pred : 'a -> unit m) : 'a list -> unit m =
  function
    | [ ] -> ret ()
    | x :: xs -> let* _ = pred x in forM pred xs

let add_universes_constraints (env : Environ.env) (c : Hott.t) : Hott.t Proofview.tactic =
  let$ sigma = Proofview.tclEVARMAP in
  let (sigma,_) = Typing.solve_evars env sigma c in
  Proofview.tclTHEN
    (Proofview.Unsafe.tclEVARS sigma)
    (Proofview.tclUNIT c)

let extract_hyps (goal : Proofview.Goal.t) : (Hott.t Data.morphism * Hott.t Data.morphism) option m =
  let env = Proofview.Goal.env goal in
  let context = Proofview.Goal.hyps goal in
  let goal = Proofview.Goal.concl goal in
  let* store = forM (extract_hyp env) context in
  Hott.parseEqGoal goal

let extract' (path : string) (goal : Proofview.Goal.t) : unit m =
  let* _ = St.registerEqPredicate Hott.eq in
  let* _ = extract_hyps goal in
  let* sigma = evars () in
  let env = Proofview.Goal.env goal in
  let* pp = Renderer.to_graphviz sigma env in
  let oc = open_out path in
  Printf.fprintf oc "%s\n" (Pp.string_of_ppcmds pp);
  flush oc;
  close_out oc;
  ret ()
let extract (path : string) : unit Proofview.tactic =
  Proofview.Goal.enter_one (fun goal -> extract' path goal |> runWithGoal goal)

let normalize' (goal : Proofview.Goal.t) : unit m =
  let* _ = St.registerEqPredicate Hott.eq in
  let* obj = extract_hyps goal in
  match obj with
  | None -> fail "Goal is not a face"
  | Some (side1,side2) ->
      assert false
    (* let env = Proofview.Goal.env goal in *)
    (* let* side1 = Commutation.normalize_iso_in_path side1 in *)
    (* let* side2 = Commutation.normalize_iso_in_path side2 in *)
    (* let* eq2 = Hyps.inv side2.eq in *)
    (* let* ngl = eqHole env side1 side2 in *)
    (* let* ngl = Hyps.concat side1.eq ngl in *)
    (* let* ngl = Hyps.concat ngl eq2 in *)
    (* let* ngl = Hott.realizeEq (Hyps.simpl_eq ngl) in *)
    (* let* ngl = add_universes_constraints env ngl in *)
    (* Refine.refine ~typecheck:false (fun sigma -> (sigma, ngl)) *)
let normalize (_ : unit) : unit Proofview.tactic =
  Proofview.Goal.enter_one (fun goal -> normalize' goal |> runWithGoal goal)

let solve' (level : int) (goal : Proofview.Goal.t) : unit m =
  let* _ = St.registerEqPredicate Hott.eq in
  let* obj = extract_hyps goal in
  match obj with
  | None -> fail "Goal is not a face"
  | Some (side1,side2) ->
      assert false
    (* let* commuter = Commutation.build store level in *)
    (* let* sol = Commutation.query side1 side2 commuter in *)
    (* match sol with *)
    (* | None -> Tacticals.tclFAIL 0 (Pp.str "Couldn't make goal commute") *)
    (* | Some eq -> *)
    (*   let env = Proofview.Goal.env goal in *)
    (*   let* eq = Hott.realizeEq (Hyps.simpl_eq eq) in *)
    (*   let* eq = add_universes_constraints env eq in *)
    (*   Refine.refine ~typecheck:false (fun sigma -> (sigma, eq)) *)
let solve (level : int) : unit Proofview.tactic =
  Proofview.Goal.enter_one (fun goal -> solve' level goal |> runWithGoal goal)
