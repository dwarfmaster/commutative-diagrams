
let (++) = Pp.(++)

(*  _____                 _                _ *)
(* |_   _|__  _ __       | | _____   _____| | *)
(*   | |/ _ \| '_ \ _____| |/ _ \ \ / / _ \ | *)
(*   | | (_) | |_) |_____| |  __/\ V /  __/ | *)
(*   |_|\___/| .__/      |_|\___| \_/ \___|_| *)
(*           |_| *)
(* Top-level *)
module M = Map.Make(Data.EqMph)
module Sv = Server
type 'a m = 'a Hyps.t
open Hyps.Combinators
let runWithGoal goal act =
  let env = Proofview.Goal.env goal in
  run env act

let extract_hyp (env : Environ.env) (dec : EConstr.named_declaration) : Lemmas.t option m =
  let* sigma = evars () in
  let name,tp = match dec with
    | Context.Named.Declaration.LocalAssum (name,tp) -> (name.binder_name, tp)
    | Context.Named.Declaration.LocalDef (name,_,tp) -> (name.binder_name, tp) in
  let* is_hyp = Hott.parse (EConstr.mkVar name) tp in
  match is_hyp with
  | Some _ -> none ()
  | None -> 
      Lemmas.extractFromVar name tp |> Hyps.withMask true

let name : string -> Names.Name.t = fun s -> Names.Name.mk_name (Names.Id.of_string s)

let scanM (pred : 'a -> 'b option m) (l : 'a list) : 'b list m =
  let rec with_acc acc lst =
    match lst with
    | [] -> ret acc
    | x :: xs ->
        let* r = pred x in
        match r with
        | None -> with_acc acc xs
        | Some y -> with_acc (y :: acc) xs in
  let* r = with_acc [] l in
  ret (List.rev r)

let add_universes_constraints (env : Environ.env) (c : EConstr.t) (sigma : Evd.evar_map) : Evd.evar_map * EConstr.t =
  Typing.solve_evars env sigma c

let extract_hyps (goal : Proofview.Goal.t) 
               : (Lemmas.t list * Data.morphism * Data.morphism) option m =
  let env = Proofview.Goal.env goal in
  let context = Proofview.Goal.hyps goal in
  let goal = Proofview.Goal.concl goal in
  let* lemmas = scanM (extract_hyp env) context in
  let* tp = Hott.parseType goal in
  match tp with
  | Some (EqT (_,_,_,left,right)) -> some (lemmas,left,right)
  | _ -> none ()

let server' (file: string option) (force: bool) (goal : Proofview.Goal.t) : unit m =
  let* obj = extract_hyps goal in
  match obj with
  | None -> fail "Goal is not a face"
  | Some (lemmas,side1,side2) ->
      let* globalLemmas = Lemmas.extractAllConstants () |> Hyps.withMask true in
      let lemmas = List.append lemmas globalLemmas in
      let* _ = Sv.run (Sv.Graph (file, force, side1, side2, lemmas)) in
      ret ()
      (* let* eq = lift (Hott.realizeEq eq) in *)
      (* lift (Hott.M.lift (Refine.refine ~typecheck:false *)
      (*   (add_universes_constraints (Proofview.Goal.env goal) eq))) *)
let server file ~force : unit Proofview.tactic =
  Proofview.Goal.enter_one (fun goal -> server' file force goal |> runWithGoal goal)

let normalize' (goal : Proofview.Goal.t) : unit m =
  let* obj = extract_hyps goal in
  match obj with
  | None -> fail "Goal is not a face"
  | Some (_,side1,side2) ->
      let* _ = Sv.run (Sv.Normalize (side1, side2)) in
      ret ()
let normalize (_ : unit) : unit Proofview.tactic =
  Proofview.Goal.enter_one (fun goal -> normalize' goal |> runWithGoal goal)

let extract' (path : string) (goal : Proofview.Goal.t) : unit m =
  let* _ = extract_hyps goal in
  let* _ = Sv.run (Sv.Print path) in
  ret ()
let extract (path : string) : unit Proofview.tactic =
  Proofview.Goal.enter_one (fun goal -> extract' path goal |> runWithGoal goal)

let solve' (level : int) (goal: Proofview.Goal.t) : unit m =
  let* obj = extract_hyps goal in
  match obj with
  | None -> fail "Goal is not a face"
  | Some (_,side1,side2) ->
      let* _ = Sv.run (Sv.Solve (level,side1,side2)) in
      ret ()
let solve (level : int) : unit Proofview.tactic =
  Proofview.Goal.enter_one (fun goal -> solve' level goal |> runWithGoal goal)
