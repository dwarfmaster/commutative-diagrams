
let (++) = Pp.(++)

(*  _____                 _                _ *)
(* |_   _|__  _ __       | | _____   _____| | *)
(*   | |/ _ \| '_ \ _____| |/ _ \ \ / / _ \ | *)
(*   | | (_) | |_) |_____| |  __/\ V /  __/ | *)
(*   |_|\___/| .__/      |_|\___| \_/ \___|_| *)
(*           |_| *)
(* Top-level *)
module Sv = Server
type builder = Graphbuilder.t
open Hyps.Combinators

let runWithGoal goal act =
  let env = Proofview.Goal.env goal in
  run env act

let global_ns = 0

let extract_hyp (env : Environ.env)
                (dec : EConstr.named_declaration) 
                (bld : builder) 
              : (builder * Lemmas.t option) Hyps.t =
  let* sigma = evars () in
  let name,tp = match dec with
    | Context.Named.Declaration.LocalAssum (name,tp) -> (name.binder_name, tp)
    | Context.Named.Declaration.LocalDef (name,_,tp) -> (name.binder_name, tp) in
  let ec = EConstr.mkVar name in
  let* is_cat = Query.query global_ns env Features.Tag.Category ec tp in
  let* is_obj = Query.query global_ns env Features.Tag.Object ec tp in
  let* is_mph = Query.query global_ns env Features.Tag.Morphism ec tp in
  let* is_funct = Query.query global_ns env Features.Tag.Functor ec tp in
  let* is_eq = Query.query global_ns env Features.Tag.Equality ec tp in
  let relevant = List.exists Option.has_some [ is_cat; is_obj; is_mph; is_funct; is_eq ] in
  let* bld =
    if relevant 
    then
      let name = Names.Id.print name |> Pp.string_of_ppcmds in
      let* obj = Hyps.registerObj global_ns ec tp (Some name) in 
      let* tptp = Query.get_type global_ns env sigma tp in
      let* tpobj = Hyps.registerObj global_ns tp tptp None in
      Graphbuilder.import obj.id tpobj.id bld
    else ret bld in
  let* lem = Lemmas.extractFromVar name tp |> Hyps.withMask true in
  ret (bld, lem)

let name : string -> Names.Name.t = fun s -> Names.Name.mk_name (Names.Id.of_string s)

let scanM (pred : 'a -> 'b -> ('b * 'c option) Hyps.t) 
          (l : 'a list) 
          (acc : 'b) 
        : ('b * 'c list) Hyps.t =
  let rec with_acc acc lacc lst =
    match lst with
    | [] -> ret (acc,lacc)
    | x :: xs ->
        let* (acc,r) = pred x acc in
        match r with
        | None -> with_acc acc lacc xs
        | Some y -> with_acc acc (y :: lacc) xs in
  let* (acc,r) = with_acc acc [] l in
  ret (acc,List.rev r)

let extract_hyps (goal : Proofview.Goal.t) 
               : (Lemmas.t list * Graph.graph * EConstr.t) option Hyps.t =
  let* env = env () in
  let context = Proofview.Goal.hyps goal in
  let concl = Proofview.Goal.concl goal in
  let* goal = Build.mk_evar env concl in
  let* is_eq = Query.query global_ns env Features.Tag.Equality goal concl in
  match is_eq with
  | Some _ ->
      let* sigma = evars () in
      let* tptp = Query.get_type global_ns env sigma concl in
      let* tpobj = Hyps.registerObj global_ns concl tptp None in
      let* obj = Hyps.registerObj global_ns goal concl (Some "Goal") in
      let bld = Graphbuilder.empty () in
      let* (bld,lemmas) = scanM (extract_hyp env) context bld in
      let* bld = Graphbuilder.import obj.id tpobj.id bld in
      bld |> Graphbuilder.build |> Option.map (fun gr -> (lemmas, gr, goal)) |> ret
  | None -> none ()

let server' (file: string option) (force: bool) (goal : Proofview.Goal.t) : unit Hyps.t =
  let* obj = extract_hyps goal in
  match obj with
  | None -> fail "Goal is not a face"
  | Some (lemmas,graph,goal_term) ->
      let* globalLemmas = Lemmas.extractAllConstants () |> Hyps.withMask true in
      let lemmas = List.append lemmas globalLemmas in
      let* () = Hyps.mapState (fun sigma -> (), Evd.push_shelf sigma) in
      let* _ = Sv.run ~file:file ~force:force graph lemmas in
      let* shelf = Hyps.mapState Evd.pop_shelf in
      let* handled = Hyps.handled () in
      let* () = Hyps.mapState (fun sigma -> (), Evd.unshelve sigma handled) in
      let* sigma = evars () in
      let* env = env () in
      let* () = Refine.refine ~typecheck:false 
                  (fun _ -> Typing.solve_evars env sigma goal_term)
                |> lift in
      let* () = lift Refine.solve_constraints in
      ret ()
let server file ~force : unit Proofview.tactic =
  Proofview.Goal.enter_one (fun goal -> server' file force goal |> runWithGoal goal)

