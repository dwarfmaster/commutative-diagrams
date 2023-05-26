
type result =
  | Success of Msgpack.t
  | Failure of string
  | Terminate of Msgpack.t
type state =
  { goal: Graph.graph
  }

open Hyps.Combinators
open Msgpack
let success m = Success m |> ret
let failure m = Failure m |> ret
let terminate m = Terminate m |> ret

let goal st args =
  let* gr = Graph.Serde.pack st.goal in
  success gr

let info st args =
  match args with
  | [ Integer id ] ->
      let obj = { Hyps.namespace = 0; Hyps.id = id; } in
      (* Name *)
      let* name = Hyps.getObjName obj in
      let name = match name with
      | Some name -> String name
      | None -> Nil in
      (* Label *)
      let* env = env () in
      let* sigma = evars () in
      let* vl = Hyps.getObjValue obj in
      let label = Pp.(Printer.pr_econstr_env env sigma vl |> string_of_ppcmds) in
      (* Evars *)
      let evar = 1 in (* TODO set evar status *)
      success(Array [String label ; name; Integer evar])
  | _ -> failure "Wrong arguments for info"

let unify st args =
  let rec parse_pairs = function
    | [] -> Some []
    | Array [Integer o1; Integer o2] :: args ->
        let pairs = parse_pairs args in
        begin match pairs with
        | Some pairs -> Some ((o1,o2) :: pairs)
        | None -> None
        end
    | _ -> None in
  let* env = env () in
  let exception UnificationFailed in
  let unify_pair sigma (ec1,ec2) =
    try Unification.w_unify env sigma Reduction.CONV ec1 ec2 with
      _ -> raise UnificationFailed in
  let pairs = parse_pairs args in
  match pairs with
  | Some pairs -> begin try
      let* pairs = mapM (fun (o1,o2) ->
        let* ec1 = Hyps.getObjValue { Hyps.namespace = 0; Hyps.id = o1; } in
        let* ec2 = Hyps.getObjValue { Hyps.namespace = 0; Hyps.id = o2; } in
        ret (ec1,ec2)) pairs in
      let* sigma = evars () in
      let sigma = List.fold_left unify_pair sigma pairs in
      let* _ = Hyps.setState sigma in
      success (Boolean true)
  with UnificationFailed -> success (Boolean false) end
  | None -> failure "Wrong arguments to unify"

let equalify st args = assert false
let lemmas st args = assert false
let instantiate st args = assert false
let query st args = assert false
let build st args = assert false
let parse st args = assert false
let saveState st args = assert false
let restoreState st args = assert false
