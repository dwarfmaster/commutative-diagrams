
type result =
  | Success of Msgpack.t
  | Failure of string
  | Terminate of Msgpack.t
type state =
  { goal: Graph.graph
  ; lemmas: Lemmas.t array
  }

open Hyps.Combinators
open Msgpack
let success m = Success m |> ret
let failure m = Failure m |> ret
let terminate m = Terminate m |> ret
let global id = { Hyps.namespace = 0; Hyps.id = id; }

let goal st args =
  let* gr = Graph.Serde.pack st.goal in
  success gr

let info st args =
  match args with
  | [ Integer id ] ->
      let obj = global id in
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
        let* ec1 = Hyps.getObjValue (global o1) in
        let* ec2 = Hyps.getObjValue (global o2) in
        ret (ec1,ec2)) pairs in
      let* sigma = evars () in
      let sigma = List.fold_left unify_pair sigma pairs in
      let* _ = Hyps.setState sigma in
      success (Boolean true)
  with UnificationFailed -> success (Boolean false) end
  | None -> failure "Wrong arguments to unify"

let equalify st args =
  match args with
  | [ Integer o1; Integer o2 ] ->
      let* ec1 = Hyps.getObjValue (global o1) in
      let* ec2 = Hyps.getObjValue (global o2) in
      let* env = env () in
      let* sigma = evars () in
      Reductionops.is_conv env sigma ec1 ec2 |> (fun b -> Boolean b) |> success
  | _ -> failure "Wrong arguments to equalify"

let lemmas st args =
  let prep_lem id lem =
    Array [ Integer id
          ; String (Lemmas.name lem)
          ; String (Lemmas.namespace lem)
          ] in
  let lms = List.mapi prep_lem (Array.to_list st.lemmas) in
  success (Array lms)

let instantiate st args =
  match args with
  | [ Integer lem ] ->
      let* graph = Lemmas.instantiate st.lemmas.(lem) in
      success @<< Graph.Serde.pack graph
  | _ -> failure "Wrong arguments to instantiate"

let query st args =
  match args with
  | [ Integer id; String feat_str ] ->
      let feat = Features.Tag.parse feat_str in begin
        match feat with
        | Some feat ->
            let id = global id in
            let* env = env () in
            let* obj = Hyps.getObjValue id in
            let* tp = Hyps.getObjType id in
            let* result = Query.query 0 env feat obj tp in
            begin match result with
            | Some (_,feat) ->
                let args = Features.to_list feat |> List.map (fun x -> Integer x.Hyps.id) in
                let tag = feat |> Features.tag |> Features.Tag.to_string in
                success (Array (String tag :: args))
            | None -> success Nil
            end
        | _ -> failure ("Unknown feature to query: " ^ feat_str)
      end
  | _ -> failure "Wrong arguments to query"

let build st args =
  match args with
  | String tag_str :: args ->
      let tag = Features.Tag.parse tag_str in
      begin match tag with
      | Some tag ->
          let rec parse_args = function
            | [] -> Some []
            | Integer id :: args ->
                parse_args args |> Option.map (fun args -> global id :: args)
            | _ -> None in
          let feat = Option.bind (parse_args args) (Features.from_list tag) in
          begin match feat with
          | Some feat ->
              let* obj = Build.build 0 feat in
              success (Integer obj.Hyps.id)
          | None -> failure ("Wrong number of arguments when building " ^ tag_str)
          end
      | _ -> failure ("Unknown feature to build: " ^ tag_str)
      end
  | _ -> failure "Wrong arguments to build"

let parse st args = assert false
let saveState st args = assert false
let restoreState st args = assert false
