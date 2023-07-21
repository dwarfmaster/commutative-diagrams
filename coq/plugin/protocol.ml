
type result =
  | Success of Msgpack.t
  | Failure of string
  | Terminate of bool * Msgpack.t
type state =
  { goal: Graph.graph
  ; lemmas: Lemmas.t array
  }

open Hyps.Combinators
open Msgpack
let success m = Success m |> ret
let failure m = Failure m |> ret
let terminate (b,m) = Terminate (b,m) |> ret

let pack_lem_id id = id + 1
let unpack_lem_id id = id - 1
let withNS st lem act =
  if lem = 0
  then act
  else 
    let ns = Lemmas.context st.lemmas.(unpack_lem_id lem) in
    Hyps.inNamespace ns act

let goal st args =
  let* gr = Graph.Serde.pack st.goal in
  success gr

let info st args =
  match args with
  | [ Integer lem; Integer id ] ->
      withNS st lem begin
        (* Name *)
        let* name = Hyps.getObjName id in
        let name = match name with
        | Some name -> String name
        | None -> Nil in
        (* Label *)
        let* env = env () in
        let* sigma = evars () in
        let* vl = Hyps.getObjValue id in
        let label = Pp.(Printer.pr_econstr_env env sigma vl |> string_of_ppcmds) in
        (* Evars *)
        let evar = if EConstr.isEvar sigma vl then 0 (* Evar *)
          else if EConstr.fold sigma (fun b ec -> b || EConstr.isEvar sigma ec) false vl
            then 2 (* Partial *)
            else 1 (* Grounded *) in
        success(Array [String label ; name; Integer evar])
      end
  | _ -> failure "Wrong arguments for info"

let repr st args =
  match args with
  | [ Integer lem; Integer id ] ->
      withNS st lem begin
        let* r = Hyps.getObjRepr id in
        success (Integer r)
      end
  | _ -> failure "Wrong arguments for repr"

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
  let unify_pair sigma (ec1,ec2) =
    Unification.w_unify env sigma Reduction.CONV ec1 ec2 in
  let pairs = parse_pairs args in
  match pairs with
  | Some pairs -> begin 
    try
      let* pairs = mapM (fun (o1,o2) ->
        let* ec1 = Hyps.getObjValue o1 in
        let* ec2 = Hyps.getObjValue o2 in
        ret (ec1,ec2)) pairs in
      let* sigma = evars () in
      Feedback.msg_info Pp.(str "Unifying");
      let sigma = List.fold_left unify_pair sigma pairs in
      Feedback.msg_info Pp.(str "Unification returned");
      if Evd.has_given_up sigma
      then begin
        Feedback.msg_info Pp.(str "Given up");
        success (Boolean false)
      end else begin
        Feedback.msg_info Pp.(str "Success");
        let* _ = Hyps.setState sigma in
        success (Boolean true)
      end
    with 
      e -> 
        Feedback.msg_info Pp.(str "Exception caught");
        success (Boolean false) 
  end
  | None -> failure "Wrong arguments to unify"

(* TODO equalify is broken *)
(* The conversion checker fails with Univ.repr: Universe ... undefined *)
let equalify st args =
  match args with
  | [ Integer o1; Integer o2 ] ->
      let* ec1 = Hyps.getObjValue o1 in
      let* ec2 = Hyps.getObjValue o2 in
      let* env = env () in
      let* sigma = evars () in
      Reductionops.is_conv env sigma ec1 ec2 |> (fun b -> Boolean b) |> success
  | _ -> failure "Wrong arguments to equalify"

let lemmas st args =
  let prep_lem id lem =
    Array [ Integer (pack_lem_id id)
          ; String (Lemmas.name lem)
          ; String (Lemmas.namespace lem)
          ] in
  let lms = List.mapi prep_lem (Array.to_list st.lemmas) in
  success (Array lms)

let instantiate st args =
  match args with
  | [ Integer lem ] ->
      let* graph = Lemmas.instantiate st.lemmas.(unpack_lem_id lem) in
      success @<< Graph.Serde.pack graph
  | _ -> failure "Wrong arguments to instantiate"

let pattern st args =
  match args with
  | [ Integer lem ] ->
      let lem = unpack_lem_id lem in
      let ns = Lemmas.context st.lemmas.(lem) in
      let* graph = Lemmas.instantiate st.lemmas.(lem)
                   |> Hyps.inNamespace ns in
      success @<< Graph.Serde.pack graph
  | _ -> failure "Wrong arguments to pattern"

let query st args =
  match args with
  | [ Integer lem; Integer id; String feat_str ] ->
      withNS st lem begin
        let feat = Features.Tag.parse feat_str in begin
          match feat with
          | Some feat ->
              let* env = env () in
              let* obj = Hyps.getObjValue id in
              let* tp = Hyps.getObjType id in
              let* result = Query.query env feat obj tp in
              begin match result with
              | Some (_,feat) ->
                  let args = Features.to_list feat |> List.map (fun x -> Integer x) in
                  let tag = feat |> Features.tag |> Features.Tag.to_string in
                  success (Array [ Array (String tag :: args) ])
              | None -> success Nil
              end
          | _ -> failure ("Unknown feature to query: " ^ feat_str)
        end
      end
  | _ -> failure "Wrong arguments to query"

let build st args =
  match args with
  | [ Integer lem; Array (String tag_str :: args) ] ->
      withNS st lem begin
        let tag = Features.Tag.parse tag_str in
        begin match tag with
        | Some tag ->
            let rec parse_args = function
              | [] -> Some []
              | Integer id :: args ->
                  parse_args args |> Option.map (fun args -> id :: args)
              | _ -> None in
            let feat = Option.bind (parse_args args) (Features.from_list tag) in
            begin match feat with
            | Some feat ->
                let* obj = Build.build feat in
                success (Integer obj)
            | None -> failure ("Wrong number of arguments when building " ^ tag_str)
            end
        | _ -> failure ("Unknown feature to build: " ^ tag_str)
        end
      end
  | _ -> failure "Wrong arguments to build"

let parse st args =
  match args with
  | [ String str ] -> begin try
      let ast = Pcoq.parse_string Pcoq.Constr.term str in
      let* env = env () in
      let* sigma = evars () in
      let (c, ctx) = Constrintern.interp_constr env sigma ast in
      let sigma = Evd.merge_universe_context sigma ctx in
      let* () = Hyps.setState sigma in
      let* tp = Query.get_type env sigma c in
      let* obj = Hyps.registerObj c tp None in
      success (Integer obj)
  with _ -> success Nil end
  | _ -> failure "Wrong arguments to parse"

let saveState st args =
  let* saved = Hyps.saveState () in
  success (Integer saved)
let restoreState st args =
  match args with
  | [ Integer saved ] -> 
      let* () = Hyps.restoreState saved in
      success Nil
  | _ -> failure "Wrong arguments to restoreState"

let finish st args =
  match args with
  | [ Boolean false ] -> terminate (false,Nil)
  | [ Boolean true ] ->
      let* sigma = evars () in
      let* _ = Proofview.Unsafe.tclEVARS sigma |> lift in
      terminate (true,Nil)
  | _ -> failure "Wrong arguments to finish"
