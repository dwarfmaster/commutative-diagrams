
type skelComponent = (Data.morphismData,Data.pathSkeleton) Data.pathComponent
type component = (Data.morphism,int) Data.pathComponent
type path = Data.elem * component list
type queryComponent = (Data.morphism,query) Data.pathComponent
and query = Data.elem * queryComponent list
type cell =
  { mutable parent : int
  ; mutable rank   : int
  ; path           : path
  ; mutable eq     : Data.eq
  }
module OrderedPaths = struct
  type t = path
  let compareMphs = fun (m1 : Data.morphism) (m2 : Data.morphism) -> m1.id - m2.id
  let comparePathComps = fun (p1 : component) (p2 : component) ->
    match p1, p2 with
    | Base m1, Base m2 -> compareMphs m1 m2
  let compare = fun (p1 : path) (p2 : path) ->
    if (fst p1).id = (fst p2).id
    then CList.compare comparePathComps (snd p1) (snd p2)
    else (fst p1).id - (fst p2).id
end
module M = struct
  include Map.Make(OrderedPaths)
  let of_list : (path * 'a) list -> 'a t = fun l -> of_seq (List.to_seq l)
end
type t =
  { cells : cell array
  ; map : int M.t
  }

let rec mkQuery (path : Data.path) : query =
  (path.mph.tp.src, List.map mkQueryComp path.path)
and mkQueryComp comp : queryComponent =
  match comp with
  | Base m -> Base m

let (let*) = Proofview.tclBIND
let ret = Proofview.tclUNIT
let (<$>) : ('a -> 'b) -> 'a Proofview.tactic -> 'b Proofview.tactic = fun f m ->
  Proofview.tclBIND m (fun x -> ret (f x))
let (@<<) : ('a -> 'b Proofview.tactic) -> 'a Proofview.tactic -> 'b Proofview.tactic =
  fun f x -> Proofview.tclBIND x f

let rec sequenceM : 'a Proofview.tactic list -> 'a list Proofview.tactic = function
  | [] -> ret []
  | m :: ms ->
    let* x = m in
    let* xs = sequenceM ms in
    ret (x :: xs)
let mapiM : (int -> 'a -> 'b Proofview.tactic) -> 'a list -> 'b list Proofview.tactic =
  fun f arr -> sequenceM (List.mapi f arr)

(* Whenever a path appear in the component of another, it is listed first in the result *)
let rec closePaths (acc : query list) (paths : query list) : query list =
  List.fold_left closePath acc paths
and closePath (acc : query list) (path : query) : query list =
  closePathComponents (path :: acc) (snd path)
and closePathComponents (acc : query list) (comps : queryComponent list) : query list =
  List.fold_left closePathComponent acc comps
and closePathComponent (acc : query list) (comp : queryComponent) : query list =
  match comp with
  | Base _ -> acc

let rec extractPath (ids : int M.t) (path : query) : path =
  let accum = fun comps comp -> extractPathComponent ids comp :: comps in
  let comps = List.fold_left accum [] (snd path) in
  (fst path, List.rev comps)
and extractPathComponent (ids : int M.t) (comp : queryComponent) : component =
  match comp with
  | Base m -> Base m

let rec extractPaths (ids : int M.t) (id : int) (paths : query list)
                   : path list * int M.t = 
  match paths with
  | [] -> ([],ids)
  | path :: paths ->
      let path = extractPath ids path in
      let ids  = M.add path id ids in
      let (paths, ids) = extractPaths ids (id + 1) paths in
      (path :: paths, ids)

let rec toSkeleton (paths : path array) (path : path) : Data.pathSkeleton =
  (fst path, List.map (toSkeletonComp paths) (snd path))
and toSkeletonComp (paths : path array) (comp : component) : skelComponent =
  match comp with
  | Base m -> Base m.data
  | Functor (f,p) -> Functor (f,toSkeleton paths paths.(p))

let initCell (paths : path array) (i : int) (path : path) : cell Proofview.tactic =
  let* eq = Hyps.refl @<< (Hyps.realize (toSkeleton paths path)) in
  ret { parent = i
      ; rank   = 1
      ; path   = path
      ; eq     = eq
      }

let init = fun paths ->
  let (paths,ids) = extractPaths M.empty 0 (closePaths [] paths) in
  let paths_arr = Array.of_list paths in
  let* cells = Array.of_list <$> mapiM (initCell paths_arr) paths in
  ret { cells = cells
      ; map   = ids }

let rec find (id : int) store =
  if id = store.(id).parent
  then ret (id,store.(id).eq)
  else
    let* pid,peq = find store.(id).parent store in
    store.(id).parent <- pid;
    let* eq = Hyps.concat store.(id).eq peq in
    store.(id).eq <- eq;
    ret (pid,eq)
let query_conn (p1 : query) (p2 : query) (store : t) =
  let p1 = extractPath store.map p1 in 
  let p2 = extractPath store.map p2 in
  let* (id1,eq1) = find (M.find p1 store.map) store.cells in
  let* (id2,eq2) = find (M.find p2 store.map) store.cells in
  if id1 = id2
  then (fun x -> Some x) <$> Hyps.concat eq1 @<< Hyps.inv eq2
  else ret None

let conjugate : Data.eq -> Data.eq -> Data.eq -> Data.eq Proofview.tactic =
  fun eq1 eq eq2 ->
  let* eq1 = Hyps.inv eq1 in
  let* c = Hyps.concat eq1 eq in
  Hyps.concat c eq2

let union = fun id1 id2 eq store ->
  let* parent1,eq1 = find id1 store in
  let* parent2,eq2 = find id2 store in
  if parent1 = parent2 then ret false
  else if store.(parent1).rank < store.(parent2).rank
  then begin
    let* eq = Hyps.inv eq in
    let* eq = conjugate eq2 eq eq1 in
    store.(parent2).parent <- parent1;
    store.(parent2).eq     <- eq;
    store.(parent1).rank   <- store.(parent1).rank + store.(parent2).rank;
    ret true
  end else begin
    let* eq = conjugate eq1 eq eq2 in
    store.(parent1).parent <- parent2;
    store.(parent1).eq     <- eq;
    store.(parent2).rank   <- store.(parent1).rank + store.(parent2).rank;
    ret true
  end

let connect (p1 : query) (p2 : query) eq store =
  let p1 = extractPath store.map p1 in 
  let p2 = extractPath store.map p2 in
  union (M.find p1 store.map) (M.find p2 store.map) eq store.cells

(* let rec print_path = fun (p : path) -> *)
(*   let (++) = Pp.(++) in *)
(*   match snd p with *)
(*   | [] -> *)
(*     let* env = Proofview.tclENV in *)
(*     let* sigma = Proofview.tclEVARMAP in *)
(*     ret (Pp.str "id_(" ++ Printer.pr_econstr_env env sigma (fst p).obj ++ Pp.str ")") *)
(*   | m :: [] -> *)
(*     let* env = Proofview.tclENV in *)
(*     let* sigma = Proofview.tclEVARMAP in *)
(*     ret (Printer.pr_econstr_env env sigma m.data.obj) *)
(*   | m :: ms -> *)
(*     let* mph = print_path (m.data.tp.dst,ms) in *)
(*     let* env = Proofview.tclENV in *)
(*     let* sigma = Proofview.tclEVARMAP in *)
(*     ret (Printer.pr_econstr_env env sigma m.data.obj ++ Pp.str ">" ++ mph) *)
