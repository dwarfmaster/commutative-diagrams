
type path = Hyps.elem * Hyps.morphism list
type cell =
  { mutable parent : int
  ; mutable rank   : int
  ; path           : path
  ; mutable eq     : Hyps.eq
  }
module OrderedPaths = struct
  type t = path
  let compareMphs = fun (m1 : Hyps.morphism) (m2 : Hyps.morphism) -> m1.id - m2.id
  let compare = fun (p1 : path) (p2 : path) -> List.compare compareMphs (snd p1) (snd p2)
end
module M = struct
  include Map.Make(OrderedPaths)
  let of_list : (path * 'a) list -> 'a t = fun l -> of_seq (List.to_seq l)
end
type t =
  { cells : cell array
  ; map : int M.t
  }

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

let initCell = fun i (path : path) ->
  let* eq = Hyps.refl @<< (Hyps.realize (fst path) (Hyps.extract (snd path))) in
  ret { parent = i
      ; rank   = 0
      ; path   = path
      ; eq     = eq
      }
let extract = fun (p : Hyps.path) -> (p.mph.tp.src, p.path)
let init = fun paths ->
  let* cells = Array.of_list <$> mapiM initCell paths in
  ret { cells = cells
      ; map   = M.of_list (List.mapi (fun i p -> (p,i)) paths)}

let rec find = fun id store ->
  if id = store.(id).parent
  then ret (id,store.(id).eq)
  else
    let* pid,peq = find store.(id).parent store in
    store.(id).parent <- pid;
    let* eq = Hyps.concat store.(id).eq peq in
    store.(id).eq <- eq;
    ret (pid,eq)
let query = fun p store ->
  let* (id,eq) = find (M.find p store.map) store.cells in
  ret (store.cells.(id).path,eq)
let query_conn = fun p1 p2 store ->
  let* (id1,eq1) = find (M.find p1 store.map) store.cells in
  let* (id2,eq2) = find (M.find p2 store.map) store.cells in
  if id1 = id2
  then (fun x -> Some x) <$> Hyps.concat eq1 @<< Hyps.inv eq2
  else ret None

let union = fun id1 id2 eq store ->
  let* parent1,_ = find id1 store in
  let* parent2,_ = find id2 store in
  if parent1 = parent2 then ret ()
  else if store.(parent1).rank < store.(parent2).rank
  then begin
    let* eq = Hyps.inv eq in
    store.(parent2).parent <- parent1;
    store.(parent2).eq     <- eq;
    store.(parent1).rank   <- store.(parent1).rank + store.(parent2).rank;
    ret ()
  end else begin
    store.(parent1).parent <- parent2;
    store.(parent1).eq     <- eq;
    store.(parent2).rank   <- store.(parent1).rank + store.(parent2).rank;
    ret ()
  end

let connect = fun p1 p2 eq store ->
  union (M.find p1 store.map) (M.find p2 store.map) eq store.cells
