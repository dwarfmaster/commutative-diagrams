
type path = Data.elem * Data.morphism list
type cell =
  { mutable parent : int
  ; mutable rank   : int
  ; path           : path
  ; mutable eq     : Data.eq
  }
module OrderedPaths = struct
  type t = path
  let compareMphs = fun (m1 : Data.morphism) (m2 : Data.morphism) -> m1.id - m2.id
  let compare = fun (p1 : path) (p2 : path) ->
    if (fst p1).id = (fst p2).id
    then List.compare compareMphs (snd p1) (snd p2)
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
      ; rank   = 1
      ; path   = path
      ; eq     = eq
      }
let extract = fun (p : Data.path) -> (p.mph.tp.src, p.path)
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

let connect = fun p1 p2 eq store ->
  union (M.find p1 store.map) (M.find p2 store.map) eq store.cells

let rec print_path = fun (p : path) ->
  let (++) = Pp.(++) in
  match snd p with
  | [] ->
    let* env = Proofview.tclENV in
    let* sigma = Proofview.tclEVARMAP in
    ret (Pp.str "id_(" ++ Printer.pr_econstr_env env sigma (fst p).obj ++ Pp.str ")")
  | m :: [] ->
    let* env = Proofview.tclENV in
    let* sigma = Proofview.tclEVARMAP in
    ret (Printer.pr_econstr_env env sigma m.data.obj)
  | m :: ms ->
    let* mph = print_path (m.data.tp.dst,ms) in
    let* env = Proofview.tclENV in
    let* sigma = Proofview.tclEVARMAP in
    ret (Printer.pr_econstr_env env sigma m.data.obj ++ Pp.str ">" ++ mph)
