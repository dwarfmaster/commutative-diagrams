
module Registerer(Ord : Map.OrderedType) = struct
  module M = Map.Make(Ord)
  type t =
    { values: Ord.t list
    ; length: int
    ; reverse: int M.t
    }

  let empty () =
    { values = []
    ; length = 0
    ; reverse = M.empty
    }

  let to_array (reg: t) : Ord.t array =
    Array.of_list (List.rev reg.values)

  let insert (x : Ord.t) (reg : t) : int * t =
    let nreg =
      { values = x :: reg.values
      ; length = reg.length + 1
      ; reverse = M.add x reg.length reg.reverse
      } in
    reg.length, nreg

  let get (x : Ord.t) (reg: t) : int option =
    M.find_opt x reg.reverse

  let find (x : Ord.t) (reg: t) : int =
    M.find x reg.reverse

  let may_insert (x : Ord.t) (reg : t) : int * t =
    match get x reg with
    | Some i -> i, reg
    | None -> insert x reg
end

module RElem = Registerer(Data.EqElem)
module RMph = Registerer(Data.EqMph)
module REq = Registerer(Data.EqEq)
type t =
  { elems : RElem.t
  ; mphs : RMph.t
  ; eqs : REq.t
  }

let empty () =
  { elems = RElem.empty ()
  ; mphs = RMph.empty ()
  ; eqs = REq.empty ()
  }

let add_node (obj: Data.elem) (builder: t) =
  let i, nelems = RElem.may_insert obj builder.elems in
  i, { builder with elems = nelems }
let add_edge (mph: Data.morphism) (builder: t) =
  let _, builder = add_node (Data.morphism_src mph) builder in
  let _, builder = add_node (Data.morphism_dst mph) builder in
  let _, nmphs = RMph.may_insert mph builder.mphs in
  { builder with mphs = nmphs }
let add_face (eq: Data.eq) (builder: t) =
  let rec add_comp mph builder =
    match mph with
    | Data.Comp (m1, m2) ->
        let builder = add_edge m1 builder in
        add_comp m2 builder
    | _ -> add_edge mph builder in
  let builder = add_comp (Data.eq_left eq) builder in
  let builder = add_comp (Data.eq_right eq) builder in
  let _, neqs = REq.may_insert eq builder.eqs in
  { builder with eqs = neqs }

let import_hyps (builder: t) =
  let open Hyps.Combinators in
  let* builder =
    Array.fold_left
      (fun bld (elm,mask) -> if mask then bld else add_node (AtomicElem elm) bld |> snd)
      builder
      <$> Hyps.getElems () in
  let* builder =
    Array.fold_left
      (fun bld (mph,mask) -> if mask then bld else add_edge (AtomicMorphism mph) bld)
      builder
      <$> Hyps.getMorphisms () in
  let* builder =
    Array.fold_left
      (fun bld (eq,mask) -> if mask then bld else add_face (AtomicEq eq) bld)
      builder
      <$> Hyps.getEqs () in
  ret builder

let rec find_in_list (lst: (int * 'a) list) (x : 'a) (n: int) : int * int =
  match lst with
  | [] -> assert false
  | (i,y) :: l when x = y -> (i,n)
  | _ :: l -> find_in_list l x (n+1)

let rec build_path (mphs : (int * Data.morphism) list array) (src: int) (mph : Data.morphism) : int list =
  match mph with
  | Data.Comp (m1, m2) ->
      let dst,id = find_in_list mphs.(src) m1 0 in
      let path = build_path mphs dst m2 in
      id :: path
  | _ -> let _,id = find_in_list mphs.(src) mph 0 in [ id ]
let build_face (builder : t) mphs eq : Graph.faces =
  let src_id = RElem.find (Data.eq_src eq) builder.elems in
  let dst_id = RElem.find (Data.eq_dst eq) builder.elems in
  let left = build_path mphs src_id (Data.eq_left eq) in
  let right = build_path mphs src_id (Data.eq_right eq) in
  let open Graph in
  { face_src = src_id
  ; face_dst = dst_id
  ; face_left = left
  ; face_right = right
  ; face_eq = eq
  }

let build (builder: t) =
  let nodes = RElem.to_array builder.elems in
  let mphs = Array.make (Array.length nodes) [] in
  List.iter
    (fun mph ->
      let src_id = RElem.find (Data.morphism_src mph) builder.elems in
      let dst_id = RElem.find (Data.morphism_dst mph) builder.elems in
      mphs.(src_id) <- (dst_id,mph) :: mphs.(src_id))
    builder.mphs.values;
  let faces = List.map (build_face builder mphs) builder.eqs.values in
  let open Graph in
  { gr_nodes = nodes
  ; gr_edges = mphs
  ; gr_faces = faces
  }
