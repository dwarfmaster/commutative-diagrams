
module Make(PA : Pa.ProofAssistant) = struct
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

  module RElem = Registerer(Data.EqElem(PA))
  module RMph = Registerer(Data.EqMph(PA))
  module REq = Registerer(Data.EqEq(PA))
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

  let add_node (obj: PA.t Data.elem) (builder: t) =
    let i, nelems = RElem.may_insert obj builder.elems in
    i, { builder with elems = nelems }
  let add_edge (mph: PA.t Data.morphism) (builder: t) =
    let _, builder = add_node (Data.morphism_src mph) builder in
    let _, builder = add_node (Data.morphism_dst mph) builder in
    let _, nmphs = RMph.may_insert mph builder.mphs in
    { builder with mphs = nmphs }
  let add_face (eq: PA.t Data.eq) (builder: t) =
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
    let module St = Hyps.Make(PA.M) in
    let open St.Combinators in
    let* builder =
      Array.fold_left
        (fun bld elm -> add_node (AtomicElem elm) bld |> snd)
        builder
        <$> St.getElems () in
    let* builder =
      Array.fold_left
        (fun bld mph -> add_edge (AtomicMorphism mph) bld)
        builder
        <$> St.getMorphisms () in
    let* builder =
      Array.fold_left
        (fun bld eq -> add_face (AtomicEq eq) bld)
        builder
        <$> St.getEqs () in
    ret builder

  let build (builder: t) =
    let nodes = RElem.to_array builder.elems in
    let mphs = Array.make (Array.length nodes) [] in
    List.iter
      (fun mph ->
        let src_id = RElem.find (Data.morphism_src mph) builder.elems in
        let dst_id = RElem.find (Data.morphism_dst mph) builder.elems in
        mphs.(src_id) <- (dst_id,mph) :: mphs.(src_id))
      builder.mphs.values;
    (* TODO faces *)
    let open Graph.Make(PA) in
    { gr_nodes = nodes
    ; gr_edges = mphs
    ; gr_faces = []
    }
end
