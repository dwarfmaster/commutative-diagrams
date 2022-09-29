
module Make(PA : Pa.ProofAssistant) = struct
  open Data
  module St = Store.Make(PA.M)
  open St.Combinators
  type 'a m = ('a,PA.t) St.t

  module MphMap = Map.Make(EqMph(PA))
  type idxMap = int MphMap.t
  type enumeration =
    { paths : PA.t morphism array
    ; indices : idxMap
    }

  type partialEnumeration =
    { categories : PA.t category list array
    ; functors : PA.t funct list array
    ; elems : PA.t elem list array
    ; morphisms : PA.t morphism list array
    }

  let init_partial size : partialEnumeration m =
    assert (size >= 0);
    let init : partialEnumeration =
      { categories = Array.make (size+1) []
      ; functors   = Array.make (size+1) []
      ; elems      = Array.make (size+1) []
      ; morphisms  = Array.make (size+1) []
      } in
    let* cats = St.getCategories () in
    let* functs = St.getFunctors () in 
    let* elems = St.getElems () in 
    let* mphs = St.getMorphisms () in
    Array.iter (fun c -> init.categories.(0) <- AtomicCategory c :: init.categories.(0)) cats;
    Array.iter (fun f -> init.functors.(0) <- AtomicFunctor f :: init.functors.(0)) functs;
    Array.iter (fun e -> init.elems.(0) <- AtomicElem e :: init.elems.(0)) elems;
    Array.iter (fun m -> init.morphisms.(0) <- AtomicMorphism m :: init.morphisms.(0)) mphs;
    Array.iter (fun e -> init.morphisms.(0) <- Identity (AtomicElem e) :: init.morphisms.(0)) elems;
    ret init

  let forL (lst : 'a list) (f : 'a -> unit) : unit = List.iter f lst

  (* Update enum by side-effect *)
  let step size enum : unit =
    assert (size >= 1);
    (* FObj *)
    for id = 0 to size - 1 do
      forL enum.functors.(id)
        (fun f -> forL enum.elems.(size - 1 - id)
          (fun e -> enum.elems.(size) <- FObj (f,e) :: enum.elems.(size)))
    done;
    (* Identity *)
    forL enum.elems.(size)
      (fun e -> enum.morphisms.(size) <- Identity e :: enum.morphisms.(size));
    (* Comp *)
    for id = 0 to size - 1 do 
      forL enum.morphisms.(id)
        (fun m1 -> forL enum.morphisms.(size - 1 - id)
          (fun m2 -> enum.morphisms.(size) <- Comp (m1,m2) :: enum.morphisms.(size)))
    done;
    (* We don't want to include Inv here *)
    (* FMph *)
    for id = 0 to size - 1 do
      forL enum.functors.(id)
        (fun f -> forL enum.morphisms.(size - 1 - id)
          (fun m -> enum.morphisms.(size) <- FMph (f,m) :: enum.morphisms.(size)))
    done;
    ()

  let construct_from_partial enum : enumeration =
    let paths = enum.morphisms |> Array.to_list |> List.flatten in
    let ids, _ = List.fold_left (fun (acc,id) m -> (m,id) :: acc, id+1) ([],0) paths in
    { paths = paths |> Array.of_list
    ; indices = ids |> List.to_seq |> MphMap.of_seq }

  let enumerate_paths ~size =
    let* enum = init_partial size in
    for sz = 1 to size do
      step sz enum
    done;
    ret (construct_from_partial enum)
end
