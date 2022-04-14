
module Make = functor (C : Utils.ConstrLike) -> struct

  include Morphisms.Make(C)

  (* The composed morphism of the path may not be in the context since we only keep the base *)
  type path =
    { mph  : morphism
    ; path : mph_id list
    }
  type face =
    { category : int
    ; src      : elem
    ; dst      : elem
    ; side1    : path
    ; side2    : path
    ; obj      : constr
    }
  type t =
    { categories : category array
    ; elems      : elem array
    ; morphisms  : morphism array
    ; faces      : face array
    }

  exception Unimplemented

  let empty_context =
    { categories = [| |]
    ; elems      = [| |]
    ; morphisms  = [| |]
    ; faces      = [| |]
    }
  let get_cat  = raise Unimplemented
  let get_elem = raise Unimplemented
  let get_mph  = raise Unimplemented
  let get_face = raise Unimplemented

  let parse_cat  = raise Unimplemented
  let parse_elem = raise Unimplemented
  let parse_mph  = raise Unimplemented
  let parse_face = raise Unimplemented
end
