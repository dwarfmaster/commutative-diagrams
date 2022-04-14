
module Make : functor (C : Utils.ConstrLike) -> sig

  include module type of Morphisms.Make(C)

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

  val empty_context : t
  val get_cat  : constr -> t -> t * cat_id
  val get_elem : constr -> t -> t * elem_id
  val get_mph  : constr -> t -> t * mph_id
  val get_face : constr -> t -> t * face_id

  val parse_cat  : Evd.evar_map -> Environ.env -> kind -> t -> t * cat_id  option
  val parse_elem : Evd.evar_map -> Environ.env -> kind -> t -> t * elem_id option
  val parse_mph  : Evd.evar_map -> Environ.env -> kind -> t -> t * mph_id  option
  val parse_face : Evd.evar_map -> Environ.env -> kind -> t -> t * face_id option
end
