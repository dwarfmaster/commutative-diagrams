
include Morphisms
type kind = (EConstr.t,EConstr.t,EConstr.ESorts.t,EConstr.EInstance.t) Constr.kind_of_term

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
  ; obj      : EConstr.t
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
let get_cat  = fun _ _ -> raise Unimplemented
let get_elem = fun _ _ -> raise Unimplemented
let get_mph  = fun _ _ -> raise Unimplemented
let get_face = fun _ _ -> raise Unimplemented

let parse_cat  = fun _ _ _ _ -> raise Unimplemented
let parse_elem = fun _ _ _ _ -> raise Unimplemented
let parse_mph  = fun _ _ _ _ -> raise Unimplemented
let parse_face = fun _ _ _ _ -> raise Unimplemented
