
module Make(PA: Pa.ProofAssistant) : sig
  module type Packable = sig
    type t
    val pack : t -> (Msgpack.t,PA.t) Hyps.Make(PA.M).t
    val unpack : Msgpack.t -> (t option,PA.t) Hyps.Make(PA.M).t
  end

  val mk_cat_id : int -> int
  val mk_funct_id : int -> int
  val mk_elem_id : int -> int
  val mk_mph_id : int -> int
  val mk_eq_id : int -> int

  module Cat : Packable with type t = PA.t Data.category
  module Funct : Packable with type t = PA.t Data.funct
  module Elem : Packable with type t = PA.t Data.elem
  module Mph : Packable with type t = PA.t Data.morphism
  module Eq : Packable with type t = PA.t Data.eq
end
