
module Make(PA: Pa.ProofAssistant) : sig
  module type Packable = sig
    type t
    val pack : t -> (Msgpack.t,PA.t) Store.Make(PA.M).t
    val unpack : Msgpack.t -> (t option,PA.t) Store.Make(PA.M).t
  end

  module Cat : Packable with type t = PA.t Data.category
  module Funct : Packable with type t = PA.t Data.funct
  module Elem : Packable with type t = PA.t Data.elem
  module Mph : Packable with type t = PA.t Data.morphism
  module Eq : Packable with type t = PA.t Data.eq
end
