
module type Packable = sig
  type t
  val pack : t -> Msgpack.t Hyps.t
  val unpack : Msgpack.t -> t option Hyps.t
end

module Cat : Packable with type t = Data.category
module Funct : Packable with type t = Data.funct
module Elem : Packable with type t = Data.elem
module Mph : Packable with type t = Data.morphism
module Eq : Packable with type t = Data.eq
