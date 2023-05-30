
module type Packable = sig
  type t
  val pack : t -> Msgpack.t Hyps.t
  val unpack : Msgpack.t -> t option Hyps.t
end

