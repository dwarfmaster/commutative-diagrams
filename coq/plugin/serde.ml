
exception Unimplemented

module Make(PA: Pa.ProofAssistant) = struct
  module Pk = Msgpack
  module St = Store.Make(PA.M)
  open St.Combinators
  open Data
  type 't m = ('t,PA.t) St.t

  module type Packable = sig
    type t
    val pack : t -> Pk.t m
    val unpack : Pk.t -> t option m
  end

  let mk_global_id tag id =
    (id lsl 3) lor (tag land 7)
  let un_id id = id lsr 3

  module Cat = struct
    type t = PA.t category

    let mk_id = mk_global_id 0

    let pack cat =
      match cat with
      | AtomicCategory data ->
          let po = Pk.Map [(Pk.String "term", Pk.Integer (mk_id data.cat_id))] in
          ret (Pk.Map [(Pk.String "atomic", po)])

    let unpack mp =
      match mp with
      | Pk.Map [ (Pk.String cons, mp) ] -> begin
        match cons, mp with
        | "atomic", Pk.Array [ Pk.Map [ (Pk.String name, Pk.Integer id) ] ] -> begin 
          match name with
          | "term" ->
              let id = un_id id in
              let* cats = St.getCategories () in
              if id < Array.length cats
              then ret (Some (AtomicCategory cats.(id)))
              else ret None
          | "existential" -> raise Unimplemented
          | _ -> ret None
        end
        | _ -> ret None
      end
      | _ -> ret None
  end

  module Funct = struct
    type t = PA.t funct
    let mk_id = mk_global_id 1
    let pack cat = raise Unimplemented
    let unpack mp = raise Unimplemented
  end

  module Elem = struct
    type t = PA.t elem
    let mk_id = mk_global_id 2
    let pack cat = raise Unimplemented
    let pack cat = raise Unimplemented
    let unpack mp = raise Unimplemented
  end

  module Mph = struct
    type t = PA.t morphism
    let pack cat = raise Unimplemented
    let mk_id = mk_global_id 3
    let pack cat = raise Unimplemented
    let unpack mp = raise Unimplemented
  end

  module Eq = struct
    type t = PA.t eq
    let mk_id = mk_global_id 4
    let pack cat = raise Unimplemented
    let pack cat = raise Unimplemented
    let unpack mp = raise Unimplemented
  end

end
