
exception Unimplemented

module Make(PA: Pa.ProofAssistant) = struct
  module Pk = Msgpack
  module St = Store.Make(PA.M)
  open St.Combinators
  open Data
  type 't m = ('t,PA.t) St.t

  let cons (name : string) (arg : Pk.t) : Pk.t =
    Pk.Map [(Pk.String name, arg)]

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
          let po = cons "term" (Pk.Integer (mk_id data.cat_id)) in
          ret (cons "atomic" po)

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

    let pack funct =
      match funct with
      | AtomicFunctor data ->
          let po = cons "term" (Pk.Integer (mk_id data.funct_id)) in
          ret (Pk.Map [(Pk.String "atomic", po)])

    let unpack mp =
      match mp with
      | Pk.Map [ (Pk.String cons, mp) ] -> begin
        match cons, mp with
        | "atomic", Pk.Array [ Pk.Map [ (Pk.String name, Pk.Integer id) ] ] -> begin 
          match name with
          | "term" ->
              let id = un_id id in
              let* functs = St.getFunctors () in
              if id < Array.length functs
              then ret (Some (AtomicFunctor functs.(id)))
              else ret None
          | "existential" -> raise Unimplemented
          | _ -> ret None
        end
        | _ -> ret None
      end
      | _ -> ret None
  end

  module Elem = struct
    type t = PA.t elem
    let mk_id = mk_global_id 2

    let rec pack elem =
      match elem with
      | AtomicElem data ->
          let po = cons "term" (Pk.Integer (mk_id data.elem_id)) in
          ret (Pk.Map [(Pk.String "atomic", po)])
      | FObj (funct,elem) ->
          let* funct = Funct.pack funct in
          let* elem = pack elem in
          ret (cons "funct" (Pk.Array [funct; elem]))

    let rec unpack mp =
      match mp with
      | Pk.Map [ (Pk.String cons, mp) ] -> begin
        match cons, mp with
        | "atomic", Pk.Array [ Pk.Map [ (Pk.String name, Pk.Integer id) ] ] -> begin 
          match name with
          | "term" ->
              let id = un_id id in
              let* elems = St.getElems () in
              if id < Array.length elems 
              then ret (Some (AtomicElem elems.(id)))
              else ret None
          | "existential" -> raise Unimplemented
          | _ -> ret None
        end
        | "funct", Pk.Array [ funct; elem ] -> begin
          let* funct = Funct.unpack funct in
          let* elem = unpack elem in
          match funct, elem with
          | Some funct, Some elem -> ret (Some (FObj (funct,elem)))
          | _ -> ret None
        end
        | _ -> ret None
      end
      | _ -> ret None
  end

  module Mph = struct
    type t = PA.t morphism
    let mk_id = mk_global_id 3

    let rec pack mph =
      match mph with
      | AtomicMorphism data ->
          let po = cons "term" (Pk.Integer (mk_id data.mph_id)) in
          ret (Pk.Map [(Pk.String "atomic", po)])
      | Comp (m1,m2) ->
          let* m1 = pack m1 in
          let* m2 = pack m2 in
          ret (cons "comp" (Pk.Array [ m1; m2 ]))
      | Identity x ->
          let* x = Elem.pack x in
          ret (cons "identity" x)
      | FMph (funct,mph) ->
          let* funct = Funct.pack funct in
          let* mph = pack mph in
          ret (cons "funct" (Pk.Array [ funct; mph ]))
      | Inv _ -> raise Unimplemented (* Not supported on rust side *)

    let rec unpack mp =
      match mp with
      | Pk.Map [ (Pk.String cons, mp) ] -> begin
        match cons, mp with
        | "atomic", Pk.Array [ Pk.Map [ (Pk.String name, Pk.Integer id) ] ] -> begin 
          match name with
          | "term" ->
              let id = un_id id in
              let* mphs = St.getMorphisms () in
              if id < Array.length mphs
              then ret (Some (AtomicMorphism mphs.(id)))
              else ret None
          | "existential" -> raise Unimplemented
          | _ -> ret None
        end
        | "comp", Pk.Array [ m1; m2 ] -> begin
          let* m1 = unpack m1 in
          let* m2 = unpack m2 in
          match m1, m2 with
          | Some m1, Some m2 -> ret (Some (Comp (m1, m2)))
          | _ -> ret None
        end
        | "identity", x -> begin
          let* x = Elem.unpack x in
          match x with
          | Some x -> ret (Some (Identity x))
          | _ -> ret None
        end
        | "funct", Pk.Array [ funct; mph ] -> begin
          let* funct = Funct.unpack funct in
          let* mph = unpack mph in
          match funct, mph with
          | Some funct, Some mph -> ret (Some (FMph (funct,mph)))
          | _ -> ret None
        end
        | _ -> ret None
      end
      | _ -> ret None
  end

  module Eq = struct
    type t = PA.t eq
    let mk_id = mk_global_id 4
    let pack cat = raise Unimplemented
    let unpack mp = raise Unimplemented
  end

end
