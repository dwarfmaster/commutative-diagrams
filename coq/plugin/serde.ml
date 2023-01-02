
exception Unimplemented

module Make(PA: Pa.ProofAssistant) = struct
  module Pk = Msgpack
  module St = Hyps.Make(PA.M)
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

    let rec pack eq =
      match eq with
      | AtomicEq data ->
          let po = cons "term" (Pk.Integer (mk_id data.eq_id)) in
          ret (Pk.Map [(Pk.String "atomic", po)])
      | Hole _ -> raise Unimplemented (* Should become an existential *)
      | Refl m ->
          let* m = Mph.pack m in
          ret (cons "refl" m)
      | Concat (p1,p2) ->
          let* p1 = pack p1 in
          let* p2 = pack p2 in
          ret (cons "concat" (Pk.Array [ p1; p2 ]))
      | InvEq p ->
          let* p = pack p in
          ret (cons "inv" p)
      | Compose (p1,p2) ->
          let* p1 = pack p1 in
          let* p2 = pack p2 in
          ret (cons "compose" (Pk.Array [ p1; p2 ]))
      | Assoc (m1,m2,m3) ->
          let* m1 = Mph.pack m1 in
          let* m2 = Mph.pack m2 in
          let* m3 = Mph.pack m3 in
          ret (cons "assoc" (Pk.Array [ m1; m2; m3 ]))
      | LeftId m ->
          let* m = Mph.pack m in
          ret (cons "left_id" m)
      | RightId m ->
          let* m = Mph.pack m in
          ret (cons "right_id" m)
      | RAp (p,m) ->
          let* p = pack p in
          let* m = Mph.pack m in
          ret (cons "rap" (Pk.Array [ p; m ]))
      | LAp (m,p) ->
          let* m = Mph.pack m in
          let* p = pack p in
          ret (cons "lap" (Pk.Array [ m; p ]))
      | RInv _ -> raise Unimplemented (* not implemented on rust side *)
      | LInv _ -> raise Unimplemented (* not implemented on rust side *)
      | Mono _ -> raise Unimplemented (* not implemented on rust side *)
      | Epi _ -> raise Unimplemented (* not implemented on rust side *)
      | FId (f,x) ->
          let* f = Funct.pack f in
          let* x = Elem.pack x in
          ret (cons "funct_id" (Pk.Array [ f; x ]))
      | FComp (f, m1, m2) ->
          let* f = Funct.pack f in
          let* m1 = Mph.pack m1 in
          let* m2 = Mph.pack m2 in
          ret (cons "funct_comp" (Pk.Array [ f; m1; m2 ]))
      | FCtx (f,p) ->
          let* f = Funct.pack f in
          let* p = pack p in
          ret (cons "funct_ctx" (Pk.Array [ f; p ]))

    let rec unpack mp =
      match mp with
      | Pk.Map [ (Pk.String cons, mp) ] -> begin
        match cons, mp with
        | "atomic", Pk.Array [ Pk.Map [ (Pk.String name, Pk.Integer id) ] ] -> begin 
          match name with
          | "term" ->
              let id = un_id id in
              let* eqs = St.getEqs () in
              if id < Array.length eqs 
              then ret (Some (AtomicEq eqs.(id)))
              else ret None
          | "existential" -> raise Unimplemented
          | _ -> ret None
        end
        | "refl", m -> begin
            let* m = Mph.unpack m in
            match m with
            | Some m -> ret (Some (Refl m))
            | _ -> ret None
        end
        | "concat", Pk.Array [ p1; p2 ] -> begin
          let* p1 = unpack p1 in
          let* p2 = unpack p2 in
          match p1, p2 with
          | Some p1, Some p2 -> ret (Some (Concat (p1,p2)))
          | _ -> ret None
        end
        | "inv", eq -> begin
          let* eq = unpack eq in
          match eq with
          | Some eq -> ret (Some (InvEq eq))
          | _ -> ret None
        end
        | "compose", Pk.Array [ p1; p2 ] -> begin
          let* p1 = unpack p1 in
          let* p2 = unpack p2 in
          match p1, p2 with
          | Some p1, Some p2 -> ret (Some (Compose (p1,p2)))
          | _ -> ret None
        end
        | "assoc", Pk.Array [ m1; m2; m3 ] -> begin
          let* m1 = Mph.unpack m1 in
          let* m2 = Mph.unpack m2 in
          let* m3 = Mph.unpack m3 in
          match m1,m2,m3 with
          | Some m1, Some m2, Some m3 -> ret (Some (Assoc(m1,m2,m3)))
          | _ -> ret None
        end
        | "left_id", m -> begin
          let* m = Mph.unpack m in
          match m with
          | Some m -> ret (Some (LeftId m))
          | _ -> ret None
        end
        | "right_id", m -> begin
          let* m = Mph.unpack m in
          match m with
          | Some m -> ret (Some (RightId m))
          | _ -> ret None
        end
        | "rap", Pk.Array [ p; m ] -> begin
          let* p = unpack p in
          let* m = Mph.unpack m in
          match p,m with
          | Some p, Some m -> ret (Some (RAp (p,m)))
          | _ -> ret None
        end
        | "lap", Pk.Array [ m; p ] -> begin
          let* m = Mph.unpack m in
          let* p = unpack p in
          match m,p with
          | Some m, Some p -> ret (Some (LAp (m,p)))
          | _ -> ret None
        end
        | "funct_id", Pk.Array [ f; x ] -> begin
          let* f = Funct.unpack f in
          let* x = Elem.unpack x in
          match f, x with
          | Some f, Some x -> ret (Some (FId (f,x)))
          | _ -> ret None
        end
        | "funct_comp", Pk.Array [ f; m1; m2 ] -> begin
          let* f = Funct.unpack f in
          let* m1 = Mph.unpack m1 in
          let* m2 = Mph.unpack m2 in
          match f, m1, m2 with
          | Some f, Some m1, Some m2 -> ret (Some (FComp (f,m1,m2)))
          | _ -> ret None
        end
        | "funct_ctx", Pk.Array [ f; p ] -> begin
          let* f = Funct.unpack f in
          let* p = unpack p in
          match f, p with
          | Some f, Some p -> ret (Some (FCtx (f,p)))
          | _ -> ret None
        end
        | _ -> ret None
      end
      | _ -> ret None
  end

end
