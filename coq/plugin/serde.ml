
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

  let mk_cat_id = mk_global_id 0
  let mk_funct_id = mk_global_id 1
  let mk_elem_id = mk_global_id 2
  let mk_mph_id = mk_global_id 3
  let mk_eq_id = mk_global_id 4

  let rec mapM f = function
    | [] -> ret []
    | x :: t ->
        let* x = f x in
        let* t = mapM f t in
        ret (x :: t)

(*  ____            _       _ _           _   _              *)
(* / ___|  ___ _ __(_) __ _| (_)___  __ _| |_(_) ___  _ __   *)
(* \___ \ / _ \ '__| |/ _` | | / __|/ _` | __| |/ _ \| '_ \  *)
(*  ___) |  __/ |  | | (_| | | \__ \ (_| | |_| | (_) | | | | *)
(* |____/ \___|_|  |_|\__,_|_|_|___/\__,_|\__|_|\___/|_| |_| *)
(*                                                           *)
  let rec pack_atom mkid atom =
    match atom with
    | Ctx (i,_) -> cons "term" (Pk.Integer (mkid i)) |> ret
    | Evar (e,_) -> cons "existential" (Pk.Integer e) |> ret
    | Cat c -> cons "category" <$> pack_cat c
    | Funct f -> cons "functor" <$> pack_funct f
    | Elem e -> cons "object" <$> pack_elem e
    | Mph m -> cons "morphism" <$> pack_mph m
    | Eq e -> cons "equality" <$> pack_eq e
    | Composed (id,t,args) ->
        let* name = PA.print t |> lift in
        (* TODO get rid of mkid, since on recursion we don't know this is what we want *)
        let* args = mapM (pack_atom mkid) args in
        cons "composed" (Pk.Array [Pk.Integer id; Pk.String name; Pk.Array args]) |> ret
  and pack_cat cat =
    match cat with
    | AtomicCategory data -> 
        let* po = pack_atom mk_cat_id data.cat_atom in
        ret (cons "atomic" (cons "pobj" po))
  and pack_funct funct =
    match funct with
    | AtomicFunctor data ->
        let* po = pack_atom mk_funct_id data.funct_atom in
        let* src = pack_cat data.funct_src_ in
        let* dst = pack_cat data.funct_dst_ in
        let po = Pk.Map [
          (Pk.String "pobj", po);
          (Pk.String "src", src);
          (Pk.String "dst", dst);
        ] in
        ret (cons "atomic" po)
  and pack_elem elem =
    match elem with
    | AtomicElem data ->
        let* po = pack_atom mk_elem_id data.elem_atom in
        let* cat = pack_cat data.elem_cat_ in
        let po = Pk.Map [
          (Pk.String "pobj", po);
          (Pk.String "category", cat);
        ] in
      ret (cons "atomic" po)
    | FObj (funct,elem) ->
        let* funct = pack_funct funct in
        let* elem = pack_elem elem in
        ret (cons "funct" (Pk.Array [funct; elem]))
  and pack_mph mph =
    match mph with
    | AtomicMorphism data ->
        let* po = pack_atom mk_mph_id data.mph_atom in
        let* cat = pack_cat data.mph_cat_ in
        let* src = pack_elem data.mph_src_ in
        let* dst = pack_elem data.mph_dst_ in
        let po = Pk.Map [
          (Pk.String "pobj", po);
          (Pk.String "category", cat);
          (Pk.String "src", src);
          (Pk.String "dst", dst);
        ] in
        ret (cons "atomic" po)
    | Comp (m1,m2) ->
        let* m1 = pack_mph m1 in
        let* m2 = pack_mph m2 in
        ret (cons "comp" (Pk.Array [ m1; m2 ]))
    | Identity x ->
        let* x = pack_elem x in
        ret (cons "identity" x)
    | FMph (funct,mph) ->
        let* funct = pack_funct funct in
        let* mph = pack_mph mph in
        ret (cons "funct" (Pk.Array [ funct; mph ]))
    | Inv _ -> raise Unimplemented (* Not supported on rust side *)
  and pack_eq eq =
    match eq with
    | AtomicEq data -> 
        let* po = pack_atom mk_eq_id data.eq_atom in
        let* cat = pack_cat data.eq_cat_ in
        let* src = pack_elem data.eq_src_ in
        let* dst = pack_elem data.eq_dst_ in
        let* left = pack_mph data.eq_left_ in
        let* right = pack_mph data.eq_right_ in
        let po = Pk.Map [
          (Pk.String "pobj", po);
          (Pk.String "category", cat);
          (Pk.String "src", src);
          (Pk.String "dst", dst);
          (Pk.String "left", left);
          (Pk.String "right", right);
        ] in
        ret (cons "atomic" po)
    | Refl m ->
        let* m = pack_mph m in
        ret (cons "refl" m)
    | Concat (p1,p2) ->
        let* p1 = pack_eq p1 in
        let* p2 = pack_eq p2 in
        ret (cons "concat" (Pk.Array [ p1; p2 ]))
    | InvEq p ->
        let* p = pack_eq p in
        ret (cons "inv" p)
    | Compose (p1,p2) ->
        let* p1 = pack_eq p1 in
        let* p2 = pack_eq p2 in
        ret (cons "compose" (Pk.Array [ p1; p2 ]))
    | Assoc (m1,m2,m3) ->
        let* m1 = pack_mph m1 in
        let* m2 = pack_mph m2 in
        let* m3 = pack_mph m3 in
        ret (cons "assoc" (Pk.Array [ m1; m2; m3 ]))
    | LeftId m ->
        let* m = pack_mph m in
        ret (cons "right_id" m)
    | RightId m ->
        let* m = pack_mph m in
        ret (cons "left_id" m)
    | RAp (p,m) ->
        let* p = pack_eq p in
        let* m = pack_mph m in
        ret (cons "rap" (Pk.Array [ p; m ]))
    | LAp (m,p) ->
        let* m = pack_mph m in
        let* p = pack_eq p in
        ret (cons "lap" (Pk.Array [ m; p ]))
    | RInv _ -> raise Unimplemented (* not implemented on rust side *)
    | LInv _ -> raise Unimplemented (* not implemented on rust side *)
    | Mono _ -> raise Unimplemented (* not implemented on rust side *)
    | Epi _ -> raise Unimplemented (* not implemented on rust side *)
    | FId (f,x) ->
        let* f = pack_funct f in
        let* x = pack_elem x in
        ret (cons "funct_id" (Pk.Array [ f; x ]))
    | FComp (f, m1, m2) ->
        let* f = pack_funct f in
        let* m1 = pack_mph m1 in
        let* m2 = pack_mph m2 in
        ret (cons "funct_comp" (Pk.Array [ f; m1; m2 ]))
    | FCtx (f,p) ->
        let* f = pack_funct f in
        let* p = pack_eq p in
        ret (cons "funct_ctx" (Pk.Array [ f; p ]))

  let unpack_evar id =
    let* ev = St.getEvar id in
    match ev with
    | Abstract -> ret (Evar (id,None))
    | Realized e -> ret (Evar (id,Some e))
    | NotFound -> let* _ = St.newEvarAt id in ret (Evar (id,None))

  module Cat = struct
    type t = PA.t category

    let pack = pack_cat

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
          | "existential" ->
              let* atom = unpack_evar id in
              ret (Some (AtomicCategory { cat_atom = atom }))
          | _ -> ret None
        end
        | _ -> ret None
      end
      | _ -> ret None
  end

  module Funct = struct
    type t = PA.t funct

    let pack = pack_funct

    let unpack mp =
      match mp with
      | Pk.Map [ (Pk.String cons, mp) ] -> begin
        match cons, mp with
        | "atomic", Pk.Array [ Pk.Map [ (Pk.String name, Pk.Integer id) ]; src; dst ] -> begin 
          let* src = Cat.unpack src in
          let* dst = Cat.unpack dst in
          match src, dst, name with
          | Some _, Some _, "term" ->
              let id = un_id id in
              let* functs = St.getFunctors () in
              if id < Array.length functs
              then ret (Some (AtomicFunctor functs.(id)))
              else ret None
          | Some src, Some dst, "existential" ->
              let* atom = unpack_evar id in
              ret (Some (AtomicFunctor {
                funct_atom = atom;
                funct_src_ = src;
                funct_dst_ = dst;
              }))
          | _ -> ret None
        end
        | _ -> ret None
      end
      | _ -> ret None
  end

  module Elem = struct
    type t = PA.t elem

    let pack = pack_elem

    let rec unpack mp =
      match mp with
      | Pk.Map [ (Pk.String cons, mp) ] -> begin
        match cons, mp with
        | "atomic", Pk.Array [ Pk.Map [ (Pk.String name, Pk.Integer id) ]; cat ] -> begin 
          let* cat = Cat.unpack cat in
          match cat, name with
          | Some _, "term" ->
              let id = un_id id in
              let* elems = St.getElems () in
              if id < Array.length elems 
              then ret (Some (AtomicElem elems.(id)))
              else ret None
          | Some cat, "existential" ->
              let* atom = unpack_evar id in
              ret (Some (AtomicElem {
                elem_atom = atom;
                elem_cat_ = cat;
              }))
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

    let pack = pack_mph

    let rec unpack mp =
      match mp with
      | Pk.Map [ (Pk.String cons, mp) ] -> begin
        match cons, mp with
        | "atomic", Pk.Array [ Pk.Map [ (Pk.String name, Pk.Integer id) ]; cat; src; dst ] -> begin
          let* cat = Cat.unpack cat in
          let* src = Elem.unpack src in
          let* dst = Elem.unpack dst in
          match cat, src, dst, name with
          | Some _, Some _, Some _, "term" ->
              let id = un_id id in
              let* mphs = St.getMorphisms () in
              if id < Array.length mphs
              then ret (Some (AtomicMorphism mphs.(id)))
              else ret None
          | Some cat, Some src, Some dst, "existential" ->
              let* atom = unpack_evar id in
              ret (Some (AtomicMorphism {
                mph_atom = atom;
                mph_cat_ = cat;
                mph_src_ = src;
                mph_dst_ = dst;
                mono = None;
                epi = None;
                iso = None;
              }))
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

    let pack = pack_eq

    let rec unpack mp =
      match mp with
      | Pk.Map [ (Pk.String cons, mp) ] -> begin
        match cons, mp with
        | "atomic", Pk.Array [ Pk.Map [ (Pk.String name, Pk.Integer id) ]; cat; src; dst; left; right ] -> begin 
          let* cat = Cat.unpack cat in
          let* src = Elem.unpack src in
          let* dst = Elem.unpack dst in
          let* left = Mph.unpack left in
          let* right = Mph.unpack right in
          match cat, src, dst, left, right, name with
          | Some _, Some _, Some _, Some _, Some _, "term" ->
              let id = un_id id in
              let* eqs = St.getEqs () in
              if id < Array.length eqs 
              then ret (Some (AtomicEq eqs.(id)))
              else ret None
          | Some cat, Some src, Some dst, Some left, Some right, "existential" ->
              let* atom = unpack_evar id in
              ret (Some (AtomicEq {
                eq_atom = atom;
                eq_cat_ = cat;
                eq_src_ = src;
                eq_dst_ = dst;
                eq_left_ = left;
                eq_right_ = right;
              }))
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
          | Some m -> ret (Some (RightId m))
          | _ -> ret None
        end
        | "right_id", m -> begin
          let* m = Mph.unpack m in
          match m with
          | Some m -> ret (Some (LeftId m))
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
