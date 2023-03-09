
exception Unimplemented

module Make(PA: Pa.ProofAssistant) = struct
  module Pk = Msgpack
  module St = Hyps.Make(PA.M)
  open St.Combinators
  open Data
  type 't m = ('t,PA.t) St.t

  let some x = ret (Some x)
  let none () = ret None

  let cons (name : string) (arg : Pk.t) : Pk.t =
    Pk.Map [(Pk.String name, arg)]

  module type Packable = sig
    type t
    val pack : t -> Pk.t m
    val unpack : Pk.t -> t option m
  end

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
  let rec pack_atom atom =
    match atom with
    | Ctx (i,_) -> cons "term" (Pk.Integer i) |> ret
    | Evar (e,_) -> cons "existential" (Pk.Integer e) |> ret
    | Cat c -> cons "category" <$> pack_cat c
    | Funct f -> cons "functor" <$> pack_funct f
    | Elem e -> cons "object" <$> pack_elem e
    | Mph m -> cons "morphism" <$> pack_mph m
    | Eq e -> cons "equality" <$> pack_eq e
    | Composed (id,t,args) ->
        let* name = PA.print t |> lift in
        let* args = mapM pack_atom args in
        cons "composed" (Pk.Array [Pk.Integer id; Pk.String name; Pk.Array args]) |> ret
  and pack_cat cat =
    match cat with
    | AtomicCategory data -> 
        let* po = pack_atom data.cat_atom in
        ret (cons "atomic" (cons "pobj" po))
  and pack_funct funct =
    match funct with
    | AtomicFunctor data ->
        let* po = pack_atom data.funct_atom in
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
        let* po = pack_atom data.elem_atom in
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
        let* po = pack_atom data.mph_atom in
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
        let* po = pack_atom data.eq_atom in
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




(*  ____                      _       _ _           _   _              *)
(* |  _ \  ___  ___  ___ _ __(_) __ _| (_)___  __ _| |_(_) ___  _ __   *)
(* | | | |/ _ \/ __|/ _ \ '__| |/ _` | | / __|/ _` | __| |/ _ \| '_ \  *)
(* | |_| |  __/\__ \  __/ |  | | (_| | | \__ \ (_| | |_| | (_) | | | | *)
(* |____/ \___||___/\___|_|  |_|\__,_|_|_|___/\__,_|\__|_|\___/|_| |_| *)
(*                                                                     *)
  let unpack_evar id =
    let* ev = St.getEvar id in
    match ev with
    | Abstract -> ret (Evar (id,None))
    | Realized e -> ret (Evar (id,Some e))
    | NotFound -> let* _ = St.newEvarAt id in ret (Evar (id,None))

  let rec unpack_atom mp =
    match mp with
    | Pk.Map [ (Pk.String "term", Pk.Integer id) ] -> begin
        let* atom = St.getAtom id in
        match atom with
        | Some (Ctx (i,ec)) -> some (Ctx (i,ec))
        | _ -> none ()
    end
    | Pk.Map [ (Pk.String "existential", Pk.Integer ex) ] ->
        some @<< unpack_evar ex
    | Pk.Map [ (Pk.String "category", cat) ] -> begin
        let* cat = unpack_cat cat in
        match cat with
        | Some cat -> some (Cat cat)
        | _ -> none ()
    end
    | Pk.Map [ (Pk.String "functor", funct) ] -> begin
        let* funct = unpack_funct funct in
        match funct with
        | Some funct -> some (Funct funct)
        | _ -> none ()
    end
    | Pk.Map [ (Pk.String "object", elem) ] -> begin
        let* elem = unpack_elem elem in
        match elem with
        | Some elem -> some (Elem elem)
        | _ -> none ()
    end
    | Pk.Map [ (Pk.String "morphism", mph) ] -> begin
        let* mph = unpack_mph mph in
        match mph with
        | Some mph -> some (Mph mph)
        | _ -> none ()
    end
    | Pk.Map [ (Pk.String "equality", eq) ] -> begin
        let* eq = unpack_eq eq in
        match eq with
        | Some eq -> some (Eq eq)
        | _ -> none ()
    end
    | Pk.Map [ (Pk.String "composed", Pk.Array [Pk.Integer id; Pk.String _; Pk.Nil]) ] ->
        let* ec = St.getFun id in
        some (Composed (id, ec, []))
    | Pk.Map [ (Pk.String "composed", Pk.Array [Pk.Integer id; Pk.String _; Pk.Array subs]) ] ->
        let rec foldOptions = function
          | [] -> Some []
          | None :: _ -> None
          | Some x :: t -> begin
            match foldOptions t with
            | Some t -> Some (x :: t)
            | None -> None
          end in
        let* ec = St.getFun id in
        let* subs = foldOptions <$> mapM unpack_atom subs in
        begin match subs with
        | Some subs -> some (Composed (id, ec, subs))
        | _ -> none ()
        end
    | _ -> none ()
  and unpack_cat mp =
    match mp with
    | Pk.Map [ (Pk.String cons, mp) ] -> begin
      match cons, mp with
      | "atomic", Pk.Array [ atom ] -> begin
        let* atom = unpack_atom atom in
        match atom with
        | Some atom -> some (AtomicCategory { cat_atom = atom })
        | _ -> none ()
      end
      | _ -> none ()
    end
    | _ -> ret None
  and unpack_funct mp =
    match mp with
    | Pk.Map [ (Pk.String cons, mp) ] -> begin
      match cons, mp with
      | "atomic", Pk.Array [ atom; src; dst ] -> begin
        let* atom = unpack_atom atom in
        let* src = unpack_cat src in
        let* dst = unpack_cat dst in
        match atom, src, dst with
        | Some atom, Some src, Some dst ->
            some (AtomicFunctor {
              funct_atom = atom;
              funct_src_ = src;
              funct_dst_ = dst;
            })
        | _ -> none ()
      end
      | _ -> ret None
    end
    | _ -> ret None
  and unpack_elem mp =
    match mp with
    | Pk.Map [ (Pk.String cons, mp) ] -> begin
      match cons, mp with
      | "atomic", Pk.Array [ atom; cat ] -> begin 
        let* atom = unpack_atom atom in
        let* cat = unpack_cat cat in
        match atom, cat with
        | Some atom, Some cat ->
            ret (Some (AtomicElem {
              elem_atom = atom;
              elem_cat_ = cat;
            }))
        | _ -> ret None
      end
      | "funct", Pk.Array [ funct; elem ] -> begin
        let* funct = unpack_funct funct in
        let* elem = unpack_elem elem in
        match funct, elem with
        | Some funct, Some elem -> ret (Some (FObj (funct,elem)))
        | _ -> ret None
      end
      | _ -> ret None
    end
    | _ -> ret None
  and unpack_mph mp =
    match mp with
    | Pk.Map [ (Pk.String cons, mp) ] -> begin
      match cons, mp with
      | "atomic", Pk.Array [ atom; cat; src; dst ] -> begin
        let* atom = unpack_atom atom in
        let* cat = unpack_cat cat in
        let* src = unpack_elem src in
        let* dst = unpack_elem dst in
        match atom, cat, src, dst with
        | Some atom, Some cat, Some src, Some dst ->
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
        let* m1 = unpack_mph m1 in
        let* m2 = unpack_mph m2 in
        match m1, m2 with
        | Some m1, Some m2 -> ret (Some (Comp (m1, m2)))
        | _ -> ret None
      end
      | "identity", x -> begin
        let* x = unpack_elem x in
        match x with
        | Some x -> ret (Some (Identity x))
        | _ -> ret None
      end
      | "funct", Pk.Array [ funct; mph ] -> begin
        let* funct = unpack_funct funct in
        let* mph = unpack_mph mph in
        match funct, mph with
        | Some funct, Some mph -> ret (Some (FMph (funct,mph)))
        | _ -> ret None
      end
      | _ -> ret None
    end
    | _ -> ret None
  and unpack_eq mp =
    match mp with
    | Pk.Map [ (Pk.String cons, mp) ] -> begin
      match cons, mp with
      | "atomic", Pk.Array [ atom; cat; src; dst; left; right ] -> begin 
        let* atom = unpack_atom atom in
        let* cat = unpack_cat cat in
        let* src = unpack_elem src in
        let* dst = unpack_elem dst in
        let* left = unpack_mph left in
        let* right = unpack_mph right in
        match atom, cat, src, dst, left, right with
        | Some atom, Some cat, Some src, Some dst, Some left, Some right ->
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
          let* m = unpack_mph m in
          match m with
          | Some m -> ret (Some (Refl m))
          | _ -> ret None
      end
      | "concat", Pk.Array [ p1; p2 ] -> begin
        let* p1 = unpack_eq p1 in
        let* p2 = unpack_eq p2 in
        match p1, p2 with
        | Some p1, Some p2 -> ret (Some (Concat (p1,p2)))
        | _ -> ret None
      end
      | "inv", eq -> begin
        let* eq = unpack_eq eq in
        match eq with
        | Some eq -> ret (Some (InvEq eq))
        | _ -> ret None
      end
      | "compose", Pk.Array [ p1; p2 ] -> begin
        let* p1 = unpack_eq p1 in
        let* p2 = unpack_eq p2 in
        match p1, p2 with
        | Some p1, Some p2 -> ret (Some (Compose (p1,p2)))
        | _ -> ret None
      end
      | "assoc", Pk.Array [ m1; m2; m3 ] -> begin
        let* m1 = unpack_mph m1 in
        let* m2 = unpack_mph m2 in
        let* m3 = unpack_mph m3 in
        match m1,m2,m3 with
        | Some m1, Some m2, Some m3 -> ret (Some (Assoc(m1,m2,m3)))
        | _ -> ret None
      end
      | "left_id", m -> begin
        let* m = unpack_mph m in
        match m with
        | Some m -> ret (Some (RightId m))
        | _ -> ret None
      end
      | "right_id", m -> begin
        let* m = unpack_mph m in
        match m with
        | Some m -> ret (Some (LeftId m))
        | _ -> ret None
      end
      | "rap", Pk.Array [ p; m ] -> begin
        let* p = unpack_eq p in
        let* m = unpack_mph m in
        match p,m with
        | Some p, Some m -> ret (Some (RAp (p,m)))
        | _ -> ret None
      end
      | "lap", Pk.Array [ m; p ] -> begin
        let* m = unpack_mph m in
        let* p = unpack_eq p in
        match m,p with
        | Some m, Some p -> ret (Some (LAp (m,p)))
        | _ -> ret None
      end
      | "funct_id", Pk.Array [ f; x ] -> begin
        let* f = unpack_funct f in
        let* x = unpack_elem x in
        match f, x with
        | Some f, Some x -> ret (Some (FId (f,x)))
        | _ -> ret None
      end
      | "funct_comp", Pk.Array [ f; m1; m2 ] -> begin
        let* f = unpack_funct f in
        let* m1 = unpack_mph m1 in
        let* m2 = unpack_mph m2 in
        match f, m1, m2 with
        | Some f, Some m1, Some m2 -> ret (Some (FComp (f,m1,m2)))
        | _ -> ret None
      end
      | "funct_ctx", Pk.Array [ f; p ] -> begin
        let* f = unpack_funct f in
        let* p = unpack_eq p in
        match f, p with
        | Some f, Some p -> ret (Some (FCtx (f,p)))
        | _ -> ret None
      end
      | _ -> ret None
    end
    | _ -> ret None


(*  __  __           _       _            *)
(* |  \/  | ___   __| |_   _| | ___  ___  *)
(* | |\/| |/ _ \ / _` | | | | |/ _ \/ __| *)
(* | |  | | (_) | (_| | |_| | |  __/\__ \ *)
(* |_|  |_|\___/ \__,_|\__,_|_|\___||___/ *)
(*                                        *)
  module Cat = struct
    type t = PA.t category
    let pack = pack_cat
    let unpack = unpack_cat
  end

  module Funct = struct
    type t = PA.t funct
    let pack = pack_funct
    let unpack = unpack_funct
  end

  module Elem = struct
    type t = PA.t elem
    let pack = pack_elem
    let unpack = unpack_elem
  end

  module Mph = struct
    type t = PA.t morphism
    let pack = pack_mph
    let unpack = unpack_mph
  end

  module Eq = struct
    type t = PA.t eq
    let pack = pack_eq
    let unpack = unpack_eq
  end

end
