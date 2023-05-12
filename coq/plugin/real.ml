
open Hyps.Combinators
open Data

let rec mapM f l =
  match l with
  | [] -> ret []
  | x :: t ->
      let* x = f x in
      let* t = mapM f t in
      x :: t |> ret

let rec instantiateAtomic atom tp =
  match atom with
  | Evar (i,None) ->
      let* status = Hyps.getEvar i in
      let* evar = match status with
      | Realized ev -> ret ev
      | Abstract ->
          let* evar = Hott.realizeEvar tp in
          let* _ = Hyps.instantiateEvar i evar in
          ret evar
      | NotFound ->
          let* evar = Hott.realizeEvar tp in
          let* _ = Hyps.newEvarAt i in
          let* _ = Hyps.instantiateEvar i evar in
          ret evar in
      ret (Evar (i, Some evar))
  | Cat c ->
      let* c = instantiateCat c in
      Cat c |> ret
  | Funct f ->
      let* f = instantiateFunct f in
      Funct f |> ret
  | Elem e ->
      let* e = instantiateElem e in
      Elem e |> ret
  | Mph m ->
      let* m = instantiateMorphism m in
      Mph m |> ret
  | Eq p ->
      let* p = instantiateEq p in
      Eq p |> ret
  (* TODO fix composed case *)
  (* | Composed (f,args) -> *)
  (*     let* f = instantiateAtomic f in *)
  (*     let* args = mapM instantiateAtomic args in *)
  (*     Composed (f, args) |> ret *)
  | _ -> ret atom

and instantiateCat cat =
  match cat with
  | AtomicCategory data ->
      let* tp = Hott.realizeCatType data in
      let* atom = instantiateAtomic data.cat_atom tp in
      AtomicCategory { cat_atom = atom } |> ret

and instantiateFunct funct =
  match funct with
  | AtomicFunctor data ->
      let* tp = Hott.realizeFunctType data in
      let* atom = instantiateAtomic data.funct_atom tp in
      AtomicFunctor { data with funct_atom = atom } |> ret

and instantiateElem elem =
  match elem with
  | AtomicElem data ->
      let* tp = Hott.realizeElemType data in
      let* atom = instantiateAtomic data.elem_atom tp in
      AtomicElem { data with elem_atom = atom } |> ret
  | FObj (f,o) ->
      let* f = instantiateFunct f in
      let* o = instantiateElem o in
      FObj (f,o) |> ret

and instantiateMorphism mph =
  match mph with
  | AtomicMorphism data ->
      let* tp = Hott.realizeMphType data in
      let* atom = instantiateAtomic data.mph_atom tp in
      AtomicMorphism { data with mph_atom = atom } |> ret
  | Identity o ->
      let* o = instantiateElem o in
      Identity o |> ret
  | Comp (m1,m2) ->
      let* m1 = instantiateMorphism m1 in
      let* m2 = instantiateMorphism m2 in
      Comp (m1,m2) |> ret
  | Inv m ->
      let* m = instantiateMorphism m in
      Inv m |> ret
  | FMph (f,m) ->
      let* f = instantiateFunct f in
      let* m = instantiateMorphism m in
      FMph (f,m) |> ret

and instantiateEq eq =
  match eq with
  | AtomicEq data ->
      let* tp = Hott.realizeEqType data in
      let* atom = instantiateAtomic data.eq_atom tp in
      AtomicEq { data with eq_atom = atom } |> ret
  | Refl m ->
      let* m = instantiateMorphism m in
      Refl m |> ret
  | Concat (p1,p2) ->
      let* p1 = instantiateEq p1 in
      let* p2 = instantiateEq p2 in
      Concat (p1,p2) |> ret
  | InvEq p ->
      let* p = instantiateEq p in
      InvEq p |> ret
  | Compose (p1,p2) ->
      let* p1 = instantiateEq p1 in
      let* p2 = instantiateEq p2 in
      Compose (p1,p2) |> ret
  | Assoc (m1,m2,m3) ->
      let* m1 = instantiateMorphism m1 in
      let* m2 = instantiateMorphism m2 in
      let* m3 = instantiateMorphism m3 in
      Assoc (m1,m2,m3) |> ret
  | LeftId m ->
      let* m = instantiateMorphism m in
      LeftId m |> ret
  | RightId m ->
      let* m = instantiateMorphism m in
      RightId m |> ret
  | RAp (p,m) ->
      let* p = instantiateEq p in
      let* m = instantiateMorphism m in
      RAp (p,m) |> ret
  | LAp (m,p) ->
      let* m = instantiateMorphism m in
      let* p = instantiateEq p in
      LAp (m,p) |> ret
  | RInv i -> RInv i |> ret
  | LInv i -> LInv i |> ret
  | Mono (mono,m1,m2,p) ->
      let* m1 = instantiateMorphism m1 in
      let* m2 = instantiateMorphism m2 in
      let* p = instantiateEq p in
      Mono (mono,m1,m2,p) |> ret
  | Epi (epi,m1,m2,p) ->
      let* m1 = instantiateMorphism m1 in
      let* m2 = instantiateMorphism m2 in
      let* p = instantiateEq p in
      Epi (epi,m1,m2,p) |> ret
  | FId (f,o) ->
      let* f = instantiateFunct f in
      let* o = instantiateElem o in
      FId (f,o) |> ret
  | FComp (f,m1,m2) ->
      let* f = instantiateFunct f in
      let* m1 = instantiateMorphism m1 in
      let* m2 = instantiateMorphism m2 in
      FComp (f,m1,m2) |> ret
  | FCtx (f,p) ->
      let* f = instantiateFunct f in
      let* p = instantiateEq p in
      FCtx (f,p) |> ret

(* Wrappers over Hott.realize* that instantiate all evars before doing the realization *)
let realizeCategory cat =
  let* cat = instantiateCat cat in
  Hott.realizeCategory cat
let realizeFunctor funct =
  let* funct = instantiateFunct funct in
  Hott.realizeFunctor funct
let realizeElem elem =
  let* elem = instantiateElem elem in
  Hott.realizeElem elem
let realizeMorphism mph =
  let* mph = instantiateMorphism mph in
  Hott.realizeMorphism mph
let realizeEq eq = 
  let* eq = instantiateEq eq in
  Hott.realizeEq eq
