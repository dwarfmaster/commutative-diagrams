
type preCategoryShape =
  | Cat of EConstr.t
type preCategory =
  { shape : preCategoryShape }
type preElemShape =
  | Elem of EConstr.t
type preElem =
  { shape    : preElemShape
  ; category : preCategory
  }
type preMorphismType =
  { src : preElem
  ; dst : preElem
  ; category : preCategory
  ; obj : EConstr.t
  }
type preMorphismShape =
  | Id   of preElem
  | Comp of preMorphism * preMorphism
  | Mph  of EConstr.t
and preMorphism =
  { shape : preMorphismShape
  ; tp : preMorphismType
  }
type preIso =
  { mph : preMorphism
  ; inv : preMorphism
  ; iso : EConstr.t
  }
type preMono =
  { mph : preMorphism
  ; mono : EConstr.t 
  }
type preEpi =
  { mph : preMorphism
  ; epi : EConstr.t
  }
type 'a preEqShape =
  | Refl of 'a
  | Concat of 'a preEq * 'a preEq
  | Inv of 'a preEq
  | Eq of EConstr.t
and 'a preEq =
  { shape : 'a preEqShape
  ; src : 'a
  ; dst : 'a 
  ; tp  : EConstr.t
  }
type preFace =
  { tp  : preMorphismType
  ; obj : preMorphism preEq
  }

open Monad

let normCategory (cat : preCategory) : Data.category m =
  match cat.shape with
  | Cat cat -> getCategory @<< initCategory cat

let normElem (elem : preElem) : Data.elem m =
  let* cat = normCategory elem.category in 
  match elem.shape with
  | Elem elem -> getElem @<< initElem cat elem

let normMorphismT (mphT : preMorphismType) : Data.morphismT m =
  let* cat = normCategory mphT.category in 
  let* src = normElem mphT.src in 
  let* dst = normElem mphT.dst in 
  ret { Data.category = cat; src = src; dst = dst; obj = mphT.obj }

(* TODO *)
let normMorphism (m : preMorphism) : Data.path m =
  assert false
  (* let* env = getEnv in  *)
  (* let* sigma = getEvarMap in  *)
  (* let* mph = liftTactic (Hott.parse_compose env m.obj) in  *)
  (* match mph with *)
  (* | Some (cat,src,int,dst,msi,mid) -> *)
  (*     let* catP = getCategory @<< initCategory cat in  *)
  (*     let* intP = getElem @<< initElem cat int in  *)
  (*     let* obj = liftTactic (Hott.morphism env m.tp.category.obj src int) in  *)
  (*     let msi = *)
  (*       { *)
  (*       } *)
  (*     let msi = *)
  (*       { obj = msi *)
  (*       ; tp = { category = m.tp.category  *)
  (*              ; src = m.tp.src *)
  (*              ; dst = intP  *)
  (*              ; obj = obj } *)
  (*       } in *)
  (*     let* obj = liftTactic (Hott.morphism env m.tp.category.obj int dst) in  *)
  (*     let mid = *)
  (*       { obj = mid  *)
  (*       ; tp = { category = m.tp.category  *)
  (*              ; src = intP *)
  (*              ; dst = m.tp.dst  *)
  (*              ; obj = obj } *)
  (*       } in *)
  (*     let* p1 = normMorphism msi in *)
  (*     assert false *)

let normIso (iso : preIso) : Data.isoData m =
  let* mph = normMorphism iso.mph in
  let* inv = normMorphism iso.inv in
  match mph.path, inv.path with
  | [ mph ], [ inv ] ->
      begin match mph.shape, inv.shape with
      | Base mph, Base inv ->
        ret { Data.obj = iso.iso; mph = mph; inv = inv }
      end
  | _, _ -> assert false

let normEpi (epi : preEpi) : Data.epiData m =
  let* mph = normMorphism epi.mph in 
  match mph.path with
  | [ mph ] ->
      begin match mph.shape with
      | Base mph -> ret ({ obj = epi.epi; mph = mph } : Data.epiData)
      end
  | _ -> assert false

let normMono (mono : preMono) : Data.monoData m =
  let* mph = normMorphism mono.mph in 
  match mph.path with
  | [ mph ] ->
      begin match mph.shape with
      | Base mph -> ret ({ obj = mono.mono; mph = mph } : Data.monoData)
      end
  | _ -> assert false

let makeEq (p1: Data.path) (p2 : Data.path) (eqT : Data.eqT) : Data.eq =
  { Data.src = p1.mph; dst = p2.mph; tp = p1.mph.tp; eq = eqT }

(* TODO handle tp data *)
let rec normEqT (shape : preMorphism preEqShape) : Data.eqT m =
  match shape with
  | Refl mph ->
      let* mph = normMorphism mph in 
      ret (Data.Refl mph.mph)
  | Concat (eq1,eq2) ->
      let* eq1 = normEq eq1 in 
      let* eq2 = normEq eq2 in 
      ret (Data.Concat (eq1,eq2))
  | Inv eq ->
      let* eq = normEq eq in
      ret (Data.Inv eq)
  | Eq eq ->
      ret (Data.Atom eq)
and normEq' (eq : preMorphism preEq) : (Data.path * Data.path * Data.eqT) m = 
  let* src = normMorphism eq.src in 
  let* dst = normMorphism eq.dst in 
  let* eqT = normEqT eq.shape in
  ret (src,dst,eqT)
and normEq (eq : preMorphism preEq) : Data.eq m =
  let* (src,dst,eqT) = normEq' eq in
  ret (makeEq src dst eqT)

let normFace (fce : preFace) : Data.face m =
  let* mphT = normMorphismT fce.tp in 
  let* (src,dst,eqT) = normEq' fce.obj in 
  getFace @<< initFace mphT src dst (makeEq src dst eqT)

