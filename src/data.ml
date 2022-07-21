
type kind = (EConstr.t,EConstr.t,EConstr.ESorts.t,EConstr.EInstance.t) Constr.kind_of_term

type cat_id  = int
type elem_id = int
type mph_id  = int
type face_id = int

type category =
  { obj : EConstr.t
  ; id  : cat_id
  }
type elem =
  { obj      : EConstr.t
  ; category : category
  ; id       : elem_id
  }
type morphismT =
  { category : category
  ; src      : elem
  ; dst      : elem
  ; obj      : EConstr.t
  }
type morphismData =
  { obj : EConstr.t
  ; tp  : morphismT
  }
type morphismBase =
  { data : morphismData
  ; id   : mph_id
  }
type isoData =
  { obj : EConstr.t
  ; mph : morphismBase
  ; inv : morphismBase
  }
type epiData =
  { obj : EConstr.t
  ; mph : morphismBase
  }
type monoData =
  { obj : EConstr.t 
  ; mph : morphismBase 
  }
type morphismShape =
  | Base of morphismBase
type morphism =
  { data  : morphismData
  ; shape : morphismShape
  }

let fromBase (m : morphismBase) : morphism =
  { data = m.data; shape = Base m; }

type eqT =
  | Refl of morphismData
  | Concat of eq * eq
  | Inv of eq
  | Compose of eq * eq
  | Assoc of morphismData * morphismData * morphismData
  | LeftId of morphismData
  | RightId of morphismData
  | RAp of eq * morphismData
  | LAp of morphismData * eq
  | RInv of isoData
  | LInv of isoData
  | Mono of EConstr.t * morphismData * morphismData * eq
  | Epi of EConstr.t * morphismData * morphismData * eq
  | Atom of EConstr.t
and eq =
  { src : morphismData
  ; dst : morphismData
  ; tp  : morphismT
  ; eq  : eqT
  }

type path =
  { mph  : morphismData
  ; eq   : eq
  ; path : morphism list
  }
type face =
  { tp    : morphismT
  ; side1 : path
  ; side2 : path
  ; obj   : eq
  ; id    : face_id
  }

let comp_category (cmp : EConstr.t -> EConstr.t -> bool) (x1 : category) (x2 : category) : bool =
  x1.id == x2.id

let comp_elem (cmp : EConstr.t -> EConstr.t -> bool) (x1 : elem) (x2 : elem) : bool =
  x1.id == x2.id

let comp_morphismT (cmp : EConstr.t -> EConstr.t -> bool) (x1 : morphismT) (x2 : morphismT) : bool =
  comp_category cmp x1.category x2.category
  && comp_elem cmp x1.src x2.src
  && comp_elem cmp x1.dst x2.dst
  && cmp x1.obj x2.obj

let comp_morphismData (cmp : EConstr.t -> EConstr.t -> bool) (x1 : morphismData) (x2 : morphismData) : bool =
  cmp x1.obj x2.obj

let comp_morphismBase (cmp : EConstr.t -> EConstr.t -> bool) (x1 : morphismBase) (x2 : morphismBase) : bool =
  x1.id == x2.id

let comp_isoData (cmp : EConstr.t -> EConstr.t -> bool) (x1 : isoData) (x2 : isoData) : bool =
  cmp x1.obj x2.obj
let comp_epiData (cmp : EConstr.t -> EConstr.t -> bool) (x1 : epiData) (x2 : epiData) : bool =
  cmp x1.obj x2.obj
let comp_monoData (cmp : EConstr.t -> EConstr.t -> bool) (x1 : monoData) (x2 : monoData) : bool =
  cmp x1.obj x2.obj

let comp_morphismShape (cmp : EConstr.t -> EConstr.t -> bool) (x1 : morphismShape) (x2 : morphismShape) : bool =
  match x1, x2 with
  | Base x1, Base x2 -> comp_morphismBase cmp x1 x2

let comp_morphism (cmp : EConstr.t -> EConstr.t -> bool) (x1 : morphism) (x2 : morphism) : bool =
  comp_morphismData cmp x1.data x2.data

let rec comp_eqT (cmp : EConstr.t -> EConstr.t -> bool) (x1 : eqT) (x2 : eqT) : bool =
  match x1, x2 with
  | Refl m1, Refl m2 -> comp_morphismData cmp m1 m2 
  | Concat (eq1,eq1'), Concat (eq2,eq2') -> comp_eq cmp eq1 eq2 && comp_eq cmp eq1' eq2'
  | Inv eq1, Inv eq2 -> comp_eq cmp eq1 eq2
  | Compose (eq1,eq1'), Compose (eq2,eq2') -> comp_eq cmp eq1 eq2 && comp_eq cmp eq1' eq2'
  | Assoc (m1,m1',m1''), Assoc (m2,m2',m2'') -> comp_morphismData cmp m1 m2 
                                             && comp_morphismData cmp m1' m2' 
                                             && comp_morphismData cmp m1'' m2''
  | LeftId m1, LeftId m2 -> comp_morphismData cmp m1 m2
  | RightId m1, RightId m2 -> comp_morphismData cmp m1 m2
  | RAp (eq1,m1), RAp (eq2,m2) -> comp_morphismData cmp m1 m2 && comp_eq cmp eq1 eq2
  | LAp (m1,eq1), LAp (m2,eq2) -> comp_morphismData cmp m1 m2 && comp_eq cmp eq1 eq2
  | RInv iso1, RInv iso2 -> comp_isoData cmp iso1 iso2
  | LInv iso1, LInv iso2 -> comp_isoData cmp iso1 iso2
  | Mono (mono1,_,_,eq1), Mono (mono2,_,_,eq2) -> cmp mono1 mono2 && comp_eq cmp eq1 eq2
  | Epi (epi1,_,_,eq1), Mono (epi2,_,_,eq2) -> cmp epi1 epi2 && comp_eq cmp eq1 eq2
  | Atom eq1, Atom eq2 -> cmp eq1 eq2
  | _, _ -> false
and comp_eq (cmp : EConstr.t -> EConstr.t -> bool) (x1 : eq) (x2 : eq) : bool =
  comp_eqT cmp x1.eq x2.eq

let comp_path (cmp : EConstr.t -> EConstr.t -> bool) (x1 : path) (x2 : path) : bool =
  comp_morphismData cmp x1.mph x2.mph
let comp_face (cmp : EConstr.t -> EConstr.t -> bool) (x1 : face) (x2 : face) : bool =
  comp_eq cmp x1.obj x2.obj
