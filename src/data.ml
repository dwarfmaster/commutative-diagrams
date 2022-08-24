
type kind = (EConstr.t,EConstr.t,EConstr.ESorts.t,EConstr.EInstance.t) Constr.kind_of_term

type cat_id   = int
type funct_id = int
type elem_id  = int
type mph_id   = int
type face_id  = int

type category =
  { obj : EConstr.t
  ; id  : cat_id
  }
type funct =
  { obj : EConstr.t 
  ; tp  : EConstr.t
  ; id  : funct_id
  ; src : category
  ; dst : category
  }
type internedElem =
  { obj      : EConstr.t
  ; category : category
  ; id       : elem_id
  }
type elem =
  | Elem of internedElem
  | FObj of funct * elem
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
type isoData =
  { obj : EConstr.t
  ; mph : morphism
  ; inv : morphism
  }
and morphism =
  { data : morphismData
  ; id   : mph_id
  ; mutable mono : EConstr.t option
  ; mutable epi  : EConstr.t option
  ; mutable iso  : isoData option
  }

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

type ('morphism,'path) pathComponent =
  | Base of 'morphism
  | Functor of funct * 'path
type path =
  { mph  : morphismData
  ; eq   : eq (* Equality from `mph` to `realize path` *)
  ; path : (morphism,path) pathComponent list
  }
type pathSkeleton = elem * ((morphismData,pathSkeleton) pathComponent list)
type face =
  { tp    : morphismT
  ; side1 : path
  ; side2 : path
  ; obj   : eq
  ; id    : face_id
  }

let rec toSkeleton (path : path) : pathSkeleton =
  (path.mph.tp.src, List.map toSkeletonComp path.path)
and toSkeletonComp (comp : (morphism,path) pathComponent) : (morphismData,pathSkeleton) pathComponent =
  match comp with
  | Base m -> Base m.data
  | Functor (f,p) -> Functor (f,toSkeleton p)

  let elemCategory (e : elem) =
    match e with
    | Elem e -> e.category 
    | FObj (f,_) -> f.dst
