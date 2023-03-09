
module type Monad = sig
  type 'a m
  val bind : 'a m -> ('a -> 'b m) -> 'b m
  val return : 'a -> 'a m 
end

module Make(M : Monad) = struct

  open Data
  
  module IntMap = Map.Make(struct type t = int let compare = compare end)
  type 't store =
    { categories : 't categoryData array
    ; functors   : 't functData array 
    ; elems      : 't elemData array 
    ; morphisms  : 't morphismData array 
    ; faces      : 't eqData array
    ; evars      : 't option IntMap.t
    ; eqPred     : 't -> 't -> bool M.m
    }
  let emptyStore : 't store =
    { categories = [| |]
    ; functors   = [| |]
    ; elems      = [| |]
    ; morphisms  = [| |]
    ; faces      = [| |]
    ; evars      = IntMap.empty
    ; eqPred     = fun _ _ -> M.return false
    }
  
  type ('a,'t) t = 
    { runState : 't store -> ('a * 't store) M.m }
  
  
  (*  __  __                       _  *)
  (* |  \/  | ___  _ __   __ _  __| | *)
  (* | |\/| |/ _ \| '_ \ / _` |/ _` | *)
  (* | |  | | (_) | | | | (_| | (_| | *)
  (* |_|  |_|\___/|_| |_|\__,_|\__,_| *)
  (*                                  *)
  (* Monadic operations *)
  module Combinators = struct
    let ret x = { runState = fun st -> M.return (x,st) }
    let bind a f = { runState = fun st -> M.bind (a.runState st) (fun (a,st) -> (f a).runState st) }
    let (let*) = bind
    let (>>=)  = bind
    let (@<<) f a = bind a f
    let (<$>) f a = bind a (fun x -> ret (f x))
    let run m = M.bind (m.runState emptyStore) (fun (x,_) -> M.return x) 
    let lift (x : 'a M.m) : ('a,'t) t = { runState = fun st -> M.bind x (fun x -> M.return (x,st)) }
  end
  open Combinators
  
  
  let get (f : 't store -> 'a) : ('a,'t) t = { runState = fun st -> (ret (f st)).runState st }
  let set (f : 't store -> 't store) : (unit,'t) t = { runState = fun st -> (ret ()).runState (f st) }
  
  (*  ____                               _    *)
  (* / ___| _   _ _ __  _ __   ___  _ __| |_  *)
  (* \___ \| | | | '_ \| '_ \ / _ \| '__| __| *)
  (*  ___) | |_| | |_) | |_) | (_) | |  | |_  *)
  (* |____/ \__,_| .__/| .__/ \___/|_|   \__| *)
  (*             |_|   |_|                    *)
  (* Support *)
  let registerEqPredicate (eq : 't -> 't -> bool M.m) =
    set (fun st -> { st with eqPred = eq })
  
  
  (*  ____  _        _        *)
  (* / ___|| |_ __ _| |_ ___  *)
  (* \___ \| __/ _` | __/ _ \ *)
  (*  ___) | || (_| | ||  __/ *)
  (* |____/ \__\__,_|\__\___| *)
  (*                          *)
  (* State operations *)
  
  let push_back (arr : 'a array) (x : 'a) : 'a array = Array.append arr [| x |]
  let eqPred () = get (fun st -> fun x y ->
    match x, y with
    | Ctx (_,x), Ctx (_,y) -> st.eqPred x y
    | Evar (e1,_), Evar (e2,_) -> M.return (e1 = e2)
    | _ -> M.return false)

  let rec arr_find_optM' (id : int) (pred : 'a -> bool M.m) (arr : 'a array) : 'a option M.m =
    if id >= Array.length arr
    then M.return None
    else
      M.bind
        (pred arr.(id)) 
        (fun p -> if p then M.return (Some arr.(id)) else arr_find_optM' (id + 1) pred arr)

  let arr_find_optM pred (arr : 'a array) : ('a option,'t) t = lift (arr_find_optM' 0 pred arr)

  let mkPred () =
    get (fun st -> fun atom obj ->
      match atom with
      | Ctx (_, h) -> st.eqPred h obj
      | _ -> M.return false)

  let toId base i =
    if i mod 8 = base then Some (i / 8) else None
  let fromId base i =
    i * 8 + base
  
  let catToIndex = toId 0
  let catFromIndex = fromId 0
  let getCategories () : ('t categoryData array,'t) t = get (fun (st : 't store) -> st.categories)
  let getCategory i = match catToIndex i with
  | Some i -> get (fun st -> st.categories.(i))
  | None -> assert false
  let registerCategory ~cat =
    let* pred = mkPred () in
    let* id = arr_find_optM (fun c -> pred c.cat_atom cat) @<< getCategories () in 
    match id with
    | Some cat -> ret cat
    | None ->
        let* nid = catFromIndex <$> (Array.length <$> getCategories ()) in 
        let* _ = set (fun st -> 
          { st with categories = push_back st.categories 
            { cat_atom = Ctx (nid,cat) }}) in 
        getCategory nid
  
  let functorToIndex = toId 1
  let functorFromIndex = fromId 1
  let getFunctors () = get (fun st -> st.functors) 
  let getFunctor i = match functorToIndex i with
  | Some i -> get (fun st -> st.functors.(i))
  | None -> assert false
  let registerFunctor ~funct ~src ~dst =
    let* pred = mkPred () in 
    let* id = arr_find_optM (fun c -> pred c.funct_atom funct) @<< getFunctors () in 
    match id with
    | Some funct -> ret funct
    | None ->
        let* nid = functorFromIndex <$> (Array.length <$> getFunctors ()) in 
        let* _ = set (fun st ->
          { st with functors = push_back st.functors 
            { funct_atom = Ctx (nid,funct)
            ; funct_src_ = src; funct_dst_ = dst }}) in
        getFunctor nid
  
  let elemToIndex = toId 2
  let elemFromIndex = fromId 2
  let getElems () = get (fun st -> st.elems)
  let getElem i = match elemToIndex i with
  | Some i -> get (fun st -> st.elems.(i))
  | None -> assert false
  let registerElem ~elem ~cat =
    let* pred = mkPred () in 
    let* id = arr_find_optM (fun e -> pred e.elem_atom elem) @<< getElems () in 
    match id with
    | Some elem -> ret elem 
    | None ->
        let* nid = elemFromIndex <$> (Array.length <$> getElems ()) in 
        let* _ = set (fun st ->
          { st with elems = push_back st.elems 
            { elem_atom = Ctx (nid,elem)
            ; elem_cat_ = cat }}) in 
        getElem nid
  
  let mphToIndex = toId 3
  let mphFromIndex = fromId 3
  let getMorphisms () = get (fun st -> st.morphisms)
  let getMorphism i = match mphToIndex i with
  | Some i -> get (fun st -> st.morphisms.(i))
  | None -> assert false
  let registerMorphism ~mph ~cat ~src ~dst =
    let* pred = mkPred () in 
    let* id = arr_find_optM (fun m -> pred m.mph_atom mph) @<< getMorphisms () in 
    match id with 
    | Some mph -> ret mph
    | None ->
        let* nid = mphFromIndex <$> (Array.length <$> getMorphisms ()) in 
        let* _ = set (fun st ->
          { st with morphisms = push_back st.morphisms 
            { mph_atom = Ctx (nid,mph)
            ; mph_cat_ = cat; mph_src_ = src; mph_dst_ = dst
            ; mono = None; epi = None; iso = None }}) in
        getMorphism nid
  
  let eqToIndex = toId 4
  let eqFromIndex = fromId 4
  let getEqs () = get (fun st -> st.faces)
  let getEq i = match eqToIndex i with
  | Some i -> get (fun st -> st.faces.(i))
  | None -> assert false
  let registerEq ~eq ~right ~left ~cat ~src ~dst =
    let* pred = mkPred () in 
    let* id = arr_find_optM (fun e -> pred e.eq_atom eq) @<< getEqs () in
    match id with 
    | Some eq -> ret eq 
    | None -> 
        let* nid = eqFromIndex <$> (Array.length <$> getEqs ()) in 
        let* _ = set (fun st ->
          { st with faces = push_back st.faces 
            { eq_atom = Ctx (nid,eq)
            ; eq_left_ = left; eq_right_ = right 
            ; eq_cat_ = cat; eq_src_ = src; eq_dst_ = dst }}) in
        getEq nid

  type 't evar =
    | Abstract
    | Realized of 't
    | NotFound
  let getEvar i = get (fun st -> match IntMap.find_opt i st.evars with
        | Some (Some ev) -> Realized ev
        | Some None -> Abstract
        | None -> NotFound)
  let newEvar () =
    let* empty = get (fun st -> IntMap.is_empty st.evars) in
    if empty
    then ret 0
    else
      let* nid = get (fun st -> fst (IntMap.find_last (fun _ -> true) st.evars) + 1) in
      let* _ = set (fun st -> { st with evars = IntMap.add nid None st.evars }) in
      ret nid
  let newEvarAt nid =
    set (fun st -> { st with evars = IntMap.add nid None st.evars })
  let instantiateEvar i evar =
    set (fun st -> { st with evars = IntMap.add i (Some evar) st.evars })
end
