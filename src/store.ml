
module type Monad = sig
  type 'a m
  val bind : 'a m -> ('a -> 'b m) -> 'b m
  val return : 'a -> 'a m 
end

module Make(M : Monad) = struct

  open Data
  
  type 't store =
    { categories : 't categoryData array
    ; functors   : 't functData array 
    ; elems      : 't elemData array 
    ; morphisms  : 't morphismData array 
    ; faces      : 't eqData array
    ; eqPred     : 't -> 't -> bool M.m
    }
  let emptyStore : 't store =
    { categories = [| |]
    ; functors   = [| |]
    ; elems      = [| |]
    ; morphisms  = [| |]
    ; faces      = [| |]
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
  let eqPred () = get (fun st -> st.eqPred)

  let rec arr_find_optM' (id : int) (pred : 'a -> bool M.m) (arr : 'a array) : 'a option M.m =
    if id >= Array.length arr
    then M.return None
    else
      M.bind
        (pred arr.(id)) 
        (fun p -> if p then M.return (Some arr.(id)) else arr_find_optM' (id + 1) pred arr)

  let arr_find_optM pred (arr : 'a array) : ('a option,'t) t = lift (arr_find_optM' 0 pred arr)
  
  let getCategories () : ('t categoryData array,'t) t = get (fun (st : 't store) -> st.categories)
  let getCategory i = get (fun st -> st.categories.(i))
  let registerCategory ~cat =
    let* pred = eqPred () in
    let* id = arr_find_optM (fun c -> pred c.cat_obj cat) @<< getCategories () in 
    match id with
    | Some cat -> ret cat
    | None ->
        let* nid = Array.length <$> getCategories () in 
        let* _ = set (fun st -> 
          { st with categories = push_back st.categories 
            { cat_obj = cat; cat_id = nid }}) in 
        getCategory nid
  
  let getFunctors () = get (fun st -> st.functors) 
  let getFunctor i = get (fun st -> st.functors.(i))
  let registerFunctor ~funct ~src ~dst =
    let* pred = eqPred () in 
    let* id = arr_find_optM (fun c -> pred c.funct_obj funct) @<< getFunctors () in 
    match id with
    | Some funct -> ret funct
    | None ->
        let* nid = Array.length <$> getFunctors () in 
        let* _ = set (fun st ->
          { st with functors = push_back st.functors 
            { funct_obj = funct; funct_id = nid
            ; funct_src_ = src; funct_dst_ = dst }}) in
        getFunctor nid
  
  let getElems () = get (fun st -> st.elems)
  let getElem i = get (fun st -> st.elems.(i))
  let registerElem ~elem ~cat =
    let* pred = eqPred () in 
    let* id = arr_find_optM (fun e -> pred e.elem_obj elem) @<< getElems () in 
    match id with
    | Some elem -> ret elem 
    | None ->
        let* nid = Array.length <$> getElems () in 
        let* _ = set (fun st ->
          { st with elems = push_back st.elems 
            { elem_obj = elem; elem_id = nid
            ; elem_cat_ = cat }}) in 
        getElem nid
  
  let getMorphisms () = get (fun st -> st.morphisms)
  let getMorphism i = get (fun st -> st.morphisms.(i))
  let registerMorphism ~mph ~cat ~src ~dst =
    let* pred = eqPred () in 
    let* id = arr_find_optM (fun m -> pred m.mph_obj mph) @<< getMorphisms () in 
    match id with 
    | Some mph -> ret mph
    | None ->
        let* nid = Array.length <$> getMorphisms () in 
        let* _ = set (fun st ->
          { st with morphisms = push_back st.morphisms 
            { mph_obj = mph; mph_id = nid
            ; mph_cat_ = cat; mph_src_ = src; mph_dst_ = dst
            ; mono = None; epi = None; iso = None }}) in
        getMorphism nid
  
  let getEqs () = get (fun st -> st.faces)
  let getEq i = get (fun st -> st.faces.(i))
  let registerEq ~eq ~right ~left ~cat ~src ~dst =
    let* pred = eqPred () in 
    let* id = arr_find_optM (fun e -> pred e.eq_obj eq) @<< getEqs () in
    match id with 
    | Some eq -> ret eq 
    | None -> 
        let* nid = Array.length <$> getEqs () in 
        let* _ = set (fun st ->
          { st with faces = push_back st.faces 
            { eq_obj = eq; eq_id = nid
            ; eq_left_ = left; eq_right_ = right 
            ; eq_cat_ = cat; eq_src_ = src; eq_dst_ = dst }}) in
        getEq nid

end
