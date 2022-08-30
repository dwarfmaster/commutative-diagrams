
open Data
type store =
  { categories : categoryData array
  ; functors   : functData array 
  ; elems      : elemData array 
  ; morphisms  : morphismData array 
  ; faces      : eqData array
  ; eqPred     : Data.PA.t -> Data.PA.t -> bool
  }
let emptyStore : store =
  { categories = [| |]
  ; functors   = [| |]
  ; elems      = [| |]
  ; morphisms  = [| |]
  ; faces      = [| |]
  ; eqPred     = fun _ _ -> false
  }

type 'a t = store -> 'a * store


(*  __  __                       _  *)
(* |  \/  | ___  _ __   __ _  __| | *)
(* | |\/| |/ _ \| '_ \ / _` |/ _` | *)
(* | |  | | (_) | | | | (_| | (_| | *)
(* |_|  |_|\___/|_| |_|\__,_|\__,_| *)
(*                                  *)
(* Monadic operations *)
let ret x st = (x,st)
let bind a f st = let (a,st) = a st in f a st
let (let*) = bind
let (>>=)  = bind
let (@<<) f a = bind a f
let (<$>) f a = bind a (fun x -> ret (f x))
let run m = fst (m emptyStore) 


let get f = fun st -> ret (f st) st
let set f = fun st -> ret () (f st)

(*  ____                               _    *)
(* / ___| _   _ _ __  _ __   ___  _ __| |_  *)
(* \___ \| | | | '_ \| '_ \ / _ \| '__| __| *)
(*  ___) | |_| | |_) | |_) | (_) | |  | |_  *)
(* |____/ \__,_| .__/| .__/ \___/|_|   \__| *)
(*             |_|   |_|                    *)
(* Support *)
let registerEqPredicate (eq : Data.PA.t -> Data.PA.t -> bool) =
  set (fun st -> { st with eqPred = eq })


(*  ____  _        _        *)
(* / ___|| |_ __ _| |_ ___  *)
(* \___ \| __/ _` | __/ _ \ *)
(*  ___) | || (_| | ||  __/ *)
(* |____/ \__\__,_|\__\___| *)
(*                          *)
(* State operations *)

let push_back (arr : 'a array) (x : 'a) : 'a array = Array.append arr [| x |]
let eqPred = get (fun st -> st.eqPred)

let getCategories = get (fun st -> st.categories)
let getCategory i = get (fun st -> st.categories.(i))
let registerCategory (cat : Data.PA.t) =
  let* pred = eqPred in
  let* id = Array.find_opt (fun c -> pred c.cat_obj cat) <$> getCategories in 
  match id with
  | Some cat -> ret cat
  | None ->
      let* nid = Array.length <$> getCategories in 
      let* _ = set (fun st -> 
        { st with categories = push_back st.categories 
          { cat_obj = cat; cat_id = nid }}) in 
      getCategory nid

let getFunctors = get (fun st -> st.functors) 
let getFunctor i = get (fun st -> st.functors.(i))
let registerFunctor ~funct ~src ~dst =
  let* pred = eqPred in 
  let* id = Array.find_opt (fun c -> pred c.funct_obj funct) <$> getFunctors in 
  match id with
  | Some funct -> ret funct
  | None ->
      let* nid = Array.length <$> getFunctors in 
      let* _ = set (fun st ->
        { st with functors = push_back st.functors 
          { funct_obj = funct; funct_id = nid
          ; funct_src_ = src; funct_dst_ = dst }}) in
      getFunctor nid

let getElems = get (fun st -> st.elems)
let getElem i = get (fun st -> st.elems.(i))
let registerElem ~elem ~cat =
  let* pred = eqPred in 
  let* id = Array.find_opt (fun e -> pred e.elem_obj elem) <$> getElems in 
  match id with
  | Some elem -> ret elem 
  | None ->
      let* nid = Array.length <$> getElems in 
      let* _ = set (fun st ->
        { st with elems = push_back st.elems 
          { elem_obj = elem; elem_id = nid
          ; elem_cat_ = cat }}) in 
      getElem nid

let getMorphisms = get (fun st -> st.morphisms)
let getMorphism i = get (fun st -> st.morphisms.(i))
let registerMorphism ~mph ~cat ~src ~dst =
  let* pred = eqPred in 
  let* id = Array.find_opt (fun m -> pred m.mph_obj mph) <$> getMorphisms in 
  match id with 
  | Some mph -> ret mph
  | None ->
      let* nid = Array.length <$> getMorphisms in 
      let* _ = set (fun st ->
        { st with morphisms = push_back st.morphisms 
          { mph_obj = mph; mph_id = nid
          ; mph_cat_ = cat; mph_src_ = src; mph_dst_ = dst
          ; mono = None; epi = None; iso = None }}) in
      getMorphism nid

let getEqs = get (fun st -> st.faces)
let getEq i = get (fun st -> st.faces.(i))
let registerEq ~eq ~right ~left ~cat ~src ~dst =
  let* pred = eqPred in 
  let* id = Array.find_opt (fun e -> pred e.eq_obj eq) <$> getEqs in
  match id with 
  | Some eq -> ret eq 
  | None -> 
      let* nid = Array.length <$> getEqs in 
      let* _ = set (fun st ->
        { st with faces = push_back st.faces 
          { eq_obj = eq; eq_id = nid
          ; eq_left_ = left; eq_right_ = right 
          ; eq_cat_ = cat; eq_src_ = src; eq_dst_ = dst }}) in
      getEq nid
