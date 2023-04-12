
open Data

(* Masked elements are not included in the goal graph sent to the engine *)
module IntMap = Map.Make(struct type t = int let compare = compare end)
type store =
  { categories : categoryData array
  ; functors   : functData array 
  ; elems      : (elemData*bool) array 
  ; morphisms  : (morphismData*bool) array 
  ; faces      : (eqData*bool) array
  ; funs       : Data.fn array
  ; evars      : EConstr.t option IntMap.t
  }
let emptyStore : store =
  { categories = [| |]
  ; functors   = [| |]
  ; elems      = [| |]
  ; morphisms  = [| |]
  ; faces      = [| |]
  ; funs       = [| |]
  ; evars      = IntMap.empty
  }

type 'a t = 
  { runState : Environ.env -> bool -> store -> ('a * store) Proofview.tactic }


(*  __  __                       _  *)
(* |  \/  | ___  _ __   __ _  __| | *)
(* | |\/| |/ _ \| '_ \ / _` |/ _` | *)
(* | |  | | (_) | | | | (_| | (_| | *)
(* |_|  |_|\___/|_| |_|\__,_|\__,_| *)
(*                                  *)
(* Monadic operations *)
module Combinators = struct
  let m_ret = Proofview.tclUNIT
  let m_bind = Proofview.tclBIND
  let ret x = { runState = fun _ _ st -> m_ret (x,st) }
  let bind a f = 
    { runState = fun env mask st -> 
        m_bind 
          (a.runState env mask st) 
          (fun (a,st) -> (f a).runState env mask st) }
  let (let*) = bind
  let (>>=)  = bind
  let (@<<) f a = bind a f
  let (<$>) f a = bind a (fun x -> ret (f x))

  let run env m = m_bind (m.runState env false emptyStore) (fun (x,_) -> m_ret x) 
  let lift (x : 'a Proofview.tactic) : 'a t = { runState = fun _ _ st -> m_bind x (fun x -> m_ret (x,st)) }

  let env () = { runState = fun env _ st -> m_ret (env,st) }
  let evars () = lift Proofview.tclEVARMAP
  let masked () = { runState = fun _ mask st -> m_ret (mask,st) }
  let none () = ret None
  let some x = ret (Some x)
  let print ec =
    let* env = env () in
    let* sigma = evars () in
    let pp = Printer.pr_econstr_env env sigma ec in
    ret (Pp.string_of_ppcmds pp)
  let fail msg =
    msg |> Pp.str |> Tacticals.tclFAIL |> lift
  let message msg =
    Feedback.msg_info (Pp.str msg); ret ()
  let warning msg =
    Feedback.msg_warning (Pp.str msg); ret ()
end
open Combinators


let get (f : store -> 'a) : 'a t = 
  { runState = fun env mask st -> (ret (f st)).runState env mask st }
let set (f : store -> store) : unit t = 
  { runState = fun env mask st -> (ret ()).runState env mask (f st) }

(*  ____  _        _        *)
(* / ___|| |_ __ _| |_ ___  *)
(* \___ \| __/ _` | __/ _ \ *)
(*  ___) | || (_| | ||  __/ *)
(* |____/ \__\__,_|\__\___| *)
(*                          *)
(* State operations *)

let push_back (arr : 'a array) (x : 'a) : 'a array = Array.append arr [| x |]
let eqPred x y =
  let* sigma = evars () in
  ret (EConstr.eq_constr sigma x y)
  (* TODO Reductionops.check_conv fails under quantifiers for some reason *)
  (* ret (Reductionops.check_conv env sigma x y) *)

let rec arr_find_optM' (id : int) (pred : 'a -> bool t) (arr : 'a array) : (int*'a) option t =
  if id >= Array.length arr
  then ret None
  else
    let* p = pred arr.(id) in
    if p 
    then some (id,arr.(id))
    else arr_find_optM' (id + 1) pred arr

let arr_find_optM pred (arr : 'a array) : (int*'a) option t = 
  arr_find_optM' 0 pred arr

let mkPred atom obj =
  let pred = eqPred in
  match atom with
  | Ctx (_, h) -> pred h obj
  | _ -> ret false

let toId base i =
  if i mod 8 = base then Some (i / 8) else None
let fromId base i =
  i * 8 + base

let getAtom id =
  match id mod 8 with
  | 0 -> get (fun st -> Some st.categories.(id/8).cat_atom)
  | 1 -> get (fun st -> Some st.functors.(id/8).funct_atom)
  | 2 -> get (fun st -> Some (fst st.elems.(id/8)).elem_atom)
  | 3 -> get (fun st -> Some (fst st.morphisms.(id/8)).mph_atom)
  | 4 -> get (fun st -> Some (fst st.faces.(id/8)).eq_atom)
  | _ -> ret None

let withMask mask act = { runState = fun env _ st -> act.runState env mask st }
let withEnv env act = { runState = fun _ mask st -> act.runState env mask st }

let catToIndex = toId 0
let catFromIndex = fromId 0
let getCategories () : categoryData array t = get (fun (st : store) -> st.categories)
let getCategory i = match catToIndex i with
| Some i -> get (fun st -> st.categories.(i))
| None -> assert false
let registerCategory ~cat =
  let* id = arr_find_optM (fun c -> mkPred c.cat_atom cat) @<< getCategories () in 
  match id with
  | Some (_,cat) -> ret cat
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
  let* id = arr_find_optM (fun c -> mkPred c.funct_atom funct) @<< getFunctors () in 
  match id with
  | Some (_,funct) -> ret funct
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
| Some i -> get (fun st -> fst st.elems.(i))
| None -> assert false
let getElemMask i = match elemToIndex i with
| Some i -> get (fun st -> snd st.elems.(i))
| None -> assert false
let registerElem ~elem ~cat =
  let* id = arr_find_optM (fun e -> mkPred (fst e).elem_atom elem) @<< getElems () in 
  match id with
  | Some (_,(elem,_)) -> ret elem 
  | None ->
      let* nid = elemFromIndex <$> (Array.length <$> getElems ()) in 
      let* mask = masked () in
      let* _ = set (fun st ->
        { st with elems = push_back st.elems 
          ( { elem_atom = Ctx (nid,elem)
            ; elem_cat_ = cat }
          , mask) }) in 
      getElem nid

let mphToIndex = toId 3
let mphFromIndex = fromId 3
let getMorphisms () = get (fun st -> st.morphisms)
let getMorphism i = match mphToIndex i with
| Some i -> get (fun st -> fst st.morphisms.(i))
| None -> assert false
let getMorphismMask i = match mphToIndex i with
| Some i -> get (fun st -> snd st.morphisms.(i))
| None -> assert false
let registerMorphism ~mph ~cat ~src ~dst =
  let* id = arr_find_optM (fun m -> mkPred (fst m).mph_atom mph) @<< getMorphisms () in 
  match id with 
  | Some (_,(mph,_)) -> ret mph
  | None ->
      let* nid = mphFromIndex <$> (Array.length <$> getMorphisms ()) in 
      let* mask = masked () in
      let* _ = set (fun st ->
        { st with morphisms = push_back st.morphisms 
          ( { mph_atom = Ctx (nid,mph)
            ; mph_cat_ = cat; mph_src_ = src; mph_dst_ = dst
            ; mono = None; epi = None; iso = None }
          , mask )}) in
      getMorphism nid

let eqToIndex = toId 4
let eqFromIndex = fromId 4
let getEqs () = get (fun st -> st.faces)
let getEq i = match eqToIndex i with
| Some i -> get (fun st -> fst st.faces.(i))
| None -> assert false
let getEqMask i = match eqToIndex i with
| Some i -> get (fun st -> snd st.faces.(i))
| None -> assert false
let registerEq ~eq ~right ~left ~cat ~src ~dst =
  let* id = arr_find_optM (fun e -> mkPred (fst e).eq_atom eq) @<< getEqs () in
  match id with 
  | Some (_,(eq,_)) -> ret eq 
  | None -> 
      let* nid = eqFromIndex <$> (Array.length <$> getEqs ()) in 
      let* mask = masked () in
      let* _ = set (fun st ->
        { st with faces = push_back st.faces 
          ( { eq_atom = Ctx (nid,eq)
            ; eq_left_ = left; eq_right_ = right 
            ; eq_cat_ = cat; eq_src_ = src; eq_dst_ = dst }
          , mask )}) in
      getEq nid

let funToIndex = toId 5
let funFromIndex = fromId 5
let getFuns () = get (fun st -> st.funs)
let getFun i = match  funToIndex i with
| Some i -> get (fun st -> st.funs.(i))
| None -> assert false
let registerFun ~fn =
  let eqFn fn1 fn2 =
    let open Data in
    begin match fn1, fn2 with
    | FnConst cst1, FnConst cst2 -> Names.Constant.CanOrd.equal cst1 cst2
    | FnVar id1, FnVar id2 -> Names.Id.equal id1 id2
    | FnInd (ind1,id1), FnInd (ind2,id2) -> Names.MutInd.CanOrd.equal ind1 ind2 && id1 = id2
    | FnConstr ((ind1,id1),c1), FnConstr ((ind2,id2),c2) ->
        Names.MutInd.CanOrd.equal ind1 ind2 && id1 = id2 && c1 = c2
    | FnProj p1, FnProj p2 -> Names.Projection.CanOrd.equal p1 p2
    | _ -> false end |> ret in
  let* id = arr_find_optM (eqFn fn) @<< getFuns () in
  match id with
  | Some (id,_) -> funFromIndex id |> ret
  | None ->
      let* nid = funFromIndex <$> (Array.length <$> getFuns ()) in
      let* _ = set (fun st ->
        { st with funs = push_back st.funs fn }) in
      ret nid

type evar =
  | Abstract
  | Realized of EConstr.t
  | NotFound
let getEvar i = get (fun st -> match IntMap.find_opt i st.evars with
      | Some (Some ev) -> Realized ev
      | Some None -> Abstract
      | None -> NotFound)
let newEvar () =
  let* empty = get (fun st -> IntMap.is_empty st.evars) in
  if empty
  then 
    let* _ = set (fun st -> { st with evars = IntMap.add 0 None st.evars }) in
    ret 0
  else
    let* nid = get (fun st -> fst (IntMap.find_last (fun _ -> true) st.evars) + 1) in
    let* _ = set (fun st -> { st with evars = IntMap.add nid None st.evars }) in
    ret nid
let newEvarAt nid =
  set (fun st -> { st with evars = IntMap.add nid None st.evars })
let instantiateEvar i evar =
  set (fun st -> { st with evars = IntMap.add i (Some evar) st.evars })
