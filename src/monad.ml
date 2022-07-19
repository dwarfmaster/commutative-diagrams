
type state =
  { categories : Data.category array
  ; elems      : Data.elem array
  ; morphisms  : Data.morphismBase array
  ; faces      : Data.face array
  ; env        : Environ.env
  }

(* State monad over Proofview.tactic *)
(* state might be mutated, so binding must always be linear *)
type 'a t = state -> (state * 'a) Proofview.tactic 
let run (goal : Proofview.Goal.t) (m : 'a t) =
  let env = Proofview.Goal.env goal in
  let tac = m { categories = [| |]
              ; elems      = [| |]
              ; morphisms  = [| |]
              ; faces      = [| |]
              ; env        = env
              } in
  Proofview.tclBIND tac (fun (_,x) -> Proofview.tclUNIT x)

let onState : (state -> 'a) -> 'a t =
  fun f -> fun st -> let x = f st in Proofview.tclUNIT (st,x)
let upState : (state -> state) -> unit t =
  fun f -> fun st -> Proofview.tclUNIT (f st, ())
let mutateState : (state -> unit) -> unit t =
  fun f -> fun st -> let _ = f st in Proofview.tclUNIT (st, ())
let push_back : 'a array -> 'a -> 'a array =
  fun arr x ->
    let len = Array.length arr in
    Array.init len (fun i -> if i < len then Array.get arr i else x)

(*  __  __                       _  *)
(* |  \/  | ___  _ __   __ _  __| | *)
(* | |\/| |/ _ \| '_ \ / _` |/ _` | *)
(* | |  | | (_) | | | | (_| | (_| | *)
(* |_|  |_|\___/|_| |_|\__,_|\__,_| *)
(*                                  *)
(* Monad *)

let bind : 'a t -> ('a -> 'b t) -> 'b t =
  fun x f ->
    fun st ->
      Proofview.tclBIND (x st) (fun (st,x) -> f x st)

let thn  : 'a t -> 'b t -> 'b t =
  fun x y -> bind x (fun _ -> y)

let ret  : 'a -> 'a t =
  fun x -> fun st -> Proofview.tclUNIT (st,x)

let (let*) = bind
let (>>=)  = bind
let (@<<)  = fun f x -> bind x f
let (<$>)  = fun f x -> bind x (fun x -> ret (f x))


(*  ____                   __                              _   _                  *)
(* |  _ \ _ __ ___   ___  / _|   ___  _ __   ___ _ __ __ _| |_(_) ___  _ __  ___  *)
(* | |_) | '__/ _ \ / _ \| |_   / _ \| '_ \ / _ \ '__/ _` | __| |/ _ \| '_ \/ __| *)
(* |  __/| | | (_) | (_) |  _| | (_) | |_) |  __/ | | (_| | |_| | (_) | | | \__ \ *)
(* |_|   |_|  \___/ \___/|_|    \___/| .__/ \___|_|  \__,_|\__|_|\___/|_| |_|___/ *)
(*                                   |_|                                          *)
(* Proof operations *)
let liftTactic : 'a Proofview.tactic -> 'a t =
  fun tac -> fun st -> Proofview.tclBIND tac (fun x -> Proofview.tclUNIT (st,x))
let getEnv = fun st -> Proofview.tclUNIT (st, st.env)
let getEvarMap = liftTactic Proofview.tclEVARMAP


(*  ____  _        _                                    _   _                  *)
(* / ___|| |_ __ _| |_ ___    ___  _ __   ___ _ __ __ _| |_(_) ___  _ __  ___  *)
(* \___ \| __/ _` | __/ _ \  / _ \| '_ \ / _ \ '__/ _` | __| |/ _ \| '_ \/ __| *)
(*  ___) | || (_| | ||  __/ | (_) | |_) |  __/ | | (_| | |_| | (_) | | | \__ \ *)
(* |____/ \__\__,_|\__\___|  \___/| .__/ \___|_|  \__,_|\__|_|\___/|_| |_|___/ *)
(*                                |_|                                          *)
(* State operations *)
let array_find_id : ('a -> bool) -> 'a array -> int option = fun pred arr ->
  let result : int option ref = ref None in 
  for i = 0 to Array.length arr - 1 do 
    if pred arr.(i) then result := Some i else ()
  done;
  !result

let comp_constr : Environ.env -> Evd.evar_map -> EConstr.t -> EConstr.t -> bool =
  fun env sigma e1 e2 -> Reductionops.check_conv env sigma e1 e2

let getCategories = onState (fun st -> st.categories)
let getCategory (i : int) = onState (fun st -> Array.get st.categories i)
let addCategory (cat : Data.category) =
  upState (fun st -> { st with categories = push_back st.categories cat })
let initCategory (cat : EConstr.t) =
  let* env = getEnv in 
  let* sigma = getEvarMap in 
  let* id = array_find_id (fun (c : Data.category) -> comp_constr env sigma cat c.obj) <$> getCategories in
  match id with
  | Some id -> ret id 
  | None ->
      let* nid = Array.length <$> getCategories in
      let* _ = addCategory { obj = cat; id = nid } in 
      ret nid

let getElems = onState (fun st -> st.elems)
let getElem (i : int) = onState (fun st -> Array.get st.elems i)
let addElem (elem : Data.elem) =
  upState (fun st -> { st with elems = push_back st.elems elem })
let initElem (cat : EConstr.t) (elem : EConstr.t) =
  let* env = getEnv in 
  let* sigma = getEvarMap in 
  let* cat = getCategory @<< initCategory cat in
  let* id = array_find_id (fun (e : Data.elem) -> comp_constr env sigma elem e.obj) <$> getElems in
  match id with
  | Some id -> ret id 
  | None ->
      let* nid = Array.length <$> getElems in 
      let* _ = addElem { obj = elem; id = nid; category = cat } in
      ret nid

let getMorphisms = onState (fun st -> st.morphisms)
let getMorphism (i : int) = onState (fun st -> Array.get st.morphisms i)
let addMorphism (mph : Data.morphismBase) =
  upState (fun st -> { st with morphisms = push_back st.morphisms mph })
let initMorphism (mph : Data.morphismData) =
  let* env = getEnv in 
  let* sigma = getEvarMap in 
  let* id =
    array_find_id (fun (m : Data.morphismBase) -> comp_constr env sigma mph.obj m.data.obj)
              <$> getMorphisms in
  match id with
  | Some id -> ret id
  | None ->
      let* nid = Array.length <$> getMorphisms in 
      let* _ = addMorphism { data = mph; id = nid; mono = None; epi = None; iso = None } in 
      ret nid

let getFaces = onState (fun st -> st.faces)
let getFace (i : int) = onState (fun st -> Array.get st.faces i)
let addFace (fce : Data.face) =
  upState (fun st -> { st with faces = push_back st.faces fce })
(* TODO *)
let initFace (tp : Data.morphismT) (mph1 : EConstr.t) (mph2 : EConstr.t) (fce : EConstr.t) =
  assert false
