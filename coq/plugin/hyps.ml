
type obj =
  { namespace: int
  ; id: int
  }
let compare_obj o1 o2 =
  List.compare Int.compare [o1.namespace;o1.id] [o2.namespace;o2.id]
type metadata =
  { is_cat: unit option
  ; is_funct: (obj * obj) option
  ; is_elem: obj option
  ; is_mph: (obj * obj * obj) option
  ; is_eq: (obj * obj * obj * obj * obj) option
  }
type obj_impl =
  { value: EConstr.t
  ; tp: EConstr.t
  ; name: string option
  ; mask: bool
  ; mtdt: metadata
  }

(* Masked elements are not included in the goal graph sent to the engine *)
module IntMap = Map.Make(struct type t = int let compare = compare end)
type state =
  { ctx_stack  : Evd.evar_map list
  ; objects    : obj_impl array array
  }
let emptyState sigma : state =
  { ctx_stack  = [ sigma ]
  ; objects    = [| [| |] |]
  }

type 'a t = 
  { runState : Environ.env -> bool -> state -> ('a * state) Proofview.tactic }
let ret x = { runState = fun _ _ st -> Proofview.tclUNIT (x,st) }
let get (f : state -> 'a) : 'a t = 
  { runState = fun env mask st -> (ret (f st)).runState env mask st }
let set (f : state -> state) : unit t = 
  { runState = fun env mask st -> (ret ()).runState env mask (f st) }


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

  let run env m = 
    m_bind Proofview.tclEVARMAP
      (fun sigma -> m_bind (m.runState env false (emptyState sigma)) (fun (x,_) -> m_ret x))
  let lift (x : 'a Proofview.tactic) : 'a t =
    { runState = fun _ _ st -> m_bind x (fun x -> m_ret (x,st)) }

  let env () = { runState = fun env _ st -> m_ret (env,st) }
  let evars () =
    let* stk = get (fun st -> st.ctx_stack) in
    match stk with
    | [] -> lift Proofview.tclEVARMAP
    | sigma :: _ -> ret sigma
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

  let rec concat = function
  | [] -> ret []
  | x :: t ->
      let* x = x in
      let* t = concat t in
      ret (x :: t)
  let mapM f l = concat (List.map f l)
end
open Combinators



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

let emptyMtdt : metadata =
  { is_cat = None
  ; is_funct = None
  ; is_elem = None
  ; is_mph = None
  ; is_eq = None
  }

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

let rec drop n = function
| [] -> []
| x :: l -> if n <= 0 then x :: l else drop (n-1) l
let merge_option opt1 opt2 =
  match opt1, opt2 with
  | Some x, _ -> Some x
  | None, Some y -> Some y
  | None, None -> None

let withMask mask act = { runState = fun env _ st -> act.runState env mask st }
let withEnv env act = { runState = fun _ mask st -> act.runState env mask st }

let stack () = get (fun st -> st.ctx_stack)
let saveState () =
  let* stack = stack () in
  let* sigma = evars () in
  let* _ = set (fun st -> { st with ctx_stack = sigma :: stack }) in
  List.length stack |> ret
let restoreState state =
  let* stack = stack () in
  set (fun st -> { st with ctx_stack = drop (List.length stack - state) stack })
let setState sigma =
  let* stack = stack () in 
  set (fun st -> { st with ctx_stack = sigma :: List.tl stack })

let registerNamespace () =
  let* id = get (fun st -> Array.length st.objects) in
  let* _ = set (fun st ->
    { st with objects = push_back st.objects [| |] }) in
  ret id

let objects ns = get (fun st -> st.objects.(ns))
let find_object ns ec =
  Option.map fst <$> arr_find_optM (fun o -> eqPred o.value ec) @<< objects ns
let hasObject ns ec =
  let* in_ns = find_object ns ec in
  match in_ns with
  | Some id -> some { namespace = ns; id = id; }
  | None -> if ns = 0
            then none () 
            else Option.map (fun id -> { namespace = 0; id = id; }) <$> find_object 0 ec
let registerObj ns vl tp name =
  let* id = hasObject ns vl in
  let* mask = masked () in
  match id with
  | Some obj ->
      let ns = obj.namespace in
      let id = obj.id in
      let* _ = set (fun st ->
        let objects = st.objects in
        objects.(ns).(id) <- {
          value = vl;
          tp = tp;
          name = merge_option name objects.(ns).(id).name;
          mask = mask && objects.(ns).(id).mask;
          mtdt = objects.(ns).(id).mtdt;
        };
        { st with objects = objects }) in
      ret obj
  | None ->
      let* nid = Array.length <$> objects ns in
      let obj = { value = vl; tp = tp; name = name; mask = mask; mtdt = emptyMtdt; } in
      let* _ = set (fun st -> 
        let objects = st.objects in
        objects.(ns) <- push_back st.objects.(ns) obj;
        { st with objects = objects }) in
      ret { namespace = ns; id = nid; }
let getObjValue obj = get (fun st -> st.objects.(obj.namespace).(obj.id).value)
let getObjType obj = get (fun st -> st.objects.(obj.namespace).(obj.id).tp)
let getObjName obj = get (fun st -> st.objects.(obj.namespace).(obj.id).name)
let getObjMask obj = get (fun st -> st.objects.(obj.namespace).(obj.id).mask)
let getObjMtdt obj = get (fun st -> st.objects.(obj.namespace).(obj.id).mtdt)

let markAsCat obj cat =
  set (fun st ->
    let objects = st.objects in
    objects.(obj.namespace).(obj.id) <-
      { objects.(obj.namespace).(obj.id) 
        with mtdt = { objects.(obj.namespace).(obj.id).mtdt with is_cat = Some cat } };
    { st with objects = objects })
let markAsFunct obj funct =
  set (fun st ->
    let objects = st.objects in
    objects.(obj.namespace).(obj.id) <- 
      { objects.(obj.namespace).(obj.id) 
        with mtdt = { objects.(obj.namespace).(obj.id).mtdt with is_funct = Some funct } };
    { st with objects = objects })
let markAsElem obj elem =
  set (fun st ->
    let objects = st.objects in
    objects.(obj.namespace).(obj.id) <- 
      { objects.(obj.namespace).(obj.id) 
        with mtdt = { objects.(obj.namespace).(obj.id).mtdt with is_elem = Some elem } };
    { st with objects = objects })
let markAsMph obj mph =
  set (fun st ->
    let objects = st.objects in
    objects.(obj.namespace).(obj.id) <- 
      { objects.(obj.namespace).(obj.id) 
        with mtdt = { objects.(obj.namespace).(obj.id).mtdt with is_mph = Some mph } };
    { st with objects = objects })
let markAsEq obj eq =
  set (fun st ->
    let objects = st.objects in
    objects.(obj.namespace).(obj.id) <- 
      { objects.(obj.namespace).(obj.id) 
        with mtdt = { objects.(obj.namespace).(obj.id).mtdt with is_eq = Some eq } };
    { st with objects = objects })

