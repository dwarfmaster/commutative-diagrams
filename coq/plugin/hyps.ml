
type obj = int
type metadata =
  { is_cat: unit option
  ; is_funct: (int * int) option
  ; is_elem: int option
  ; is_mph: (int * int * int) option
  ; is_eq: (int * int * int * int * int) option
  }
type obj_impl =
  { value: EConstr.t
  ; tp: EConstr.t
  ; name: string option
  ; mtdt: metadata
  }

(* Masked elements are not included in the goal graph sent to the engine *)
module IntMap = Map.Make(struct type t = int let compare = compare end)
type evar_state = (Evar.t * Evd.evar_info) list
type state =
  { ctx_stack  : evar_state list
  ; handled    : Evar.t list
  ; objects    : obj_impl array array
  ; evars      : Evd.evar_map array
  }

let evarStateFromEvarMap mask sigma =
  Evd.fold (fun ev info acc -> (ev,info) :: acc) sigma []
  |> List.filter (fun (ev,_) -> List.exists (fun ev2 -> Evar.compare ev ev2 = 0) mask)
let emptyState sigma : state =
  { ctx_stack  = [ ]
  ; handled = [ ]
  ; objects = [| [| |] |]
  ; evars = [| sigma |]
  }

type 'a t = 
  { runState : Environ.env -> int -> state -> ('a * state) Proofview.tactic }
let ret x = { runState = fun _ _ st -> Proofview.tclUNIT (x,st) }
let get (f : state -> 'a) : 'a t = 
  { runState = fun env ns st -> (ret (f st)).runState env ns st }
let set (f : state -> state) : unit t = 
  { runState = fun env ns st -> (ret ()).runState env ns (f st) }


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
    { runState = fun env ns st -> 
        m_bind 
          (a.runState env ns st) 
          (fun (a,st) -> (f a).runState env ns st) }
  let (let*) = bind
  let (>>=)  = bind
  let (@<<) f a = bind a f
  let (<$>) f a = bind a (fun x -> ret (f x))

  let run env m = 
    m_bind Proofview.tclEVARMAP
      (fun sigma -> m_bind (m.runState env 0 (emptyState sigma)) (fun (x,_) -> m_ret x))
  let lift (x : 'a Proofview.tactic) : 'a t =
    { runState = fun _ _ st -> m_bind x (fun x -> m_ret (x,st)) }

  let env () = { runState = fun env _ st -> m_ret (env,st) }
  let evars () =
    lift Proofview.tclEVARMAP
  let getNS () = { runState = fun _ ns st -> m_ret (ns,st) }
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
  ret (EConstr.eq_constr_nounivs sigma x y)
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

let withNS ns act = { runState = fun env _ st -> act.runState env ns st }
let withEnv env act = { runState = fun _ ns st -> act.runState env ns st }

let stack () = get (fun st -> st.ctx_stack)
let saveState () =
  let* stack = stack () in
  let* sigma = evars () in
  let* handled = get (fun st -> st.handled) in
  let state = evarStateFromEvarMap handled sigma in
  let* _ = set (fun st -> { st with ctx_stack = state :: stack }) in
  List.length stack |> ret
let applyState sigma handled state =
  (* A bit brutal *)
  let evars = Evd.fold (fun evar _ acc -> evar :: acc) sigma []
            |> List.filter (fun ev -> List.exists (fun ev2 -> Evar.compare ev ev2 = 0) handled) in
  let sigma = Evd.unshelve sigma evars in
  let sigma = List.fold_left (fun sigma ev -> Evd.remove sigma ev) sigma evars in
  let sigma = List.fold_left (fun sigma (ev,info) -> Evd.add sigma ev info) sigma state in
  let sigma = List.fold_left (fun sigma (ev,_) -> Evd.declare_future_goal ev sigma) sigma state in
  sigma
let restoreState state =
  let* stack = stack () in
  let nstack = drop (List.length stack - state - 1) stack in
  let* sigma = evars () in
  let state = List.hd nstack in
  let* handled = get (fun st -> st.handled) in
  let sigma = applyState sigma handled state in
  let* () = Proofview.Unsafe.tclEVARS sigma |> lift in
  let handled = handled 
              |> List.filter (fun ev -> List.exists 
                                          (fun (ev2,_) -> Evar.compare ev ev2 = 0)
                                          state) in
  set (fun st -> { st with ctx_stack = nstack; handled = handled })
let setState sigma =
  Proofview.Unsafe.tclEVARS sigma |> lift
let mapState fn =
  let* sigma = evars () in
  let r, sigma = fn sigma in
  let* () = Proofview.Unsafe.tclEVARS sigma |> lift in
  ret r
let handleEvar ev =
  let* sigma = evars () in
  match EConstr.kind sigma ev with
  | Evar (ev,_) -> set (fun st -> { st with handled = ev :: st.handled })
  | _ -> assert false (* The constr must be an evar *)
let handled () = get (fun st -> st.handled)

let evarsExcursion act =
  let* sigma = evars () in
  let* r = act in
  let* rsigma = evars () in
  let* () = setState sigma in
  ret (r, rsigma)

let registerNamespace () =
  let* id = get (fun st -> Array.length st.objects) in
  let* sigma = evars () in
  let* _ = set (fun st ->
    { st with 
        objects = push_back st.objects [| |]; 
        evars = push_back st.evars sigma;
    }) in
  ret id
let inNamespace ?(rollback = true) ns act =
  let* sigma = evars () in
  let* ns_sigma = get (fun st -> st.evars.(ns)) in
  let* () = Proofview.Unsafe.tclEVARS ns_sigma |> lift in
  let* r = withNS ns act in
  let* ns_sigma = evars () in
  let* () = set (fun st ->
    let evars = st.evars in
    evars.(ns) <- ns_sigma;
    { st with evars = evars }) in
  let* () = 
    if rollback
    then Proofview.Unsafe.tclEVARS sigma |> lift
    else ret () in
  ret r

let objects () = 
  let* ns = getNS () in
  get (fun st -> st.objects.(ns))
let hasObject ec =
  Option.map fst <$> arr_find_optM (fun o -> eqPred o.value ec) @<< objects ()
let registerObj vl tp name =
  let* ns = getNS () in
  let* id = hasObject vl in
  match id with
  | Some id ->
      let* _ = set (fun st ->
        let objects = st.objects in
        objects.(ns).(id) <- {
          value = vl;
          tp = tp;
          name = merge_option name objects.(ns).(id).name;
          mtdt = objects.(ns).(id).mtdt;
        };
        { st with objects = objects }) in
      ret id
  | None ->
      let* nid = Array.length <$> objects () in
      let obj = { 
        value = vl; 
        tp = tp; 
        name = name; 
        mtdt = emptyMtdt; 
      } in
      let* _ = set (fun st -> 
        let objects = st.objects in
        objects.(ns) <- push_back st.objects.(ns) obj;
        { st with objects = objects }) in
      ret nid
let getObjValue obj = 
  let* ns = getNS () in 
  get (fun st -> st.objects.(ns).(obj).value)
let getObjType obj = 
  let* ns = getNS () in 
  get (fun st -> st.objects.(ns).(obj).tp)
let getObjName obj = 
  let* ns = getNS () in 
  get (fun st -> st.objects.(ns).(obj).name)
let getObjMtdt obj = 
  let* ns = getNS () in 
  get (fun st -> st.objects.(ns).(obj).mtdt)
let getObjRepr obj =
  let* vl = getObjValue obj in
  let* r = hasObject vl in
  match r with
  | Some id -> ret id
  | None -> assert false (* Shouldn't happen *)
let markAsCat obj cat =
  let* ns = getNS () in 
  set (fun st ->
    let objects = st.objects in
    objects.(ns).(obj) <-
      { objects.(ns).(obj) 
        with mtdt = { objects.(ns).(obj).mtdt with is_cat = Some cat } };
    { st with objects = objects })
let markAsFunct obj funct =
  let* ns = getNS () in 
  set (fun st ->
    let objects = st.objects in
    objects.(ns).(obj) <- 
      { objects.(ns).(obj) 
        with mtdt = { objects.(ns).(obj).mtdt with is_funct = Some funct } };
    { st with objects = objects })
let markAsElem obj elem =
  let* ns = getNS () in 
  set (fun st ->
    let objects = st.objects in
    objects.(ns).(obj) <- 
      { objects.(ns).(obj) 
        with mtdt = { objects.(ns).(obj).mtdt with is_elem = Some elem } };
    { st with objects = objects })
let markAsMph obj mph =
  let* ns = getNS () in 
  set (fun st ->
    let objects = st.objects in
    objects.(ns).(obj) <- 
      { objects.(ns).(obj) 
        with mtdt = { objects.(ns).(obj).mtdt with is_mph = Some mph } };
    { st with objects = objects })
let markAsEq obj eq =
  let* ns = getNS () in 
  set (fun st ->
    let objects = st.objects in
    objects.(ns).(obj) <- 
      { objects.(ns).(obj) 
        with mtdt = { objects.(ns).(obj).mtdt with is_eq = Some eq } };
    { st with objects = objects })

