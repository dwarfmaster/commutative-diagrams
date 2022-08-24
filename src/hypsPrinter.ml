
let ppe = fun sigma env -> Printer.pr_econstr_env env sigma
let (++) = Pp.(++)

let cat = fun sigma env (cat : Data.category) ->
  ppe sigma env cat.obj ++ Pp.str ":" ++ Pp.int cat.id

let elemI = fun sigma env (elem : Data.internedElem) ->
  ppe sigma env elem.obj
  ++ Pp.str ":(" ++ (cat sigma env elem.category) ++ Pp.str "):"
  ++ Pp.int elem.id
let rec elem = fun sigma env (e : Data.elem) ->
  match e with
  | Elem elem -> elemI sigma env elem 
  | FObj (f,e) ->
      Pp.str "(" ++ ppe sigma env f.obj ++ Pp.str " _0 " ++ elem sigma env e ++ Pp.str ")"

let mphT = fun sigma env (m : Data.morphismT) ->
  ppe sigma env m.obj
  ++ Pp.str ":(" ++ (cat sigma env m.category) ++ Pp.str "):"
  ++ Pp.str ":<" ++ (elem sigma env m.src) ++ Pp.str "> -> <"
  ++ (elem sigma env m.dst) ++ Pp.str ">"

let mphDt = fun sigma env (m : Data.morphismData) ->
  ppe sigma env m.obj ++ Pp.str "::" ++ mphT sigma env m.tp

let mph = fun sigma env (m : Data.morphism) ->
  mphDt sigma env m.data ++ Pp.str "::" ++ Pp.int m.id

let eq = fun sigma env (eq : Data.eq) ->
  Pp.str "{{eq}}"

let rec mphList = fun sigma env (ms : Data.morphism list) ->
  match ms with
  | [ ] -> Pp.str ""
  | [ m ] -> ppe sigma env m.data.obj
  | m :: ms -> ppe sigma env m.data.obj ++ Pp.str ";" ++ mphList sigma env ms

let rec path = fun sigma env (p : Data.path) ->
  ppe sigma env p.mph.obj ++ Pp.str " ={"
  ++ eq sigma env p.eq ++ Pp.str "} "
  ++ compList sigma env p.path
and compList sigma env (ms : (Data.morphism,Data.path) Data.pathComponent list) =
  match ms with
  | [ ] -> Pp.str ""
  | [ m ] -> comp sigma env m 
  | m :: ms -> comp sigma env m ++ Pp.str ";" ++ compList sigma env ms
and comp sigma env (m : (Data.morphism,Data.path) Data.pathComponent) =
  match m with
  | Base m -> ppe sigma env m.data.obj
  | Functor (f,p) -> Pp.str "(" ++ ppe sigma env f.obj ++ Pp.str " _1 "
                  ++ path sigma env p ++ Pp.str ")"

let face = fun sigma env (f : Data.face) ->
  eq sigma env f.obj ++ Pp.str ":::" ++
  path sigma env f.side1 ++ Pp.str " <-> " ++ path sigma env f.side2


(*   ____                 _          _      *)
(*  / ___|_ __ __ _ _ __ | |____   _(_)____ *)
(* | |  _| '__/ _` | '_ \| '_ \ \ / / |_  / *)
(* | |_| | | | (_| | |_) | | | \ V /| |/ /  *)
(*  \____|_|  \__,_| .__/|_| |_|\_/ |_/___| *)
(*                 |_|                      *)

module OrderedElems = struct
  type t = Data.elem 
  let rec compare (e1 : Data.elem) (e2 : Data.elem) =
    match e1, e2 with
    | Elem e1, Elem e2 -> e1.id - e2.id 
    | FObj (f1,e1), FObj (f2,e2) ->
        if f1.id == f2.id then compare e1 e2 else f1.id - f2.id
    | Elem _, FObj _ -> -1
    | FObj _, Elem _ -> 1
end
module ElemSet = Set.Make(OrderedElems)

let allElems (mphs : Data.morphism array) : ElemSet.t =
  let open Data in
  Array.fold_left 
    (fun st mph -> ElemSet.add mph.data.tp.src (ElemSet.add mph.data.tp.dst st))
    ElemSet.empty mphs

let rec elem_to_str (e : Data.elem) =
  match e with
  | Elem e -> Pp.int e.id 
  | FObj (f,e) -> Pp.int f.id ++ Pp.str "_" ++ elem_to_str e
let elem_graphviz = fun sigma env (e : Data.elem) ->
  Pp.str "e" ++ elem_to_str e ++ Pp.str " [ label=\"" ++ elem sigma env e ++ Pp.str "\"];"
let mph_graphviz = fun sigma env (mph : Data.morphism) ->
  match mph.iso with
  | Some data when data.inv.id = mph.id -> Pp.str ""
  | _ ->
    Pp.str "e" ++ elem_to_str mph.data.tp.src ++ Pp.str " -> e" ++ elem_to_str mph.data.tp.dst
    ++ Pp.str " [label=\"" ++ ppe sigma env mph.data.obj ++ Pp.str "\""
    ++ (if mph.mono != None then Pp.str ",arrowhead=\"oldiamond\"" else Pp.str "")
    ++ (if mph.epi  != None then Pp.str ",arrowhead=\"onormalonormal\"" else Pp.str "")
    ++ (if mph.iso  != None then Pp.str ",color=\"red\"" else Pp.str "")
    ++ Pp.str "];"
let to_graphviz = fun sigma env (store : Hyps.t) ->
  let elems = allElems store.morphisms in
  Pp.str "digraph {"
  ++ ElemSet.fold (fun e pp -> pp ++ elem_graphviz sigma env e) elems (Pp.str "")
  ++ Array.fold_left (fun pp m -> pp ++ mph_graphviz  sigma env m) (Pp.str "") store.morphisms
  ++ Pp.str "}"
