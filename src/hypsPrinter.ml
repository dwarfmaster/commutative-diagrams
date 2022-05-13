
let ppe = fun sigma env -> Printer.pr_econstr_env env sigma
let (++) = Pp.(++)

let cat = fun sigma env (cat : Hyps.category) ->
  ppe sigma env cat.obj ++ Pp.str ":" ++ Pp.int cat.id

let elem = fun sigma env (elem : Hyps.elem) ->
  ppe sigma env elem.obj
  ++ Pp.str ":(" ++ (cat sigma env elem.category) ++ Pp.str "):"
  ++ Pp.int elem.id

let mphT = fun sigma env (m : Hyps.morphismT) ->
  ppe sigma env m.obj
  ++ Pp.str ":(" ++ (cat sigma env m.category) ++ Pp.str "):"
  ++ Pp.str ":<" ++ (elem sigma env m.src) ++ Pp.str "> -> <"
  ++ (elem sigma env m.dst) ++ Pp.str ">"

let mphDt = fun sigma env (m : Hyps.morphismData) ->
  ppe sigma env m.obj ++ Pp.str "::" ++ mphT sigma env m.tp

let mph = fun sigma env (m : Hyps.morphism) ->
  mphDt sigma env m.data ++ Pp.str "::" ++ Pp.int m.id

let eq = fun sigma env (eq : Hyps.eq) ->
  Pp.str "{{eq}}"

let rec mphList = fun sigma env (ms : Hyps.morphism list) ->
  match ms with
  | [ ] -> Pp.str ""
  | [ m ] -> ppe sigma env m.data.obj
  | m :: ms -> ppe sigma env m.data.obj ++ Pp.str ";" ++ mphList sigma env ms

let path = fun sigma env (p : Hyps.path) ->
  ppe sigma env p.mph.obj ++ Pp.str " ={"
  ++ eq sigma env p.eq ++ Pp.str "} "
  ++ mphList sigma env p.path

let face = fun sigma env (f : Hyps.face) ->
  eq sigma env f.obj ++ Pp.str ":::" ++
  path sigma env f.side1 ++ Pp.str " <-> " ++ path sigma env f.side2


(*   ____                 _          _      *)
(*  / ___|_ __ __ _ _ __ | |____   _(_)____ *)
(* | |  _| '__/ _` | '_ \| '_ \ \ / / |_  / *)
(* | |_| | | | (_| | |_) | | | \ V /| |/ /  *)
(*  \____|_|  \__,_| .__/|_| |_|\_/ |_/___| *)
(*                 |_|                      *)
let elem_graphviz = fun sigma env (elem : Hyps.elem) ->
  Pp.str "e" ++ Pp.int elem.id ++ Pp.str " [ label=\"" ++ ppe sigma env elem.obj ++ Pp.str "\"];"
let mph_graphviz = fun sigma env (mph : Hyps.morphism) ->
  match mph.iso with
  | Some data when data.inv = mph.id -> Pp.str ""
  | _ ->
    Pp.str "e" ++ Pp.int mph.data.tp.src.id ++ Pp.str " -> e" ++ Pp.int mph.data.tp.dst.id
    ++ Pp.str " [label=\"" ++ ppe sigma env mph.data.obj ++ Pp.str "\""
    ++ (if mph.mono != None then Pp.str ",arrowhead=\"oldiamond\"" else Pp.str "")
    ++ (if mph.epi  != None then Pp.str ",arrowhead=\"onormalonormal\"" else Pp.str "")
    ++ (if mph.iso  != None then Pp.str ",color=\"red\"" else Pp.str "")
    ++ Pp.str "];"
let to_graphviz = fun sigma env (store : Hyps.t) ->
  Pp.str "digraph {"
  ++ Array.fold_left (fun pp e -> pp ++ elem_graphviz sigma env e) (Pp.str "") store.elems
  ++ Array.fold_left (fun pp m -> pp ++ mph_graphviz  sigma env m) (Pp.str "") store.morphisms
  ++ Pp.str "}"
