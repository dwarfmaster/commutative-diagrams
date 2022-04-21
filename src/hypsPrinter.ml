
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
  ppe sigma env eq.eq ++ Pp.str "::<"
  ++ mphDt sigma env eq.src ++ Pp.str "> = <"
  ++ mphDt sigma env eq.dst ++ Pp.str ">::("
  ++ mphT sigma env eq.tp ++ Pp.str ")"

let rec mphList = fun sigma env (ms : Hyps.morphism list) ->
  match ms with
  | [ ] -> Pp.str ""
  | [ m ] -> mph sigma env m
  | m :: ms -> mph sigma env m ++ Pp.str ";" ++ mphList sigma env ms

let path = fun sigma env (p : Hyps.path) ->
  Pp.str "{" ++ mphDt sigma env p.mph ++ Pp.str "} ={"
  ++ eq sigma env p.eq ++ Pp.str "} {"
  ++ mphList sigma env p.path ++ Pp.str "}"

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
  Pp.str "e" ++ Pp.int mph.data.tp.src.id ++ Pp.str " -> e" ++ Pp.int mph.data.tp.dst.id
  ++ Pp.str " [label=\"" ++ ppe sigma env mph.data.obj ++ Pp.str "\"];"
let to_graphviz = fun sigma env (store : Hyps.t) ->
  Pp.str "digraph {"
  ++ Array.fold_left (fun pp e -> pp ++ elem_graphviz sigma env e) (Pp.str "") store.elems
  ++ Array.fold_left (fun pp m -> pp ++ mph_graphviz  sigma env m) (Pp.str "") store.morphisms
  ++ Pp.str "}"
