
module St = Store.Make(Hott.M)
open St.Combinators
open Data

let ppe = fun sigma env -> Printer.pr_econstr_env env sigma
let (++) = Pp.(++)

let cat sigma env cat =
  match cat with
  | AtomicCategory cat ->
      ppe sigma env cat.cat_obj ++ Pp.str ":" ++ Pp.int cat.cat_id

let funct sigma env funct =
  match funct with
  | AtomicFunctor funct ->
      ppe sigma env funct.funct_obj ++ Pp.str ":" ++ Pp.int funct.funct_id

let rec elem sigma env e =
  match e with
  | AtomicElem e ->
      ppe sigma env e.elem_obj ++ Pp.str ":" ++ Pp.int e.elem_id
  | FObj (f,e) ->
      Pp.str "(" ++ funct sigma env f ++ Pp.str " _0 " ++ elem sigma env e ++ Pp.str ")"

let rec mph sigma env m =
  match m with
  | AtomicMorphism m ->
      ppe sigma env m.mph_obj ++ Pp.str ":" ++ Pp.int m.mph_id
  | Identity e -> Pp.str "id<" ++ elem sigma env e ++ Pp.str ">"
  | Comp (m1,m2) -> Pp.str "(" ++ mph sigma env m1 ++ Pp.str " >> " ++ mph sigma env m2 ++ Pp.str ")"
  | Inv m -> mph sigma env m ++ Pp.str "^-1"
  | FMph (f,m) ->
      Pp.str "(" ++ funct sigma env f ++ Pp.str " _1 " ++ mph sigma env m ++ Pp.str ")"

let eq = fun sigma env eq ->
  Pp.str "{{eq}}"

let rec mphList sigma env ms =
  match ms with
  | [ ] -> Pp.str ""
  | [ m ] -> mph sigma env m
  | m :: ms -> mph sigma env m ++ Pp.str ";" ++ mphList sigma env ms

(*   ____                 _          _      *)
(*  / ___|_ __ __ _ _ __ | |____   _(_)____ *)
(* | |  _| '__/ _` | '_ \| '_ \ \ / / |_  / *)
(* | |_| | | | (_| | |_) | | | \ V /| |/ /  *)
(*  \____|_|  \__,_| .__/|_| |_|\_/ |_/___| *)
(*                 |_|                      *)

module ElemMap = Map.Make(Data.EqElem(Hott))
type eNums = int ElemMap.t

let allElems mphs : eNums =
  let nums, _ =
    Array.fold_left 
      (fun (st,i) mph ->
        (ElemMap.add mph.mph_src_ i
          (ElemMap.add mph.mph_dst_ (i+1) st),
          i+2))
      (ElemMap.empty,0) mphs
  in nums

let elem_graphviz sigma env nums e =
  Pp.str "e" ++ Pp.int (ElemMap.find e nums) ++ Pp.str " [ label=\"" ++ elem sigma env e ++ Pp.str "\"];"

let isMono m =
  match m with
  | AtomicMorphism m -> m.mono != None
  | _ -> false
let isEpi m =
  match m with
  | AtomicMorphism m -> m.epi != None
  | _ -> false
let isIso m =
  match m with
  | AtomicMorphism m -> m.iso != None
  | _ -> false

let mph_graphviz sigma env nums m =
  Pp.str "e" ++ Pp.int (ElemMap.find (morphism_src m) nums)
  ++ Pp.str " -> e" ++ Pp.int (ElemMap.find (morphism_dst m) nums)
  ++ Pp.str " [label=\"" ++ mph sigma env m ++ Pp.str "\""
  ++ (if isMono m then Pp.str ",arrowhead=\"oldiamond\"" else Pp.str "")
  ++ (if isEpi m  then Pp.str ",arrowhead=\"onormalonormal\"" else Pp.str "")
  ++ (if isIso m  then Pp.str ",color=\"red\"" else Pp.str "")
  ++ Pp.str "];"

let to_graphviz sigma env =
  let* mphs = St.getMorphisms () in
  let nums = allElems mphs in
  ret (Pp.str "digraph {"
    ++ ElemMap.fold (fun e _ pp -> pp ++ elem_graphviz sigma env nums e) nums (Pp.str "")
    ++ Array.fold_left (fun pp m -> pp ++ mph_graphviz  sigma env nums (AtomicMorphism m)) (Pp.str "") mphs
    ++ Pp.str "}")
