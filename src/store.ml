
open Data
type store =
  { categories : category array
  ; functors   : funct array 
  ; elems      : elem array 
  ; morphisms  : morphism array 
  ; faces      : eq array
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


(*  ____  _        _        *)
(* / ___|| |_ __ _| |_ ___  *)
(* \___ \| __/ _` | __/ _ \ *)
(*  ___) | || (_| | ||  __/ *)
(* |____/ \__\__,_|\__\___| *)
(*                          *)
(* State operations *)
let get f = fun st -> ret (f st) st

let getCategories = get (fun st -> st.categories)
let getCategory i = get (fun st -> st.categories.(i))
let registerCategory (cat : Data.PA.t) = assert false 

let getFunctors = get (fun st -> st.functors) 
let getFunctor i = get (fun st -> st.functors.(i))
let registerFunctor ~funct ~src ~dst = assert false

let getElems = get (fun st -> st.elems)
let getElem i = get (fun st -> st.elems.(i))
let registerElem ~elem ~cat = assert false

let getMorphisms = get (fun st -> st.morphisms)
let getMorphism i = get (fun st -> st.morphisms.(i))
let registerMorphism ~mph ~cat ~src ~dst = assert false

let getEqs = get (fun st -> st.faces)
let getEq i = get (fun st -> st.faces.(i))
let registerEq ~eq ~right ~left ~cat ~src ~dst = assert false
