
exception Object_not_found of string

let locate_inductive : string -> Names.inductive = fun name ->
  try match Nametab.global (Libnames.qualid_of_string name) with
    | IndRef i -> i
    | _ -> raise (Object_not_found name)
  with Not_found -> raise (Object_not_found name)

let perform_locate_ind : Names.inductive array ref -> string array -> Names.inductive array =
  fun inds names ->
  if Array.length !inds != 0
  then !inds
  else begin
    inds := Array.map locate_inductive names;
    !inds
  end
let is_ind : Names.inductive array ref -> string array -> Names.inductive -> bool =
  fun inds names ind ->
  let inds = perform_locate_ind inds names in
  Array.exists (Names.Ind.UserOrd.equal ind) inds

let is_projection : Names.Projection.t -> (Names.inductive -> bool) -> string -> bool =
  fun proj indP lbl ->
  let r = Names.Projection.repr proj in
  let pind = Names.Projection.Repr.inductive r in
  let plbl = Names.Projection.Repr.label r in
  indP pind && Names.Label.equal plbl (Names.Label.make lbl)

(*   ___      _ *)
(*  / __|__ _| |_ *)
(* | (__/ _` |  _| *)
(*  \___\__,_|\__| *)
(* cat *)
let g_coq_cat : Names.inductive array ref = ref [| |]
let g_coq_cat_names : string array =
  [| "HoTT.Categories.Category.Core.PreCategory"
   ; "HoTT.Categories.Category.PreCategory"
   ; "HoTT.Categories.PreCategory"
   ; "Categories.Category.PreCategory"
   ; "Categories.PreCategory"
  |]
let is_cat : Names.inductive -> bool = is_ind g_coq_cat g_coq_cat_names


(*  ___ *)
(* | __|__ _ *)
(* | _|/ _` | *)
(* |___\__, | *)
(*        |_| *)
(* eq *)
let g_coq_eq : Names.inductive array ref = ref [| |]
let g_coq_eq_names : string array =
  [| "HoTT.Basics.Overture.paths"
  |]
let is_eq : Names.inductive -> bool = is_ind g_coq_eq g_coq_eq_names
