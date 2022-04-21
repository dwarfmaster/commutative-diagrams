
exception Object_not_found of string

let locate_inductive : string -> Names.inductive = fun name ->
  try match Nametab.global (Libnames.qualid_of_string name) with
    | IndRef i -> i
    | _ -> raise (Object_not_found name)
  with Not_found -> raise (Object_not_found name)
let locate_constructor : string -> Names.constructor = fun name ->
  try match Nametab.global (Libnames.qualid_of_string name) with
    | ConstructRef c -> c
    | _ -> raise (Object_not_found name)
  with Not_found -> raise (Object_not_found name)
let locate_const : string -> Names.Constant.t = fun name ->
  try match Nametab.global (Libnames.qualid_of_string name) with
    | ConstRef c -> c
    | _ -> raise (Object_not_found name)
  with Not_found -> raise (Object_not_found name)

let perform_locate : 'a array ref -> string array -> (string -> 'a) -> 'a array =
  fun objs names locate ->
  if Array.length !objs != 0
  then !objs
  else begin
    objs := Array.map locate names;
    !objs
  end

let is_cached : 'a array ref -> string array -> 'a -> (string -> 'a) -> ('a -> 'a -> bool) -> bool =
  fun objs names obj locate pred ->
  let objs = perform_locate objs names locate in
  Array.exists (pred obj) objs
let is_ind : Names.inductive array ref -> string array -> Names.inductive -> bool =
  fun inds names ind -> is_cached inds names ind locate_inductive Names.Ind.UserOrd.equal
let is_construct : Names.constructor array ref -> string array -> Names.constructor -> bool =
  fun constrs names constr -> is_cached constrs names constr locate_constructor Names.Construct.UserOrd.equal
let is_const : Names.Constant.t array ref -> string array -> Names.Constant.t -> bool =
  fun consts names const -> is_cached consts names const locate_const Names.Constant.UserOrd.equal

let mk_ind : Names.inductive array -> EConstr.t =
  fun inds -> EConstr.mkInd inds.(0)
let mk_constr : Names.constructor array -> EConstr.t =
  fun constrs -> EConstr.mkConstruct constrs.(0)
let mk_const : Names.Constant.t array -> EConstr.t =
  fun consts -> EConstr.mkConst consts.(0)

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
let get_cat = fun _ -> perform_locate g_coq_cat g_coq_cat_names locate_inductive
let is_cat : Names.inductive -> bool = is_ind g_coq_cat g_coq_cat_names
let mk_cat = fun _ -> mk_ind (get_cat ())


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
let get_eq = fun _ -> perform_locate g_coq_eq g_coq_eq_names locate_inductive
let is_eq : Names.inductive -> bool = is_ind g_coq_eq g_coq_eq_names
let mk_eq = fun _ -> mk_ind (get_eq ())


(*  ___      __ _ *)
(* | _ \___ / _| | *)
(* |   / -_)  _| | *)
(* |_|_\___|_| |_| *)
(* refl *)
let g_coq_refl : Names.constructor array ref = ref [| |]
let g_coq_refl_names : string array =
  [| "HoTT.Basics.Overture.idpath"
  |]
let get_refl = fun _ -> perform_locate g_coq_refl g_coq_refl_names locate_constructor
let is_refl : Names.constructor -> bool = is_construct g_coq_refl g_coq_refl_names
let mk_refl = fun _ -> mk_constr (get_refl ())


(*   ___                  _ *)
(*  / __|___ _ _  __ __ _| |_ *)
(* | (__/ _ \ ' \/ _/ _` |  _| *)
(*  \___\___/_||_\__\__,_|\__| *)
(* Concat *)
let g_coq_concat : Names.Constant.t array ref = ref [| |]
let g_coq_concat_names : string array =
  [| "HoTT.Basics.PathGroupoids.concat2"
  |]
let get_concat = fun _ -> perform_locate g_coq_concat g_coq_concat_names locate_const
let is_concat : Names.Constant.t -> bool = is_const g_coq_concat g_coq_concat_names
let mk_concat = fun _ -> mk_const (get_concat ())


(*  ___                    _ *)
(* |_ _|_ ___ _____ _ _ __(_)___ _ _ *)
(*  | || ' \ V / -_) '_(_-< / _ \ ' \ *)
(* |___|_||_\_/\___|_| /__/_\___/_||_| *)
(* Inversion *)
let g_coq_inv : Names.Constant.t array ref = ref [| |]
let g_coq_inv_names : string array =
  [| "HoTT.Basics.Overture.inverse"
  |]
let get_inv = fun _ -> perform_locate g_coq_inv g_coq_inv_names locate_const
let mk_inv = fun _ -> mk_const (get_inv ())

(*   ___ *)
(*  / __|___ _ __  _ __  ___ ___ ___   ___ __ _ *)
(* | (__/ _ \ '  \| '_ \/ _ (_-</ -_) / -_) _` | *)
(*  \___\___/_|_|_| .__/\___/__/\___|_\___\__, | *)
(*                |_|              |___|     |_| *)
(* compose_eq *)
let g_coq_compose_eq : Names.Constant.t array ref = ref [| |]
let g_coq_compose_eq_names : string array =
  [| "Loader.compose_eq"
  |]
let get_compose_eq = fun _ -> perform_locate g_coq_compose_eq g_coq_compose_eq_names locate_const
let mk_compose_eq = fun _ -> mk_const (get_compose_eq ())


(*    _                   _      _   _     _ _         *)
(*   /_\   ______ ___  __(_)__ _| |_(_)_ _(_) |_ _  _  *)
(*  / _ \ (_-<_-</ _ \/ _| / _` |  _| \ V / |  _| || | *)
(* /_/ \_\/__/__/\___/\__|_\__,_|\__|_|\_/|_|\__|\_, | *)
(*                                               |__/  *)
(* Associativity *)
let g_coq_assoc : Names.Constant.t array ref = ref [| |]
let g_coq_assoc_names : string array =
  [| "Loader.assoc"
  |]
let get_assoc = fun _ -> perform_locate g_coq_assoc g_coq_assoc_names locate_const
let mk_assoc = fun _ -> mk_const (get_assoc ())


(*  ___    _         _   _ _         *)
(* |_ _|__| |___ _ _| |_(_) |_ _  _  *)
(*  | |/ _` / -_) ' \  _| |  _| || | *)
(* |___\__,_\___|_||_\__|_|\__|\_, | *)
(*                             |__/  *)
(* Identity *)
let g_coq_left_id : Names.Constant.t array ref = ref [| |]
let g_coq_left_id_names : string array =
  [| "Loader.left_id"
  |]
let get_left_id = fun _ -> perform_locate g_coq_left_id g_coq_left_id_names locate_const
let mk_left_id = fun _ -> mk_const (get_left_id ())
let g_coq_right_id : Names.Constant.t array ref = ref [| |]
let g_coq_right_id_names : string array =
  [| "Loader.right_id"
  |]
let get_right_id = fun _ -> perform_locate g_coq_right_id g_coq_right_id_names locate_const
let mk_right_id = fun _ -> mk_const (get_right_id ())


(*  __  __              _    _ *)
(* |  \/  |___ _ _ _ __| |_ (_)____ __  ___ *)
(* | |\/| / _ \ '_| '_ \ ' \| (_-< '  \(_-< *)
(* |_|  |_\___/_| | .__/_||_|_/__/_|_|_/__/ *)
(*                |_| *)
(* Morphisms *)
let g_coq_id : Names.Constant.t array ref = ref [| |]
let g_coq_id_names : string array =
  [| "Loader.id"
  |]
let get_id = fun _ -> perform_locate g_coq_id g_coq_id_names locate_const
let is_id = is_const g_coq_id g_coq_id_names
let mk_id = fun _ -> mk_const (get_id ())
let g_coq_comp : Names.Constant.t array ref = ref [| |]
let g_coq_comp_names : string array =
  [| "Loader.comp"
  |]
let get_comp = fun _ -> perform_locate g_coq_comp g_coq_comp_names locate_const
let mk_comp = fun _ -> mk_const (get_comp ())
let g_coq_mphT : Names.Constant.t array ref = ref [| |]
let g_coq_mphT_names : string array =
  [| "Loader.mphT"
  |]
let get_mphT = fun _ -> perform_locate g_coq_mphT g_coq_mphT_names locate_const
let mk_mphT = fun _ -> mk_const (get_mphT ())
