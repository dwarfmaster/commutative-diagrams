
open Hyps.Combinators
open Features

let get_objs_value = mapM Hyps.getObjValue

let mk_evar env tp =
  let* sigma = evars () in
  let (sigma,evar) = Evarutil.new_evar env sigma tp in
  let* () = Hyps.setState sigma in
  let* () = Hyps.handleEvar evar in
  ret evar

let build_evar env tp =
  let* ev = mk_evar env tp in
  let* id = Hyps.registerObj ev tp None in
  ret id

(* Disable non-exhaustive match warnings for the build_* functions *)
[@@@ warning "-8"]
let build_category env _ =
  let* tp = Env.mk_cat () |> lift in
  build_evar env tp

let build_object env objs =
  let* [cat] = get_objs_value objs in
  let* tp = Env.app (Env.mk_object ()) [| cat |] |> lift in
  build_evar env tp

let build_morphism env objs =
  let* [cat; src; dst] = get_objs_value objs in
  let* tp = Env.app (Env.mk_mphT ()) [| cat; src; dst |] |> lift in
  build_evar env tp

let build_functor env objs =
  let* [src; dst] = get_objs_value objs in
  let* tp = Env.app (Env.mk_functor ()) [| src; dst |] |> lift in
  build_evar env tp

let build_eq env objs =
  let* [cat; src; dst; left; right] = get_objs_value objs in
  let* mph = Env.app (Env.mk_mphT ()) [| cat; src; dst |] |> lift in
  let* tp = Env.app (Env.mk_eq ()) [| mph; left; right |] |> lift in
  build_evar env tp

let build_funct_obj env objs =
  let* [src; dst; funct; obj] = get_objs_value objs in
  let* ec = Env.app (Env.mk_funct_obj ()) [| src; dst; funct; obj |] |> lift in
  let* tp = Env.app (Env.mk_object ()) [| dst |] |> lift in
  Hyps.registerObj ec tp None

let build_identity env objs =
  let* [cat; obj] = get_objs_value objs in
  let* ec = Env.app (Env.mk_id ()) [| cat; obj |] |> lift in
  let* tp = Env.app (Env.mk_mphT ()) [| cat; obj; obj |] |> lift in
  Hyps.registerObj ec tp None

let build_compose_mph env objs =
  let* [cat; src; mid; dst; m1; m2] = get_objs_value objs in
  let* ec = Env.app (Env.mk_comp ()) [| cat; src; mid; dst; m1; m2 |] |> lift in
  let* tp = Env.app (Env.mk_mphT ()) [| cat; src; dst |] |> lift in
  Hyps.registerObj ec tp None

let build_funct_mph env objs =
  let* [scat; dcat; funct; src; dst; mph] = get_objs_value objs in
  let* ec = Env.app (Env.mk_funct_mph ()) [| scat; dcat; funct; src; dst; mph |] |> lift in
  let* fsrc = Env.app (Env.mk_funct_obj ()) [| scat; dcat; funct; src |] |> lift in
  let* fdst = Env.app (Env.mk_funct_obj ()) [| scat; dcat; funct; dst |] |> lift in
  let* tp = Env.app (Env.mk_mphT ()) [| dcat; fsrc; fdst |] |> lift in
  Hyps.registerObj ec tp None

let build_reflexivity env objs =
  let* [cat; src; dst; mph] = get_objs_value objs in
  let* tmph = Env.app (Env.mk_mphT ()) [| cat; src; dst |] |> lift in
  let* ec = Env.app (Env.mk_refl ()) [| tmph; mph |] |> lift in
  let* tp = Env.app (Env.mk_eq ()) [| tmph; mph; mph |] |> lift in
  Hyps.registerObj ec tp None

let build_concat env objs =
  let* [cat; src; dst; left; mid; right; eq1; eq2] = get_objs_value objs in
  let* tmph = Env.app (Env.mk_mphT ()) [| cat; src; dst |] |> lift in
  let* ec = Env.app (Env.mk_concat ()) [| tmph; left; mid; right; eq1; eq2 |] |> lift in
  let* tp = Env.app (Env.mk_eq ()) [| tmph; left; right |] |> lift in
  Hyps.registerObj ec tp None

let build_inverse_eq env objs =
  let* [cat; src; dst; left; right; eq] = get_objs_value objs in
  let* tmph = Env.app (Env.mk_mphT ()) [| cat; src; dst |] |> lift in
  let* ec = Env.app (Env.mk_inv ()) [| tmph; left; right; eq |] |> lift in
  let* tp = Env.app (Env.mk_eq ()) [| tmph; right; left |] |> lift in
  Hyps.registerObj ec tp None

let build_compose_eq env objs =
  let* [cat; src; mid; dst; left1; right1; eq1; left2; right2; eq2] = get_objs_value objs in
  let* left = Env.app (Env.mk_comp ()) [| cat; src; mid; dst; left1; left2 |] |> lift in
  let* right = Env.app (Env.mk_comp ()) [| cat; src; mid; dst; right1; right2 |] |> lift in
  let* tmph = Env.app (Env.mk_mphT ()) [| cat; left; right |] |> lift in
  let* ec = 
    Env.app (Env.mk_compose_eq ())
            [| cat; src; mid; dst; left1; right1; left2; right2; eq1; eq2 |] |> lift in
  let* tp = Env.app (Env.mk_eq ()) [| cat; left; right |] |> lift in
  Hyps.registerObj ec tp None

let build_assoc env objs =
  let* [cat; src; mid1; mid2; dst; m1; m2; m3] = get_objs_value objs in
  let* ec = Env.app (Env.mk_assoc ()) [| cat; src; mid1; mid2; dst; m1; m2; m3 |] |> lift in
  let* m12 = Env.app (Env.mk_comp ()) [| cat; src; mid1; mid2; m1; m2 |] |> lift in
  let* m23 = Env.app (Env.mk_comp ()) [| cat; mid1; mid2; dst; m2; m3 |] |> lift in
  let* m12_3 = Env.app (Env.mk_comp ()) [| cat; src; mid2; dst; m12; m3 |] |> lift in
  let* m1_23 = Env.app (Env.mk_comp ()) [| cat; src; mid1; dst; m1; m23 |] |> lift in
  let* tmph = Env.app (Env.mk_mphT ()) [| cat; src; dst |] |> lift in
  let* tp = Env.app (Env.mk_eq ()) [| tmph; m12_3; m1_23 |] |> lift in
  Hyps.registerObj ec tp None

let build_left_unitality env objs =
  let* [cat; src; dst; mph] = get_objs_value objs in
  let* ec = Env.app (Env.mk_right_id ()) [| cat; src; dst; mph |] |> lift in
  let* tmph = Env.app (Env.mk_mphT ()) [| cat; src; dst |] |> lift in
  let* id = Env.app (Env.mk_id ()) [| cat; dst |] |> lift in
  let* mphid = Env.app (Env.mk_comp ()) [| cat; src; dst; dst; mph; id |] |> lift in
  let* tp = Env.app (Env.mk_eq ()) [| tmph; mphid; mph |] |> lift in
  Hyps.registerObj ec tp None

let build_right_unitality env objs =
  let* [cat; src; dst; mph] = get_objs_value objs in
  let* ec = Env.app (Env.mk_left_id ()) [| cat; src; dst; mph |] |> lift in
  let* tmph = Env.app (Env.mk_mphT ()) [| cat; src; dst |] |> lift in
  let* id = Env.app (Env.mk_id ()) [| cat; src |] |> lift in
  let* idmph = Env.app (Env.mk_comp ()) [| cat; src; src; dst; id; mph |] |> lift in
  let* tp = Env.app (Env.mk_eq ()) [| tmph; idmph; mph |] |> lift in
  Hyps.registerObj ec tp None

let build_left_application env objs =
  let* [cat; src; mid; dst; mph; left; right; eq] = get_objs_value objs in
  let* ec = Env.app (Env.mk_lap ()) [| cat; src; mid; dst; mph; left; right; eq |] |> lift in
  let* mleft = Env.app (Env.mk_comp ()) [| cat; src; mid; dst; mph; left |] |> lift in
  let* mright = Env.app (Env.mk_comp ()) [| cat; src; mid; dst; mph; right |] |> lift in
  let* tmph = Env.app (Env.mk_mphT ()) [| cat; src; dst |] |> lift in
  let* tp = Env.app (Env.mk_eq ()) [| tmph; mleft; mright |] |> lift in
  Hyps.registerObj ec tp None

let build_right_application env objs =
  let* [cat; src; mid; dst; left; right; eq; mph] = get_objs_value objs in
  let* ec = Env.app (Env.mk_rap ()) [| cat; src; mid; dst; left; right; mph; eq |] |> lift in
  let* leftm = Env.app (Env.mk_comp ()) [| cat; src; mid; dst; left; mph |] |> lift in
  let* rightm = Env.app (Env.mk_comp ()) [| cat; src; mid; dst; right; mph |] |> lift in
  let* tmph = Env.app (Env.mk_mphT ()) [| cat; src; dst |] |> lift in
  let* tp = Env.app (Env.mk_eq ()) [| tmph; leftm; rightm |] |> lift in
  Hyps.registerObj ec tp None

let build_funct_identity env objs =
  let* [src; dst; funct; obj] = get_objs_value objs in
  let* ec = Env.app (Env.mk_funct_id ()) [| src; dst; funct; obj |] |> lift in
  let* idobj = Env.app (Env.mk_id ()) [| src; obj |] |> lift in
  let* fidobj = Env.app (Env.mk_funct_mph ()) [| src; dst; funct; obj; obj; idobj |] |> lift in
  let* fobj = Env.app (Env.mk_funct_obj ()) [| src; dst; funct; obj |] |> lift in
  let* idfobj = Env.app (Env.mk_id ()) [| dst; fobj |] |> lift in
  let* tmph = Env.app (Env.mk_mphT ()) [| dst; fobj; fobj |] |> lift in
  let* tp = Env.app (Env.mk_eq ()) [| tmph; fidobj; idfobj |] |> lift in
  Hyps.registerObj ec tp None

let build_funct_composition env objs =
  let* [scat; dcat; funct; src; mid; dst; m1; m2] = get_objs_value objs in
  let* ec = Env.app (Env.mk_funct_comp ()) [| scat; dcat; funct; src; mid; dst; m1; m2 |] |> lift in
  let* m12 = Env.app (Env.mk_comp ()) [| scat; src; mid; dst; m1; m2 |] |> lift in
  let* fm12 = Env.app (Env.mk_funct_mph ()) [| scat; dcat; funct; src; dst; m12 |] |> lift in
  let* fsrc = Env.app (Env.mk_funct_obj ()) [| scat; dcat; funct; src |] |> lift in
  let* fmid = Env.app (Env.mk_funct_obj ()) [| scat; dcat; funct; mid |] |> lift in
  let* fdst = Env.app (Env.mk_funct_obj ()) [| scat; dcat; funct; dst |] |> lift in
  let* fm1 = Env.app (Env.mk_funct_mph ()) [| scat; dcat; funct; src; mid; m1 |] |> lift in
  let* fm2 = Env.app (Env.mk_funct_mph ()) [| scat; dcat; funct; mid; dst; m2 |] |> lift in
  let* fm1fm2 = Env.app (Env.mk_comp ()) [| dcat; fsrc; fmid; fdst; fm1; fm2 |] |> lift in
  let* tmph = Env.app (Env.mk_mphT ()) [| dcat; fsrc; fdst |] |> lift in
  let* tp = Env.app (Env.mk_eq ()) [| tmph; fm12; fm1fm2 |] |> lift in
  Hyps.registerObj ec tp None

let build_funct_eq env objs =
  let* [scat; dcat; funct; src; dst; left; right; eq] = get_objs_value objs in
  let* ec = 
    Env.app (Env.mk_funct_ctx ()) [| scat; dcat; funct; src; dst; left; right; eq |] |> lift in
  let* fleft = Env.app (Env.mk_funct_mph ()) [| scat; dcat; funct; src; dst; left |] |> lift in
  let* fright = Env.app (Env.mk_funct_mph ()) [| scat; dcat; funct; src; dst; right |] |> lift in
  let* fsrc = Env.app (Env.mk_funct_obj ()) [| scat; dcat; funct; src |] |> lift in
  let* fdst = Env.app (Env.mk_funct_obj ()) [| scat; dcat; funct; dst |] |> lift in
  let* tmph = Env.app (Env.mk_mphT ()) [| dcat; fsrc; fdst |] |> lift in
  let* tp = Env.app (Env.mk_eq ()) [| tmph; fleft; fright |] |> lift in
  Hyps.registerObj ec tp None

[@@@ warning "+8"]
let build feat =
  let* env = env () in
  let objs = to_list feat in
  match tag feat with
  | Category -> build_category env objs
  | Object -> build_object env objs
  | Morphism -> build_morphism env objs
  | Functor -> build_functor env objs
  | Equality -> build_eq env objs
  | AppliedFunctObj -> build_funct_obj env objs
  | Identity -> build_identity env objs
  | ComposeMph -> build_compose_mph env objs
  | AppliedFunctMph -> build_funct_mph env objs
  | Reflexivity -> build_reflexivity env objs
  | Concat -> build_concat env objs
  | InverseEq -> build_inverse_eq env objs
  | ComposeEq -> build_compose_eq env objs
  | Associativity -> build_assoc env objs
  | LeftUnitality -> build_left_unitality env objs
  | RightUnitality -> build_right_unitality env objs
  | LeftApplication -> build_left_application env objs
  | RightApplication -> build_right_application env objs
  | FunctIdentity -> build_funct_identity env objs
  | FunctComposition -> build_funct_composition env objs
  | AppliedFunctEq -> build_funct_eq env objs

