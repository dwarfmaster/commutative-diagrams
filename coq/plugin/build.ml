
open Hyps.Combinators
open Features

let mk_evar env sigma tp =
  let (sigma,evar) = Evarutil.new_evar env sigma tp in
  let* _ = Hyps.setState sigma in
  ret evar

let build_evar env sigma tp =
  let* ev = mk_evar env sigma tp in
  let* id = Hyps.registerObj ev tp None in
  ret id

let build_category env sigma =
  let* tp = Env.mk_cat () |> lift in
  build_evar env sigma tp

let build_object env sigma cat =
  let* cat = Hyps.getObjValue cat in
  let* tp = Env.app (Env.mk_object ()) [| cat |] |> lift in
  build_evar env sigma tp

let build_morphism env sigma cat src dst =
  let* cat = Hyps.getObjValue cat in
  let* src = Hyps.getObjValue src in
  let* dst = Hyps.getObjValue dst in
  let* tp = Env.app (Env.mk_mphT ()) [| cat; src; dst |] |> lift in
  build_evar env sigma tp

let build_functor env sigma src dst =
  let* src = Hyps.getObjValue src in
  let* dst = Hyps.getObjValue dst in
  let* tp = Env.app (Env.mk_funct_obj ()) [| src; dst |] |> lift in
  build_evar env sigma tp

let build_eq env sigma cat src dst left right =
  let* cat = Hyps.getObjValue cat in
  let* src = Hyps.getObjValue src in
  let* dst = Hyps.getObjValue dst in
  let* left = Hyps.getObjValue left in
  let* right = Hyps.getObjValue right in
  let* mph = Env.app (Env.mk_mphT ()) [| cat; src; dst |] |> lift in
  let* tp = Env.app (Env.mk_eq ()) [| mph; left; right |] |> lift in
  build_evar env sigma tp

let build feat =
  let* env = env () in
  let* sigma = evars () in
  match feat with
  | Category -> build_category env sigma
  | Object cat -> build_object env sigma cat
  | Morphism (cat,src,dst) -> build_morphism env sigma cat src dst
  | Functor (src,dst) -> build_functor env sigma src dst
  | Equality (cat,src,dst,left,right) -> build_eq env sigma cat src dst left right
  | Prop -> assert false
  | AppliedFunctObj _ -> assert false
  | Identity _ -> assert false
  | ComposeMph _ -> assert false
  | InverseMph _ -> assert false
  | AppliedFunctMph _ -> assert false
  | Reflexivity _ -> assert false
  | Concat _ -> assert false
  | InverseEq _ -> assert false
  | ComposeEq _ -> assert false
  | Associativity _ -> assert false
  | LeftUnitality _ -> assert false
  | RightUnitality _ -> assert false
  | LeftApplication _ -> assert false
  | RightApplication _ -> assert false
  | FunctIdentity _ -> assert false
  | FunctComposition _ -> assert false
  | AppliedFunctEq _ -> assert false

