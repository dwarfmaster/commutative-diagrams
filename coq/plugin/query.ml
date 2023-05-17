
open Hyps.Combinators
open Features.Tag

let isProj sigma indPred name t =
  match EConstr.kind sigma t with
  | Proj (p,_) -> Env.is_projection p indPred name 
  | _ -> false
let isConst sigma pred t =
  match EConstr.kind sigma t with 
  | Const (name,_) -> pred name
  | _ -> false
let isInd sigma pred t =
  match EConstr.kind sigma t with
  | Ind (ind,_) -> pred ind 
  | _ -> false

let ofst = Option.map fst

(* TODO will break in case of coq evars in ctx *)
let getType env sigma e : EConstr.t Hyps.t =
  Retyping.get_type_of env sigma e |> ret

let run_query_cached env sigma tp
                     (query : EConstr.t -> 'a option Hyps.t)
                     (querySuper : 'a -> 'b option Hyps.t)
                     (checkMtdt : Hyps.metadata -> 'b option)
                     (setMtdt : int -> 'b -> unit Hyps.t)
                     : (int * 'b) option Hyps.t =
  let registerQuery = fun getId query ->
    match query with
    | None -> none ()
    | Some v ->
        let* id = getId () in
        let* super = querySuper v in
        match super with
        | None -> none ()
        | Some super ->
            let* _ = setMtdt id super in
            some (id, super) in
  let* has_obj = Hyps.hasObject tp in
  match has_obj with
  | Some id ->
      let* mtdt = Hyps.getObjMtdt id in
      begin match checkMtdt mtdt with
      | Some super -> some (id, super)
      | None ->
          let* q = query tp in
          registerQuery (fun () -> ret id) q
      end
  | None ->
      let* q = query tp in
      registerQuery (fun () -> let* tptp = getType env sigma tp in Hyps.registerObj tp tptp None) q

let rec query_cat env sigma tp =
  if isInd sigma Env.is_cat tp then some () else none ()
and query_cat_cached env sigma tp =
  run_query_cached env sigma tp
    (query_cat env sigma)
    (fun () -> some ())
    (fun mtdt -> mtdt.is_cat)
    Hyps.markAsCat

and query_object env sigma tp =
  match EConstr.kind sigma tp with
  | Proj (p,cat) when Env.is_projection p Env.is_cat "object" ->
      some cat
  | _ -> none ()
and query_object_cached env sigma tp =
  run_query_cached env sigma tp
    (query_object env sigma)
    (fun cat -> ofst <$> query_impl env sigma Category cat @<< getType env sigma cat)
    (fun mtdt -> mtdt.is_elem)
    Hyps.markAsElem

and query_morphism env sigma tp =
  match EConstr.kind sigma tp with
  | App (p, [| src; dst |]) ->
    begin match EConstr.kind sigma p with
      | Proj (p,cat) when Env.is_projection p Env.is_cat "morphism" ->
          some (cat,src,dst)
      | _ -> none ()
    end
  | _ -> none ()
and query_morphism_cached env sigma tp =
  run_query_cached env sigma tp
    (query_morphism env sigma)
    (fun (cat, src, dst) ->
      let* cat = ofst <$> query_impl env sigma Category cat @<< getType env sigma cat in
      let* src = ofst <$> query_impl env sigma Object src @<< getType env sigma src in
      let* dst = ofst <$> query_impl env sigma Object dst @<< getType env sigma dst in
      match cat, src, dst with
      | Some cat, Some src, Some dst -> some (cat, src, dst)
      | _ -> none ())
    (fun mtdt -> mtdt.is_mph)
    Hyps.markAsMph

and query_functor env sigma tp =
  match EConstr.kind sigma tp with
  | App (funct, [| src; dst |]) when isInd sigma Env.is_functor funct ->
      some (src,dst)
  | _ -> none ()
and query_functor_cached env sigma tp =
  run_query_cached env sigma tp
    (query_functor env sigma)
    (fun (src,dst) ->
      let* src = ofst <$> query_impl env sigma Category src @<< getType env sigma src in
      let* dst = ofst <$> query_impl env sigma Category dst @<< getType env sigma dst in
      match src, dst with
      | Some src, Some dst -> some (src, dst)
      | _ -> none ())
    (fun mtdt -> mtdt.is_funct)
    Hyps.markAsFunct

and query_eq env sigma tp =
  match EConstr.kind sigma tp with
  | App (eq, [| tp; left; right |]) ->
    begin match EConstr.kind sigma eq with
      | Ind (eq,_) when Env.is_eq eq ->
          let* tp = Option.map snd <$> query_morphism_cached env sigma tp in 
          begin match tp with
          | Some (cat,src,dst) -> some (cat,src,dst,left,right)
          | None -> none ()
          end
      | _ -> none ()
    end
  | _ -> none ()
and query_eq_cached env sigma tp =
  run_query_cached env sigma tp
    (query_eq env sigma)
    (fun (cat,src,dst,left,right) ->
      let* left = ofst <$> query_impl env sigma Morphism left @<< getType env sigma left in
      let* right = ofst <$> query_impl env sigma Morphism right @<< getType env sigma right in
      match left, right with
      | Some left, Some right -> some (cat, src, dst, left, right)
      | _ -> none ())
    (fun mtdt -> mtdt.is_eq)
    Hyps.markAsEq

and query_impl env sigma feat ec tp =
  match feat with
  | Category -> Option.map (fun (id,_) -> (id,Features.Category)) <$> query_cat_cached env sigma tp
  | Object -> Option.map (fun (id,v) -> (id,Features.Object v)) <$> query_object_cached env sigma tp
  | Morphism -> Option.map (fun (id,(cat,src,dst)) -> (id,Features.Morphism (cat,src,dst)))
                       <$> query_morphism_cached env sigma tp
  | Functor -> Option.map (fun (id,(src,dst)) -> (id,Features.Functor (src,dst)))
                      <$> query_functor_cached env sigma tp
  | Equality -> Option.map (fun (id,(cat,src,dst,left,right)) -> (id,Features.Equality (cat,src,dst,left,right)))
                       <$> query_eq_cached env sigma tp
  | Prop -> none ()
  | AppliedFunctObj -> assert false
  | Identity -> assert false
  | ComposeMph -> assert false
  | InverseMph -> assert false
  | AppliedFunctMph -> assert false
  | Reflexivity -> assert false
  | Concat -> assert false
  | InverseEq -> assert false
  | ComposeEq -> assert false
  | Associativity -> assert false
  | LeftUnitality -> assert false
  | RightUnitality -> assert false
  | LeftApplication -> assert false
  | RightApplication -> assert false
  | FunctIdentity -> assert false
  | FunctComposition -> assert false
  | AppliedFunctEq -> assert false

let query env feat ec tp =
  let* sigma = evars () in
  query_impl env sigma feat ec tp
