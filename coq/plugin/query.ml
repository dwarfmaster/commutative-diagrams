
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

let ofst opt = Option.map fst opt

(* TODO will break in case of coq evars in ctx *)
let getType env sigma e : EConstr.t Hyps.t =
  Retyping.get_type_of env sigma e |> ret

let get_type ns env sigma e =
  let* id = Hyps.hasObject ns e in
  match id with
  | Some id -> Hyps.getObjType id
  | None -> getType env sigma e


(*   ___                         *)
(*  / _ \ _   _  ___ _ __ _   _  *)
(* | | | | | | |/ _ \ '__| | | | *)
(* | |_| | |_| |  __/ |  | |_| | *)
(*  \__\_\\__,_|\___|_|   \__, | *)
(*                        |___/  *)
type obj = Hyps.obj

let run_query_cached ns env sigma tp
                     (query : EConstr.t -> 'a option Hyps.t)
                     (querySuper : 'a -> 'b option Hyps.t)
                     (checkMtdt : Hyps.metadata -> 'b option)
                     (setMtdt : obj -> 'b -> unit Hyps.t)
                     : (obj * 'b) option Hyps.t =
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
  let* has_obj = Hyps.hasObject ns tp in
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
      registerQuery
        (fun () -> let* tptp = get_type ns env sigma tp in Hyps.registerObj ns tp tptp None) q

let register ns ec tp (q : 'a option) : (obj * 'a) option Hyps.t =
  match q with
  | Some q ->
      let* id = Hyps.registerObj ns ec tp None in
      some (id,q)
  | None -> none ()

let rec query_cat env sigma tp =
  if isInd sigma Env.is_cat tp then some () else none ()
and query_cat_cached ns env sigma tp =
  run_query_cached ns env sigma tp
    (query_cat env sigma)
    (fun () -> some ())
    (fun mtdt -> mtdt.is_cat)
    Hyps.markAsCat

and query_object env sigma tp =
  match EConstr.kind sigma tp with
  | Proj (p,cat) when Env.is_projection p Env.is_cat "object" ->
      some cat
  | _ -> none ()
and query_object_cached ns env sigma tp =
  run_query_cached ns env sigma tp
    (query_object env sigma)
    (fun cat -> ofst <$> query_impl ns env sigma Category cat @<< get_type ns env sigma cat)
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
and query_morphism_cached ns env sigma tp =
  run_query_cached ns env sigma tp
    (query_morphism env sigma)
    (fun (cat, src, dst) ->
      let* cat = ofst <$> query_impl ns env sigma Category cat @<< get_type ns env sigma cat in
      let* src = ofst <$> query_impl ns env sigma Object src @<< get_type ns env sigma src in
      let* dst = ofst <$> query_impl ns env sigma Object dst @<< get_type ns env sigma dst in
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
and query_functor_cached ns env sigma tp =
  run_query_cached ns env sigma tp
    (query_functor env sigma)
    (fun (src,dst) ->
      let* src = ofst <$> query_impl ns env sigma Category src @<< get_type ns env sigma src in
      let* dst = ofst <$> query_impl ns env sigma Category dst @<< get_type ns env sigma dst in
      match src, dst with
      | Some src, Some dst -> some (src, dst)
      | _ -> none ())
    (fun mtdt -> mtdt.is_funct)
    Hyps.markAsFunct

and query_eq ns env sigma tp =
  match EConstr.kind sigma tp with
  | App (eq, [| tp; left; right |]) ->
    begin match EConstr.kind sigma eq with
      | Ind (eq,_) when Env.is_eq eq ->
          let* tp = Option.map snd <$> query_morphism_cached ns env sigma tp in 
          begin match tp with
          | Some (cat,src,dst) -> some (cat,src,dst,left,right)
          | None -> none ()
          end
      | _ -> none ()
    end
  | _ -> none ()
and query_eq_cached ns env sigma tp =
  run_query_cached ns env sigma tp
    (query_eq ns env sigma)
    (fun (cat,src,dst,left,right) ->
      let* left = ofst <$> query_impl ns env sigma Morphism left 
                       @<< get_type ns env sigma left in
      let* right = ofst <$> query_impl ns env sigma Morphism right 
                        @<< get_type ns env sigma right in
      match left, right with
      | Some left, Some right -> some (cat, src, dst, left, right)
      | _ -> none ())
    (fun mtdt -> mtdt.is_eq)
    Hyps.markAsEq

and query_funct_obj ns env sigma ec tp =
  match EConstr.kind sigma ec with
  | App (fobj, [| elem |]) ->
      begin match EConstr.kind sigma fobj with
      | Proj (fobj,funct) when Env.is_projection fobj Env.is_functor "object_of" ->
          let* funct = query_impl ns env sigma Functor funct @<< get_type ns env sigma funct in
          let* elem = ofst <$> query_impl ns env sigma Object elem @<< get_type ns env sigma elem in
          begin match funct, elem with
          | Some (funct,Features.Functor (src,dst)), Some elem ->
              some (Features.AppliedFunctObj (src,dst,funct,elem))
          | _ -> none ()
          end
      | _ -> none ()
      end
  | _ -> none ()

and query_identity ns env sigma ec tp =
  let module E = struct exception Ret of EConstr.t * EConstr.t end in
  try match EConstr.kind sigma ec with
  | App (id, [| cat; elem |]) ->
      begin match EConstr.kind sigma id with
      | Const (name,_) when Env.is_id name ->
          raise_notrace (E.Ret (cat,elem))
      | _ -> none ()
      end 
  | App (id, [| elem |]) ->
      begin match EConstr.kind sigma id with
      | Proj (id,cat) when Env.is_projection id Env.is_cat "identity" ->
          raise_notrace (E.Ret (cat,elem))
      | _ -> none ()
      end
  | _ -> none ()
  with E.Ret (cat, elem) -> 
    let* cat = ofst <$> query_impl ns env sigma Category cat @<< get_type ns env sigma cat in
    let* elem = ofst <$> query_impl ns env sigma Object elem @<< get_type ns env sigma elem in
    match cat, elem with
    | Some cat, Some elem -> some (Features.Identity (cat, elem))
    | _ -> none ()

and query_compose_mph ns env sigma ec tp =
  match EConstr.kind sigma ec with
  | App (cmp, [| _; _; _; mid; msi |]) ->
    begin match EConstr.kind sigma cmp with
      | Proj (cmp,cat) when Env.is_projection cmp Env.is_cat "compose" -> begin
          let* cat = ofst <$> query_impl ns env sigma Category cat @<< get_type ns env sigma cat in
          let* msi = query_impl ns env sigma Morphism msi @<< get_type ns env sigma msi in
          let* mid = query_impl ns env sigma Morphism mid @<< get_type ns env sigma mid in
          match cat, msi, mid with
          | Some cat
          , Some (msi, Features.Morphism (_,s,i))
          , Some (mid, Features.Morphism (_,_,d)) -> 
            some (Features.ComposeMph (cat,s,i,d,msi,mid))
          | _ -> none ()
      end
      | _ -> none ()
    end
  | _ -> none ()

and query_funct_mph ns env sigma ec tp =
  match EConstr.kind sigma ec with
  | App (funct, [| _; _; mph |]) ->
      begin match EConstr.kind sigma funct with
      | Proj (mof,funct) when Env.is_projection mof Env.is_functor "morphism_of" -> begin
          let* funct = query_impl ns env sigma Functor funct @<< get_type ns env sigma funct in
          let* mph = query_impl ns env sigma Morphism mph @<< get_type ns env sigma mph in
          match funct, mph with
          | Some (funct, Features.Functor (scat,dcat))
          , Some (mph, Features.Morphism (_,src,dst)) -> 
            some (Features.AppliedFunctMph (scat,dcat,funct,src,dst,mph))
          | _ -> none ()
          end
      | _ -> none ()
      end
  | _ -> none ()

and query_impl ns env sigma feat ec tp =
  match feat with
  | Category -> Option.map (fun (id,_) -> (id,Features.Category)) 
                       <$> query_cat_cached ns env sigma tp
  | Object -> Option.map (fun (id,v) -> (id,Features.Object v)) 
                     <$> query_object_cached ns env sigma tp
  | Morphism -> Option.map (fun (id,(cat,src,dst)) -> (id,Features.Morphism (cat,src,dst)))
                       <$> query_morphism_cached ns env sigma tp
  | Functor -> Option.map (fun (id,(src,dst)) -> (id,Features.Functor (src,dst)))
                      <$> query_functor_cached ns env sigma tp
  | Equality -> Option.map (fun (id,(cat,src,dst,left,right)) -> 
                              (id,Features.Equality (cat,src,dst,left,right)))
                       <$> query_eq_cached ns env sigma tp
  | AppliedFunctObj -> register ns ec tp @<< query_funct_obj ns env sigma ec tp
  | Identity -> register ns ec tp @<< query_identity ns env sigma ec tp
  | ComposeMph -> register ns ec tp @<< query_compose_mph ns env sigma ec tp
  | AppliedFunctMph -> register ns ec tp @<< query_funct_mph ns env sigma ec tp
  (* We don't parse equality terms for now *)
  | Reflexivity -> none ()
  | Concat -> none ()
  | InverseEq -> none ()
  | ComposeEq -> none ()
  | Associativity -> none ()
  | LeftUnitality -> none ()
  | RightUnitality -> none ()
  | LeftApplication -> none ()
  | RightApplication -> none ()
  | FunctIdentity -> none ()
  | FunctComposition -> none ()
  | AppliedFunctEq -> none ()

let query ns env feat ec tp =
  let* sigma = evars () in
  query_impl ns env sigma feat ec tp

let query_infer_type ns env feat ec =
  let* sigma = evars () in
  query_impl ns env sigma feat ec @<< get_type ns env sigma ec


(*   ___                    _   _  __ _          _  *)
(*  / _ \ _   _  __ _ _ __ | |_(_)/ _(_) ___  __| | *)
(* | | | | | | |/ _` | '_ \| __| | |_| |/ _ \/ _` | *)
(* | |_| | |_| | (_| | | | | |_| |  _| |  __/ (_| | *)
(*  \__\_\\__,_|\__,_|_| |_|\__|_|_| |_|\___|\__,_| *)
(*                                                  *)

type quantifiedKind =
  | Existential
  | Universal
  | LetIn of obj
type quantified =
  { name: Names.Name.t option
  ; tp: obj
  ; kind: quantifiedKind
  }

let build_env rctx sigma =
  let* env = env () in
  let name = Context.({
    binder_name = Names.Name.Anonymous;
    binder_relevance = Sorts.Irrelevant;
  }) in
  let* rctx = mapM (fun q -> Hyps.getObjValue q.tp) rctx in
  let ctx = List.rev_map (fun tp ->
    Context.Rel.Declaration.LocalAssum (name,EConstr.to_constr sigma tp)) rctx in
  List.fold_left (fun env decl -> Environ.push_rel decl env) env ctx |> ret

let is_some = function
  | Some _ -> true
  | None -> false

let mrgOpts opts =
  let mrg opt1 opt2 = match opt1, opt2 with
  | Some x, _ -> Some x
  | _, Some y -> Some y
  | _ -> None in
  List.fold_left mrg None opts

let is_relevant_type ns rctx sigma tp =
  let* env = build_env rctx sigma in
  let* is_cat = ofst <$> query_cat_cached ns env sigma tp in
  let* is_obj = ofst <$> query_object_cached ns env sigma tp in
  let* is_mph = ofst <$> query_morphism_cached ns env sigma tp in
  let* is_fun = ofst <$> query_functor_cached ns env sigma tp in
  let* is_eq  = ofst <$> query_eq_cached ns env sigma tp in
  mrgOpts [is_cat; is_obj; is_mph; is_fun; is_eq] |> ret

let rec query_lemma_impl ns rctx sigma tp =
  match EConstr.kind sigma tp with
  | Prod (name,arg,body) ->
      let* rel = is_relevant_type ns rctx sigma arg in
      begin match rel with
      | Some id -> begin
          let q = { name = Some name.binder_name; tp = id; kind = Universal } in
          query_lemma_impl ns (q :: rctx) sigma body
      end
      | None -> none ()
      end
  | _ ->
      let* rel = is_relevant_type ns rctx sigma tp in
      match rel with
      | Some id -> some (List.rev rctx, id)
      | None -> none ()

let query_lemma ns tp = 
  let* sigma = evars () in
  query_lemma_impl ns [] sigma tp

