
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
let get_type_uncached env sigma e : EConstr.t =
  Retyping.get_type_of env sigma e

let get_type env sigma e =
  let* id = Hyps.hasObject e in
  match id with
  | Some id -> Hyps.getObjType id
  | None -> get_type_uncached env sigma e |> ret


(*   ___                         *)
(*  / _ \ _   _  ___ _ __ _   _  *)
(* | | | | | | |/ _ \ '__| | | | *)
(* | |_| | |_| |  __/ |  | |_| | *)
(*  \__\_\\__,_|\___|_|   \__, | *)
(*                        |___/  *)
type obj = Hyps.obj

let do_query (ec: EConstr.t) (tp: EConstr.t) 
             (mk_feat: 'a -> Features.t) 
             (cached: (obj * 'a) option) 
             : (obj * Features.t) option Hyps.t =
  match cached with
  | None -> none ()
  | Some cached ->
      let* has_obj = Hyps.hasObject ec in
      let* id = match has_obj with
      | Some id -> ret id
      | None -> Hyps.registerObj ec tp None in
      cached |> (fun (_,f) -> (id, mk_feat f)) |> some

let run_query_cached env sigma tp
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
      registerQuery
        (fun () -> let* tptp = get_type env sigma tp in Hyps.registerObj tp tptp None) q

let register ec tp (q : 'a option) : (obj * 'a) option Hyps.t =
  match q with
  | Some q ->
      let* id = Hyps.registerObj ec tp None in
      some (id,q)
  | None -> none ()

let rec query_cat env sigma tp =
  if isConst sigma Env.is_cat tp then some () else none ()
and query_cat_cached env sigma tp =
  run_query_cached env sigma tp
    (query_cat env sigma)
    (fun () -> some ())
    (fun mtdt -> mtdt.is_cat)
    Hyps.markAsCat

and extract_cat_impl env sigma cat tp =
  match EConstr.kind sigma cat with
  | App (coercion, [| cat |]) when isConst sigma Env.is_cat_ob_mor_from_data coercion ->
      extract_cat_impl env sigma cat @<< get_type env sigma cat
  | App (coercion, [| cat |]) when isConst sigma Env.is_cat_data_from_precat coercion ->
      extract_cat_impl env sigma cat @<< get_type env sigma cat
  | _ -> ret (cat, tp)

and extract_cat env sigma cat tp =
  let* (cat,tp) = extract_cat_impl env sigma cat tp in
  query_impl env sigma Category cat tp

and query_object env sigma tp =
  match EConstr.kind sigma tp with
  | App (ob, [| cat |]) when isConst sigma Env.is_object ob ->
      some cat
  | _ -> none ()
and query_object_cached env sigma tp =
  run_query_cached env sigma tp
    (query_object env sigma)
    (fun cat -> ofst <$> extract_cat env sigma cat @<< get_type env sigma cat)
    (fun mtdt -> mtdt.is_elem)
    Hyps.markAsElem

and query_morphism env sigma tp =
  match EConstr.kind sigma tp with
  | App (m, [| cat; src; dst |]) when isConst sigma Env.is_mphT m ->
      some (cat,src,dst)
  | _ -> none ()
and query_morphism_cached env sigma tp =
  run_query_cached env sigma tp
    (query_morphism env sigma)
    (fun (cat, src, dst) ->
      let* cat = ofst <$> extract_cat env sigma cat @<< get_type env sigma cat in
      let* tp = get_type env sigma src in
      let* src = ofst <$> query_impl env sigma Object src @<< get_type env sigma src in
      let* dst = ofst <$> query_impl env sigma Object dst @<< get_type env sigma dst in
      match cat, src, dst with
      | Some cat, Some src, Some dst -> some (cat, src, dst)
      | _ -> none ())
    (fun mtdt -> mtdt.is_mph)
    Hyps.markAsMph

and query_functor env sigma tp =
  match EConstr.kind sigma tp with
  | App (funct, [| src; dst |]) when isConst sigma Env.is_functor funct ->
      some (src,dst)
  | _ -> none ()
and query_functor_cached env sigma tp =
  run_query_cached env sigma tp
    (query_functor env sigma)
    (fun (src,dst) ->
      let* src = ofst <$> extract_cat env sigma src @<< get_type env sigma src in
      let* dst = ofst <$> extract_cat env sigma dst @<< get_type env sigma dst in
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
          let* tp = query_morphism env sigma tp in 
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
      let query f x = ofst <$> query_impl env sigma f x @<< get_type env sigma x in
      let* cat = ofst <$> extract_cat env sigma cat @<< get_type env sigma cat in
      let* src = query Object src in
      let* dst = query Object dst in
      let* left = query Morphism left in
      let* right = query Morphism right in
      match cat, src, dst, left, right with
      | Some cat, Some src, Some dst, Some left, Some right -> some (cat, src, dst, left, right)
      | _ -> none ())
    (fun mtdt -> mtdt.is_eq)
    Hyps.markAsEq

and query_funct_obj env sigma ec tp =
  match EConstr.kind sigma ec with
  | App (fobj, [| funct; elem |]) when isConst sigma Env.is_funct_obj fobj ->
      let* funct = query_impl env sigma Functor funct @<< get_type env sigma funct in
      let* elem = ofst <$> query_impl env sigma Object elem @<< get_type env sigma elem in
      begin match funct, elem with
      | Some (funct,Features.Functor (src,dst)), Some elem ->
          some (Features.AppliedFunctObj (src,dst,funct,elem))
      | _ -> none ()
      end
  | _ -> none ()

and query_identity env sigma ec tp =
  let module E = struct exception Ret of EConstr.t * EConstr.t end in
  try match EConstr.kind sigma ec with
  | App (id, [| cat; elem |]) when isConst sigma Env.is_id id ->
      raise_notrace (E.Ret (cat,elem))
  | _ -> none ()
  with E.Ret (cat, elem) -> 
    let* cat = ofst <$> extract_cat env sigma cat @<< get_type env sigma cat in
    let* elem = ofst <$> query_impl env sigma Object elem @<< get_type env sigma elem in
    match cat, elem with
    | Some cat, Some elem -> some (Features.Identity (cat, elem))
    | _ -> none ()

and query_compose_mph env sigma ec tp =
  let module E = struct exception Ret of EConstr.t * EConstr.t * EConstr.t end in
  try match EConstr.kind sigma ec with
  | App (cmp, [| cat; _; _; _; msi; mid |]) when isConst sigma Env.is_comp cmp ->
      raise_notrace (E.Ret (cat, mid, msi))
  | _ -> none ()
  with E.Ret (cat, mid, msi) ->
    let* cat = ofst <$> extract_cat env sigma cat @<< get_type env sigma cat in
    let* tp = get_type env sigma msi in
    let* msi = query_impl env sigma Morphism msi @<< get_type env sigma msi in
    let* mid = query_impl env sigma Morphism mid @<< get_type env sigma mid in
    match cat, msi, mid with
    | Some cat
    , Some (msi, Features.Morphism (_,s,i))
    , Some (mid, Features.Morphism (_,_,d)) -> 
      some (Features.ComposeMph (cat,s,i,d,msi,mid))
    | _ -> none ()

and query_funct_mph env sigma ec tp =
  match EConstr.kind sigma ec with
  | App (fmph, [| funct; _; _; mph |]) when isConst sigma Env.is_funct_mph fmph ->
      let* funct = query_impl env sigma Functor funct @<< get_type env sigma funct in
      let* mph = query_impl env sigma Morphism mph @<< get_type env sigma mph in
      begin match funct, mph with
      | Some (funct, Features.Functor (scat,dcat))
      , Some (mph, Features.Morphism (_,src,dst)) -> 
        some (Features.AppliedFunctMph (scat,dcat,funct,src,dst,mph))
      | _ -> none ()
      end
  | _ -> none ()

and query_impl env sigma feat ec tp =
  match feat with
  | Category -> do_query ec tp (fun _ -> Features.Category) 
                         @<< query_cat_cached env sigma tp
  | Object -> do_query ec tp (fun v -> Features.Object v)
                       @<< query_object_cached env sigma tp
  | Morphism -> do_query ec tp (fun (cat,src,dst) -> Features.Morphism (cat,src,dst))
                         @<< query_morphism_cached env sigma tp
  | Functor -> do_query ec tp (fun (src,dst) -> Features.Functor (src,dst))
                        @<< query_functor_cached env sigma tp
  | Equality -> do_query ec tp (fun (cat,src,dst,left,right) -> 
                                     Features.Equality (cat,src,dst,left,right))
                         @<< query_eq_cached env sigma tp
  | AppliedFunctObj -> register ec tp @<< query_funct_obj env sigma ec tp
  | Identity -> register ec tp @<< query_identity env sigma ec tp
  | ComposeMph -> register ec tp @<< query_compose_mph env sigma ec tp
  | AppliedFunctMph -> register ec tp @<< query_funct_mph env sigma ec tp
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

let query env feat ec tp =
  let* sigma = evars () in
  query_impl env sigma feat ec tp

let query_infer_type env feat ec =
  let* sigma = evars () in
  query_impl env sigma feat ec @<< get_type env sigma ec


(*   ___                    _   _  __ _          _  *)
(*  / _ \ _   _  __ _ _ __ | |_(_)/ _(_) ___  __| | *)
(* | | | | | | |/ _` | '_ \| __| | |_| |/ _ \/ _` | *)
(* | |_| | |_| | (_| | | | | |_| |  _| |  __/ (_| | *)
(*  \__\_\\__,_|\__,_|_| |_|\__|_|_| |_|\___|\__,_| *)
(*                                                  *)

type property =
  | Cat
  | Funct of EConstr.t * EConstr.t
  | Elem of EConstr.t
  | Mph of EConstr.t * EConstr.t * EConstr.t
  | Eq of EConstr.t * EConstr.t * EConstr.t * EConstr.t * EConstr.t
type quantifiedKind =
  | Existential
  | Universal
  | LetIn of EConstr.t
type quantified =
  { name: Names.Name.t option
  ; tp: EConstr.t
  ; props: property list
  ; kind: quantifiedKind
  }

let build_env rctx sigma =
  let* env = env () in
  let name = Context.({
    binder_name = Names.Name.Anonymous;
    binder_relevance = Sorts.Irrelevant;
  }) in
  let rctx = List.map (fun q -> q.tp) rctx in
  let ctx = List.rev_map (fun tp ->
    Context.Rel.Declaration.LocalAssum (name,EConstr.to_constr sigma tp)) rctx in
  List.fold_left (fun env decl -> Environ.push_rel decl env) env ctx |> ret

let is_some = function
  | Some _ -> true
  | None -> false

let to_prop f = function
  | Some x -> Some (f x)
  | None -> None

let is_relevant_object env sigma tp =
  let* c = query_object env sigma tp in
  match c with
  | Some c -> 
      let* (c,_) = extract_cat_impl env sigma c (get_type_uncached env sigma c) in
      some c
  | None -> none ()

let is_relevant_morphism env sigma tp =
  let* m = query_morphism env sigma tp in
  match m with
  | Some (c,x,y) ->
      let* (c,_) = extract_cat_impl env sigma c (get_type_uncached env sigma c) in
      some (c,x,y)
  | None -> none ()

let is_relevant_functor env sigma tp =
  let* f = query_functor env sigma tp in
  match f with
  | Some (c,d) ->
      let* (c,_) = extract_cat_impl env sigma c (get_type_uncached env sigma c) in
      let* (d,_) = extract_cat_impl env sigma d (get_type_uncached env sigma d) in
      some (c,d)
  | None -> none ()

let is_relevant_eq env sigma tp =
  let* e = query_eq env sigma tp in
  match e with
  | Some (c,x,y,l,r) ->
      let* (c,_) = extract_cat_impl env sigma c (get_type_uncached env sigma c) in
      some (c,x,y,l,r)
  | None -> none ()

let is_relevant_type rctx sigma tp =
  let* env = build_env rctx sigma in
  let* is_cat = to_prop (fun x -> Cat)
              <$> query_cat env sigma tp in
  let* is_obj = to_prop (fun x -> Elem x)
              <$> is_relevant_object env sigma tp in
  let* is_mph = to_prop (fun (c,x,y) -> Mph (c,x,y))
              <$> is_relevant_morphism env sigma tp in
  let* is_fun = to_prop (fun (c,d) -> Funct (c,d))
              <$> is_relevant_functor env sigma tp in
  let* is_eq  = to_prop (fun (c,x,y,l,r) -> Eq (c,x,y,l,r))
              <$> is_relevant_eq env sigma tp in
  List.filter_map (fun x -> x) [is_cat; is_obj; is_mph; is_fun; is_eq] |> ret

let apply_property_impl ~lift env sigma obj prop =
  let do_lift x =
    match lift with
    | Some l -> EConstr.Vars.lift l x 
    | None -> x in
  let reg x =
    let xt = get_type_uncached env sigma x in
    let x_l = do_lift x in
    let xt_l = do_lift xt in
    Hyps.registerObj x_l xt_l None in
  match prop with
  | Cat -> Hyps.markAsCat obj ()
  | Funct (c,d) ->
      let* c = reg c in
      let* d = reg d in
      Hyps.markAsFunct obj (c,d)
  | Elem c ->
      let* c = reg c in
      Hyps.markAsElem obj c
  | Mph (c,x,y) ->
      let* c = reg c in
      let* x = reg x in
      let* y = reg y in
      Hyps.markAsMph obj (c,x,y)
  | Eq (c,x,y,l,r) ->
      let* c = reg c in
      let* x = reg x in
      let* y = reg y in
      let* l = reg l in
      let* r = reg r in
      Hyps.markAsEq obj (c,x,y,l,r)

let apply_property ~lift obj prop =
  let* env = env () in
  let* sigma = evars () in
  apply_property_impl ~lift env sigma obj prop

let rec query_lemma_impl rctx sigma tp =
  match EConstr.kind sigma tp with
  | Prod (name,arg,body) ->
      let* props = is_relevant_type rctx sigma arg in
      let q = { name = Some name.binder_name; tp = arg; props = props; kind = Universal } in
      query_lemma_impl (q :: rctx) sigma body
  | _ ->
      let* props = is_relevant_type rctx sigma tp in
      if List.length props > 0
      then
        let* env = build_env rctx sigma in
        let* tptp = get_type env sigma tp in
        let* id = Hyps.registerObj tp tptp None in
        let* _ = mapM (apply_property_impl ~lift:None env sigma id) props in
        some (List.rev rctx, id)
      else
        none ()

let query_lemma tp = 
  let* sigma = evars () in
  query_lemma_impl [] sigma tp

