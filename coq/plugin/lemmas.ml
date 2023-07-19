
open Hyps.Combinators
type obj = Hyps.obj

type lemmaTerm =
  | ConstLemma of Names.Constant.t
  | VarLemma of Names.Id.t

let ltcompare lt1 lt2 =
  match lt1, lt2 with
  | ConstLemma cst1, ConstLemma cst2 -> Names.Constant.CanOrd.compare cst1 cst2
  | ConstLemma _, VarLemma _ -> -1
  | VarLemma _, ConstLemma _ -> 1
  | VarLemma v1, VarLemma v2 -> Names.Id.compare v1 v2

type quantified =
  { name: string option
  ; tp: EConstr.t
  (* If it has no value, it means it must be instantiated as an evar *)
  ; value: EConstr.t option
  }

type lemmaConstr =
  | Var of int (* De Bruijn level in quantified list *)
  (* The first argument is the type of the result of the application *)
  | App of lemmaConstr * lemmaConstr * lemmaConstr list
  | Lemma of lemmaTerm
  (* The first integer is an EConstr in the context, and the second one
     is the length of the prefix of the quantified variables that should act as
     a quantifier *)
  | Subst of obj

type lemma =
  { name: string
  ; namespace: string
  ; bound: quantified list
  ; pattern: lemmaConstr Graph.graph_impl
  ; store: int (* The namespace in the store *)
  }
type t = lemma

let name lm = lm.name
let namespace lm = lm.namespace
let context lm = lm.store

module LCOrd = struct
  type t = lemmaConstr
  let lcid = function
    | Var _ -> 0
    | App _ -> 1
    | Lemma _ -> 2
    | Subst _ -> 3
  let rec compare lc1 lc2 =
    let cmp = Int.compare (lcid lc1) (lcid lc2) in
    if cmp = 0 then begin match lc1, lc2 with
    | Var id1, Var id2 -> Int.compare id1 id2
    | App (tp1,lc1,lcs1), App (tp2,lc2,lcs2) -> 
        List.compare compare (tp1 :: lc1 :: lcs1) (tp2 :: lc2 :: lcs2)
    | Lemma lm1, Lemma lm2 -> ltcompare lm1 lm2
    | Subst ec1, Subst ec2 -> Int.compare ec1 ec2
    | _ -> assert false
    end else cmp
end

let mkName ltm =
  match ltm with
  | ConstLemma cst -> 
      cst |> Names.Constant.label |> Names.Label.to_string,
      cst |> Names.Constant.modpath |> Names.ModPath.to_string
  | VarLemma id -> Names.Id.to_string id, ""

module Bld = Graphbuilder.Make(LCOrd)

let add_to_builder lc tp bld =
  let to_lc obj =
    ret (Subst obj) in
  Bld.import lc tp to_lc bld

(* dbindex is the index in the quantifiers list *)
let handle_quantifier q n dbindex lemma env =
  match q.Query.kind with
  | Existential -> assert false
  | Universal ->
      let tp = q.Query.tp in
      let id = n - 1 - dbindex in
      let lctp = EConstr.Vars.lift (id + 1) tp in
      let* lcid = Hyps.registerObj (EConstr.mkRel (id + 1)) lctp None in
      let lc = Subst lcid in
      let nq = {
        name = Option.map (fun nm -> nm |> Names.Name.print |> Pp.string_of_ppcmds) q.Query.name;
        tp = tp;
        value = None;
      } in
      let* sigma = evars () in
      let name = Context.({
        binder_name = Names.Name.Anonymous;
        binder_relevance = Sorts.Irrelevant;
      }) in
      let decl = Context.Rel.Declaration.LocalAssum (name,EConstr.to_constr sigma tp) in
      let env = Environ.push_rel decl env in
      ret (env, (lc,lctp), nq)
  | LetIn v -> assert false

let build_lemma ns lemma tp quantifiers =
  (* Process quantifiers *)
  let rec handle_quantifiers env id = function
    | [] -> ret (env,[],[])
    | q :: qs ->
      let n = List.length quantifiers in
      let* (env,lc,q) = handle_quantifier q n id lemma env in
      let* (env,lcs,qs) = handle_quantifiers env (id + 1) qs in
      ret (env, lc::lcs, q::qs) in
  let* env = env () in
  let* (env,lcs,qs) = handle_quantifiers env 0 quantifiers in

  (* Add quantified elements to the graph *)
  let rec fold_quantified bld = function
    | [] -> ret bld
    | (lc,tp) :: lcs ->
        let* sigma = evars () in
        let* tptp = Query.get_type env sigma tp in
        let* tp = Hyps.registerObj tp tptp None in
        let* bld = add_to_builder lc tp bld in
        fold_quantified bld lcs in
  let* bld = fold_quantified (Bld.empty ()) lcs |> Hyps.withEnv env in

  (* Add the lemma to the graph *)
  let prepare_quantifier i q =
    match q.Query.kind with
    | Existential -> None
    | Universal -> Some (Var i)
    | LetIn _ -> None in
  let args = quantifiers |> List.mapi prepare_quantifier |> List.filter_map (fun x -> x) in
  let lterm = App (Subst tp, Lemma lemma, args) in
  let* bld = add_to_builder lterm tp bld |> Hyps.withEnv env in

  (* Finalize *)
  let name, namespace = mkName lemma in
  let pattern = Bld.build bld in
  match pattern with
  | Some pattern ->
      if List.length pattern.gr_nodes > 0
      then some {
             name = name;
             namespace = namespace;
             bound = qs;
             pattern = pattern;
             store = ns;
           }
      else none ()
  | None -> none ()

let extractFromType (id: lemmaTerm) (tp: EConstr.t) : lemma option Hyps.t =
  let* ns = Hyps.registerNamespace () in
  Hyps.inNamespace ~rollback:false ns begin
    let* res = Query.query_lemma tp in
    match res with
    | Some (quantifiers, body) -> build_lemma ns id body quantifiers
    | None -> none ()
  end

let extractConstant name decl : lemma option Hyps.t =
  extractFromType
    (ConstLemma name)
    (EConstr.of_constr Declarations.(decl.const_type))

let extractFromVar name tp =
  extractFromType (VarLemma name) tp

let extractAllConstants () : lemma list Hyps.t =
  let* env = env () in
  Environ.fold_constants
    (fun name decl mnd ->
      let* lst = mnd in
      let* cst = extractConstant name decl in
      match cst with
      | Some(cst) -> ret (cst :: lst)
      | None -> ret lst)
    env
    (ret [])

module Instantiate = struct
  let rec bound partial subst left =
    match left with
    | [] -> ret (partial,subst)
    | (q :: left) ->
        let tp = EConstr.Vars.substl subst q.tp in
        let* vl = match q.value with
        | Some o -> EConstr.Vars.substl subst o |> ret
        | None -> 
            let* env = env () in
            let* sigma = evars () in
            let name = match q.name with
            | Some nm -> begin
              let base_name = nm in
              let name = ref base_name in
              let count = ref 0 in
              let () = try while true do
                let _ = Evd.evar_key (Names.Id.of_string !name) sigma in
                name := Printf.sprintf "%s%d" base_name !count;
                count := !count  + 1;
              done with Not_found -> () in
              Namegen.IntroIdentifier (Names.Id.of_string !name)
            end
            | None -> Namegen.IntroAnonymous in
            let* evar = 
              Hyps.mapState 
                (fun sigma -> let (sigma, evar) = Evarutil.new_evar ~naming:name env sigma tp
                              in  (evar, sigma)) in
            ret evar in
        let* obj = Hyps.registerObj vl tp q.name in
        bound (obj :: partial) (vl :: subst) left

  let rec lconstr subst lc =
    match lc with
    | Var id -> ret (List.nth subst id)
    | App (tp,f,args) ->
        let* f = lconstr subst f in
        let* args = mapM (lconstr subst) args in
        let* fapp = Env.app (Proofview.tclUNIT f) (Array.of_list args) |> lift in
        ret fapp
    | Lemma lm ->
        ret begin match lm with
        | ConstLemma cst -> EConstr.mkConst cst
        | VarLemma var -> EConstr.mkVar var
        end
    | Subst obj ->
        EConstr.Vars.substl subst <$> Hyps.getObjValue obj

  let obj_of_lconstr subst lc =
    let* ec = lconstr subst lc in
    let* env = env () in
    let* sigma = evars () in
    let* tp = Query.get_type env sigma ec in
    Hyps.registerObj ec tp None
end

let instantiate lm =
  let* (_,subst) = Instantiate.bound [] [] lm.bound in
  Graph.mapM (fun lc -> Instantiate.obj_of_lconstr subst lc) lm.pattern
