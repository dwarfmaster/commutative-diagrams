
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
  ; tp: obj
  (* If it has no value, it means it must be instantiated as an evar *)
  ; value: obj option
  }

type lemmaConstr =
  | Var of int (* De Bruijn level in quantified list *)
  (* The first argument is the type of the result of the application *)
  | App of lemmaConstr * lemmaConstr * lemmaConstr list
  | Lemma of lemmaTerm
  (* The first integer is an EConstr in the context, and the second one
     is the length of the prefix of the quantified variables that should act as
     a quantifier *)
  | Subst of obj * int

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
    | Subst (ec1,_), Subst (ec2,_) -> Hyps.compare_obj ec1 ec2
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

let add_to_builder ns lc tp dbindex bld =
  let to_lc obj =
    let* vl = Hyps.getObjValue obj in
    let* sigma = evars () in
    match EConstr.kind sigma vl with
    | Rel id -> ret (Var (id - 1))
    | _ -> ret (Subst (obj,dbindex)) in
  Bld.import ns lc tp to_lc bld

(* dbindex is the index in the quantifiers list *)
let handle_quantifier ns q dbindex lemma bld =
  match q.Query.kind with
  | Existential -> assert false
  | Universal ->
      let tp = q.Query.tp in
      let lc = Var dbindex in
      let* bld = add_to_builder ns lc tp dbindex bld in
      let nq = {
        name = Option.map (fun nm -> nm |> Names.Name.print |> Pp.string_of_ppcmds) q.Query.name;
        tp = q.Query.tp;
        value = None;
      } in
      ret (bld, nq)
  | LetIn v -> assert false

let prepare_quantifier i q =
  match q.Query.kind with
  | Existential -> None
  | Universal -> Some (Var i)
  | LetIn _ -> None

let build_lemma ns lemma tp quantifiers =
  let rec handle_quantifiers bld id = function
    | [] -> ret (bld,[])
  | q :: qs ->
      let* (bld,q) = handle_quantifier ns q id lemma bld in
      let* (bld,qs) = handle_quantifiers bld (id + 1) qs in
      ret (bld, q::qs) in
  let* (bld,qs) = handle_quantifiers (Bld.empty ()) 0 quantifiers in
  let args = quantifiers |> List.mapi prepare_quantifier |> List.filter_map (fun x -> x) in
  let lterm = App (Subst (tp,List.length quantifiers), Lemma lemma, args) in
  let* bld = add_to_builder ns lterm tp (List.length quantifiers) bld in
  let name, namespace = mkName lemma in
  let pattern = Bld.build bld in
  match pattern with
  | Some pattern ->
      some {
        name = name;
        namespace = namespace;
        bound = qs;
        pattern = pattern;
        store = ns;
      }
  | None -> none ()

let extractFromType (id: lemmaTerm) (tp: EConstr.t) : lemma option Hyps.t =
  let* ns = Hyps.registerNamespace () in
  let* res = Query.query_lemma ns tp in
  match res with
  | Some (quantifiers, body) -> build_lemma ns id body quantifiers
  | None -> none ()

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
        let* tp = EConstr.Vars.substl subst <$> Hyps.getObjValue q.tp in
        let* vl = match q.value with
        | Some o -> EConstr.Vars.substl subst <$> Hyps.getObjValue o
        | None -> 
            let* env = env () in
            let* sigma = evars () in
            let name = match q.name with
            | Some nm -> Namegen.IntroIdentifier (Names.Id.of_string nm)
            | None -> Namegen.IntroAnonymous in
            let (sigma,evar) = Evarutil.new_evar ~naming:name env sigma tp in
            let* _ = Hyps.setState sigma in
            ret evar in
        let* obj = Hyps.registerObj 0 vl tp q.name in
        bound (obj :: partial) (vl :: subst) left

  let rec drop n = function
    | [] -> []
    | x :: l when n > 0 -> drop (n-1) l
    | l -> l
  let keep n l = drop (List.length l - n) l

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
    | Subst (obj,s) ->
        let subst = keep s subst in
        EConstr.Vars.substl subst <$> Hyps.getObjValue obj

  let obj_of_lconstr subst lc =
    let* ec = lconstr subst lc in
    let* env = env () in
    let* sigma = evars () in
    let* tp = Query.get_type 0 env sigma ec in
    Hyps.registerObj 0 ec tp None
end

let instantiate lm =
  let* (_,subst) = Instantiate.bound [] [] lm.bound in
  Graph.mapM (fun lc -> (fun o -> o.Hyps.id) <$> Instantiate.obj_of_lconstr subst lc) lm.pattern
