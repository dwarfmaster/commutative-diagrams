
open Hyps.Combinators

type lemmaTerm =
  | ConstLemma of Names.Constant.t
  | VarLemma of Names.Id.t

type lemma =
  { name: string
  ; term: lemmaTerm
  ; graph: Graph.graph
  }

let mkName tm =
  match tm with
  | ConstLemma cst -> cst |> Names.Constant.canonical |> Names.KerName.to_string
  | VarLemma id -> Names.Id.to_string id

let mkTerm name =
  match name with
  | ConstLemma cst -> EConstr.mkConstU (cst, EConstr.EInstance.empty)
  | VarLemma id -> EConstr.mkVar id

let buildParsed bld tp =
  let open Hott in
  match tp with
  | Category _ -> bld
  | Functor _ -> bld
  | Elem e -> Graphbuilder.add_node e bld |> snd
  | Morphism m -> Graphbuilder.add_edge m bld
  | Equality eq -> Graphbuilder.add_face eq bld

let rec buildLemma bld lemma =
  let open Hott in
  match lemma with
  | Prod (_,tp,lemma) ->
      let bld = buildParsed bld tp in
      buildLemma bld lemma
  | Exists (_,tp,lemma) ->
      let bld = buildParsed bld tp in
      buildLemma bld lemma
  | Result tp ->
      buildParsed bld tp

let extractFromType (id: lemmaTerm) (tp: EConstr.t) : lemma option Hyps.t =
  let* res = Hott.parseLemma (mkTerm id) tp in
  match res with
  | Some lemma ->
      let bld = buildLemma (Graphbuilder.empty ()) lemma in
      let name = mkName id in
      some { name = name
           ; term = id
           ; graph = Graphbuilder.build bld
           }
  | None -> none ()

let extractConstant name decl : lemma option Hyps.t =
  extractFromType (ConstLemma name)
    (EConstr.of_constr (Declarations.(decl.const_type)))

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
