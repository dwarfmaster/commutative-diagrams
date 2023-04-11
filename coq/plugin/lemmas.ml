
open Hyps.Combinators

type lemmaTerm =
  | ConstLemma of Names.Constant.t
  | VarLemma of Names.Id.t

type lemma =
  { name: string
  ; graph: Graph.graph
  }

let mkName ltm =
  match ltm with
  | ConstLemma cst -> 
      cst |> Names.Constant.canonical |> Names.KerName.to_string
  | VarLemma id -> Names.Id.to_string id

let mkTerm ltm =
  match ltm with
  | ConstLemma cst -> Data.FnConst cst |> ret
  | VarLemma id -> Data.FnVar id |> ret

let buildParsed bld tp =
  let open Hott in
  match tp with
  | Category _ -> ret bld
  | Functor _ -> ret bld
  | Elem e -> Graphbuilder.add_node e bld |> snd |> ret
  | Morphism m -> Graphbuilder.add_edge m bld |> ret
  | Equality eq ->
      let* sigma = evars () in
      let* env = env () in
      Graphbuilder.add_face eq bld |> ret

let rec buildLemma bld lemma =
  let open Hott in
  match lemma with
  | Prod (_,tp,lemma) ->
      let* bld = buildParsed bld tp in
      buildLemma bld lemma
  | Exists (_,tp,lemma) ->
      let* bld = buildParsed bld tp in
      buildLemma bld lemma
  | Result tp ->
      buildParsed bld tp

let extractFromType (id: lemmaTerm) (tp: EConstr.t) : lemma option Hyps.t =
  let* term = mkTerm id in
  let* res = Hott.parseLemma term tp in
  match res with
  | Some lemma ->
      let name = mkName id in
      let* bld = buildLemma (Graphbuilder.empty ()) lemma in
      let* env = env () in
      let* sigma = evars () in
      some { name = name
           ; graph = Graphbuilder.build bld
           }
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
