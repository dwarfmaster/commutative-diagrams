
val query : Environ.env
         -> Features.Tag.t 
         -> EConstr.t 
         -> EConstr.t 
         -> (int * Features.t) option Hyps.t
val query_infer_type : Environ.env
                    -> Features.Tag.t 
                    -> EConstr.t 
                    -> (int * Features.t) option Hyps.t

type quantifiedKind =
  | Existential
  | Universal
  | LetIn of int
type quantified =
  { name: Names.Name.t option
  ; tp: int
  ; kind: quantifiedKind
  }
val query_lemma : EConstr.t -> (quantified list * int) option Hyps.t

