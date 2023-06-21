
val query : int
         -> Environ.env
         -> Features.Tag.t 
         -> EConstr.t 
         -> EConstr.t 
         -> (Hyps.obj * Features.t) option Hyps.t
val query_infer_type : int
                    -> Environ.env
                    -> Features.Tag.t 
                    -> EConstr.t 
                    -> (Hyps.obj * Features.t) option Hyps.t

type quantifiedKind =
  | Existential
  | Universal
  | LetIn of Hyps.obj
type quantified =
  { name: Names.Name.t option
  ; tp: Hyps.obj
  ; kind: quantifiedKind
  }
val query_lemma : int -> EConstr.t -> (quantified list * Hyps.obj) option Hyps.t

val get_type : int -> Environ.env -> Evd.evar_map -> EConstr.t -> EConstr.t Hyps.t