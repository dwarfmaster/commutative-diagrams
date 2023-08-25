
val query : Environ.env
         -> Features.Tag.t 
         -> EConstr.t 
         -> EConstr.t 
         -> (Hyps.obj * Features.t) option Hyps.t
val query_infer_type : Environ.env
                    -> Features.Tag.t 
                    -> EConstr.t 
                    -> (Hyps.obj * Features.t) option Hyps.t

type property
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
val query_lemma : EConstr.t -> (quantified list * Hyps.obj) option Hyps.t
val apply_property : lift:(int option) -> Hyps.obj -> property -> unit Hyps.t

val get_type : Environ.env -> Evd.evar_map -> EConstr.t -> EConstr.t Hyps.t
val get_type_uncached : Environ.env -> Evd.evar_map -> EConstr.t -> EConstr.t
