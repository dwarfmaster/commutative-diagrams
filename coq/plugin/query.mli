
val query : Environ.env
         -> Features.Tag.t 
         -> EConstr.t 
         -> EConstr.t 
         -> (int * Features.t) option Hyps.t
val query_infer_type : Environ.env
                    -> Features.Tag.t 
                    -> EConstr.t 
                    -> (int * Features.t) option Hyps.t

