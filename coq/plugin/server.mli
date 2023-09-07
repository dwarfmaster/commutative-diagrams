
val run : ?file:string option 
       -> ?script:string option
       -> ?force:bool 
       -> Graph.graph 
       -> Lemmas.t list 
       -> unit Hyps.t
