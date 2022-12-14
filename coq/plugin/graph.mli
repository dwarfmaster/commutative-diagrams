
module Make(PA : Pa.ProofAssistant) : sig
  type faces =
    { face_src: int
    ; face_dst: int
    ; face_left: int list
    ; face_right: int list
    ; face_eq: PA.t Data.eq
    }
  type graph =
    { gr_nodes: PA.t Data.elem array
    ; gr_edges: (int * (PA.t Data.morphism)) list array
    ; gr_faces: faces list
    }

  module Serde : Serde.Make(PA).Packable with type t = graph
end
