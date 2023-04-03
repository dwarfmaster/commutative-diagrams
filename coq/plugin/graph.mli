
type faces =
  { face_src: int
  ; face_dst: int
  ; face_left: int list
  ; face_right: int list
  ; face_eq: Data.eq
  }
type graph =
  { gr_nodes: Data.elem array
  ; gr_edges: (int * Data.morphism) list array
  ; gr_faces: faces list
  }

module Serde : Serde.Packable with type t = graph
