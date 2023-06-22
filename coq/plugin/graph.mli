
type 'a obj =
  { obj_obj: 'a
  ; obj_cat: 'a
  }
type 'a face =
  { face_src: int
  ; face_dst: int
  ; face_left: int list
  ; face_right: int list
  ; face_eq: 'a
  }
type 'a morphism =
  { mph_src: int
  ; mph_dst: int
  ; mph_mph: 'a
  }
type 'a graph_impl =
  { gr_nodes: 'a obj list
  ; gr_edges: 'a morphism list
  ; gr_faces: 'a face list
  }
type graph = int graph_impl

module Serde : Serde.Packable with type t = graph

val mapM : ('a -> 'b Hyps.t) -> 'a graph_impl -> 'b graph_impl Hyps.t
