
From HoTT Require Import Basics.
From HoTT Require Import Categories.
From HoTT Require Import Spaces.Finite.
From HoTT Require Import Spaces.Nat.
From HoTT Require Import Spaces.List.

Section Diagrams.
  Variable C : PreCategory.
  Variable Node Vertex Face : Type.
  Variable Index : Type.
  Variable Hid : DecidablePaths Index.

  Record NodeData :=
    mkNodeData
      { node_object : object C
      ; node_name   : option Index
      }.
  Record VertexData {nodes : Node -> NodeData} :=
    mkVertexData
      { vertex_src : Node
      ; vertex_dst : Node
      ; vertex_mph : morphism C (node_object (nodes vertex_src)) (node_object (nodes vertex_dst))
      ; vertex_name : option Index
      }.
  Arguments VertexData nodes : clear implicits.
  Definition eqToIso {a b : object C} (p : a = b) : morphism C a b :=
    paths_ind a (fun b _ => morphism C a b) 1%morphism b p.
  Definition conjugate {a b c d : object C} (f : morphism C b c) (p1 : a = b) (p2 : d = c) :
    morphism C a d :=
    eqToIso p2^ o f o (eqToIso p1).
  Record FaceData {nodes : Node -> NodeData} {vertices : Vertex -> VertexData nodes} :=
    mkFaceData
      { face_src : Vertex
      ; face_dst : Vertex
      ; face_src_eq : node_object (nodes (vertex_src (vertices face_src)))
                      = node_object (nodes (vertex_src (vertices face_dst)))
      ; face_dst_eq : node_object (nodes (vertex_dst (vertices face_src)))
                      = node_object (nodes (vertex_dst (vertices face_dst)))
      ; face : vertex_mph (vertices face_src)
               = conjugate (vertex_mph (vertices face_dst)) face_src_eq face_dst_eq
      ; face_name : option Index
      }.
  Arguments FaceData nodes vertices : clear implicits.
  Record Diagram :=
    mkDiagram
      { gr_node : Node -> NodeData
      ; gr_vertex : Vertex -> VertexData gr_node
      ; gr_face : Face -> FaceData gr_node gr_vertex
      }.
End Diagrams.

Section Combinators.
  Context {C : PreCategory}.
  Context {Node1 Node2 Vertex1 Vertex2 Face1 Face2 : Type}.
  Context {Index : Type}.
  Context {Hid : DecidablePaths Index}.

  Variable d1 : Diagram C Node1 Vertex1 Face1 Index.
  Variable d2 : Diagram C Node2 Vertex2 Face2 Index.

  Ltac empty_ind := apply Empty_ind; assumption.

  Definition emptyD : Diagram C Empty Empty Empty Index.
  Proof. srapply mkDiagram; intros; empty_ind. Defined.

  Definition singletonD (x : object C) (label : option Index) :
    Diagram C Unit Empty Empty Index.
  Proof.
    srapply mkDiagram; intros; [ | empty_ind | empty_ind ].
    srapply mkNodeData; [ exact x | exact label ].
  Defined.

  Definition mkSimpleNodeData : object C -> NodeData C Index :=
    fun x => mkNodeData C Index x None.

  Definition arrowD {s d : object C} (m : morphism C s d) (label : option Index) :
    Diagram C (Fin 2) Unit Empty Index.
  Proof.
    srapply mkDiagram; [ | | empty_ind ].
    - intro x. destruct x; srapply mkSimpleNodeData; [ exact d | exact s ].
    - intros _. srapply mkVertexData; [ exact (inr tt) | exact (inl (inr tt)) | exact m | exact label ].
  Defined.

  Definition faceD {s d : object C} {m1 m2 : morphism C s d} (f : m1 = m2) (label : option Index) :
    Diagram C (Fin 2) (Fin 2) Unit Index.
  Proof.
    srapply mkDiagram.
    - intro x. destruct x; srapply mkSimpleNodeData; [ exact d | exact s ].
    - intro x. srapply mkVertexData; [ exact (inr tt) | exact (inl (inr tt)) | | exact None ].
      destruct x; [ exact m2 | exact m1 ].
    - intros _. srapply mkFaceData; [ exact (inr tt) | exact (inl (inr tt)) | exact 1 | exact 1 | | exact label ].
      unfold conjugate. simpl. rewrite left_identity. rewrite right_identity. exact f.
  Defined.

End Combinators.
