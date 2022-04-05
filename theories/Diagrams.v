
From HoTT Require Import Basics.
From HoTT Require Import Categories.
From HoTT Require Import Types.Sigma.
From HoTT Require Import Spaces.Finite.
From HoTT Require Import Spaces.Nat.
From HoTT Require Import Spaces.List.

Section List.
  Context {A : Type}.
  Fixpoint length (l : list A) : nat :=
    match l with
    | nil => 0
    | cons _ l => (length l) .+1
    end.

  Fixpoint getO (l : list A) (n : nat) : option A :=
    match l,n with
    | cons v _, O => Some v
    | cons _ l, S n => getO l n
    | nil, _ => None
    end.

  Definition get' (l : list A) (n : nat) (H : (S n <= length l)%nat) : A.
  Proof.
    generalize dependent n. induction l; intros n H.
    - apply Empty_ind. exact (not_leq_Sn_0 _ H).
    - destruct n; [ exact a | ]. apply (IHl n). exact (leq_S_n _ _ H).
  Defined.

  Definition idof (l : list A) := { n : nat & (S n <= length l)%nat }.
  Definition get {l : list A} (n : idof l) : A := get' l (n.1) (n.2).
  Definition Build_idof (l : list A) (n : nat) : (S n <= length l)%nat -> idof l.
  Proof. intro H. exists n. assumption. Defined.
  Lemma path_idof {l : list A} (id1 id2 : idof l) : id1.1 = id2.1 -> id1 = id2.
  Proof. intro p. apply (path_sigma _ _ _ p). apply center. typeclasses eauto. Defined.
End List.

Section Diagrams.
  Variable C : PreCategory.
  Variable Index : Type.
  Variable Hid : DecidablePaths Index.

  Record NodeData :=
    mkNodeData
      { node_object : object C
      ; node_name   : option Index
      }.
  Record Arrow :=
    mkArrow
      { arrow_src : object C
      ; arrow_dst : object C
      ; arrow_mph : morphism C arrow_src arrow_dst
      }.
  Arguments mkArrow {src dst} mph : rename.
  Record VertexData {nodes : list NodeData} :=
    mkVertexData
      { vt_src_id : idof nodes
      ; vt_dst_id : idof nodes
      ; vt_mph    : Arrow
      ; vt_src_eq : node_object (get vt_src_id) = arrow_src vt_mph
      ; vt_dst_eq : node_object (get vt_dst_id) = arrow_dst vt_mph
      ; vt_name   : option Index
      }.
  Arguments VertexData nodes : clear implicits.

  Definition eqToIso {a b : object C} (p : a = b) : morphism C a b :=
    paths_ind a (fun b _ => morphism C a b) 1%morphism b p.
  Definition conjugate {a b c d : object C} (f : morphism C b c) (p1 : a = b) (p2 : d = c) :
    morphism C a d :=
    eqToIso p2^ o f o (eqToIso p1).
  Inductive Path {nodes : list NodeData} {vertices : list (VertexData nodes)} : idof nodes -> Type :=
  | PNil : forall(id : idof nodes), Path id
  | PCons : forall(v : idof vertices), Path (vt_dst_id (get v)) -> Path (vt_src_id (get v)).
  Arguments Path nodes vertices : clear implicits.
  Definition path_src {nodes : list NodeData} {vertices : list (VertexData nodes)} {src : idof nodes}
    : Path nodes vertices src -> idof nodes := fun _ => src.
  Fixpoint path_dst {nodes : list NodeData} {vertices : list (VertexData nodes)} {src : idof nodes}
           (p : Path nodes vertices src) : idof nodes :=
    match p with
    | PNil id => id
    | PCons _ p => path_dst p
    end.
  Fixpoint path_mph {nodes : list NodeData} {vertices : list (VertexData nodes)}
           {src : idof nodes} (p : Path nodes vertices src)
    : morphism C (node_object (get (path_src p))) (node_object (get (path_dst p)))
    := match p return morphism C (node_object (get (path_src p))) (node_object (get (path_dst p))) with
       | PNil id => identity (node_object (get id))
       | PCons v p => path_mph p
                   o conjugate (arrow_mph (vt_mph (get v))) (vt_src_eq (get v)) (vt_dst_eq (get v))
       end.
  Definition eqToIso' {nodes : list NodeData} {i1 i2 : idof nodes}
    : i1 = i2 -> morphism C (node_object (get i1)) (node_object (get i2)) :=
    fun p => eqToIso (ap (fun i => node_object (get i)) p).

  Record FaceData {nodes : list NodeData} {vertices : list (VertexData nodes)} :=
    mkFaceData
      { face_src : idof nodes
      ; face_side1 : Path nodes vertices face_src
      ; face_side2 : Path nodes vertices face_src
      ; face_dst_eq : path_dst face_side1 = path_dst face_side2
      ; face : (eqToIso' face_dst_eq o path_mph face_side1)%morphism = path_mph face_side2
      ; face_name : option Index
      }.
  Arguments FaceData nodes vertices : clear implicits.
  Record Diagram :=
    mkDiagram
      { gr_node : list NodeData
      ; gr_vertex : list (VertexData gr_node)
      ; gr_face : list (FaceData gr_node gr_vertex)
      }.
End Diagrams.

Section Combinators.
  Context {C : PreCategory}.
  Context {Index : Type}.
  Context {Hid : DecidablePaths Index}.

  Variable d1 : Diagram C Index.
  Variable d2 : Diagram C Index.

  Ltac empty_ind := apply Empty_ind; assumption.

  Definition emptyD : Diagram C Index.
  Proof. srapply mkDiagram; exact nil. Defined.

  Definition singletonD (x : object C) (label : option Index) :
    Diagram C Index.
  Proof.
    srapply mkDiagram; intros; [ refine (cons _ nil) | exact nil | exact nil ].
    srapply mkNodeData; [ exact x | exact label ].
  Defined.

  Definition mkSimpleNodeData : object C -> NodeData C Index :=
    fun x => mkNodeData C Index x None.
  Print leq.
  Ltac solve_leq :=
    match goal with
    | |- leq ?n ?n => exact (leq_n n)
    | |- leq ?n (S ?m) => refine (leq_S n m _); solve_leq
    end.
  Ltac mkid n := exists n; simpl; solve_leq.

  Definition arrowD {s d : object C} (m : morphism C s d) (label : option Index) :
    Diagram C Index.
  Proof.
    srapply mkDiagram;
      [ refine (cons _ (cons _ nil)); srapply mkSimpleNodeData; [ exact s | exact d ]
      | refine (cons _ nil) | exact nil ].
    srapply mkVertexData;
      [ mkid 0%nat | mkid 1%nat | exact (mkArrow C s d m)
      | exact 1 | exact 1 | exact label ].
  Defined.

  Definition mkSingletonPath {nodes : list (NodeData C Index)} {vertices : list (VertexData C Index)}
             (i : idof vertices) (j : idof nodes) (p : j = vt_src_id C Index (get i))
    : @Path C Index nodes vertices j.
  Proof. rewrite p. srapply PCons. apply PNil. Defined.

  Definition faceD {s d : object C} {m1 m2 : morphism C s d} (f : m1 = m2) (label : option Index) :
    Diagram C Index.
  Proof.
    srapply mkDiagram;
      [ refine (cons _ (cons _ nil)); srapply mkSimpleNodeData; [ exact s | exact d ] | | ].
    - refine (cons _ (cons _ nil));
        (srapply mkVertexData;
         [ mkid 0%nat | mkid 1%nat | refine (mkArrow C s d _) | exact 1 | exact 1 | exact None ]);
        [ exact m1 | exact m2 ].
    - refine (cons _ nil); srapply mkFaceData.
      + mkid 0%nat.
      + srapply mkSingletonPath; [ mkid 0%nat | exact 1 ].
      + srapply mkSingletonPath; [ mkid 1%nat | exact 1 ].
      + exact 1.
      + simpl; unfold conjugate; unfold eqToIso'; unfold eqToIso; simpl.
        rewrite left_identity. rewrite <- f. exact 1.
      + exact label.
  Defined.

End Combinators.
