
From HoTT Require Import Basics.
From HoTT Require Import Categories.
From HoTT Require Import Categories.Category.Morphisms.
Declare ML Module "diagrams_plugin".
Local Open Scope morphism.

Definition mphT (C : PreCategory) (a b : C) := @morphism C a b.
Definition comp (C : PreCategory) (a b c : C) (m1 : mphT C a b) (m2 : mphT C b c) : mphT C a c :=
  @HoTT.Categories.compose C a b c m2 m1.

Lemma compose_eq (C : PreCategory) (a b c : C) (m1 m2 : morphism C a b) (m3 m4 : morphism C b c)
      (p1 : m1 = m2) (p2 : m3 = m4) : comp C a b c m1 m3 = comp C a b c m2 m4.
Proof. f_ap. Defined.
Definition assoc (C : PreCategory) (a b c d : C)
      (m1 : morphism C a b) (m2 : morphism C b c) (m3 : morphism C c d) :
  comp C a c d (comp C a b c m1 m2) m3 = comp C a b d m1 (comp C b c d m2 m3)
  := (associativity C a b c d m1 m2 m3)^.

Definition id (C : PreCategory) (a : C) : morphism C a a := @identity C a.
Definition left_id (C : PreCategory) (a b : C) (m : morphism C a b) :
  comp C a b b m (id C b) = m := left_identity C a b m.
Definition right_id (C : PreCategory) (a b : C) (m : morphism C a b) :
  comp C a a b (id C a) m = m := right_identity C a b m.

Definition r_ap (C : PreCategory) (a b c : C) (m : morphism C a b) (m1 m2 : morphism C b c) :
  m1 = m2 -> m1 o m = m2 o m := fun p => ap (fun m' => m' o m) p.
Definition l_ap (C : PreCategory) (a b c : C) (m1 m2 : morphism C a b) (m : morphism C b c) :
  m1 = m2 -> m o m1 = m o m2 := fun p => ap (fun m' => m o m') p.

Definition mph_inv (C : PreCategory) (a b : C) (m : morphism C a b) (H : IsIsomorphism m) :
  morphism C b a := @morphism_inverse C a b m H.
Definition right_inv (C : PreCategory) (a b : C) (m : morphism C a b) (H : IsIsomorphism m) :
  m^-1 o m = 1 :=
  @left_inverse C a b m H.
Definition left_inv (C : PreCategory) (a b : C) (m : morphism C a b) (H : IsIsomorphism m) :
  m o m^-1 = 1 :=
  @right_inverse C a b m H.

Definition funct_obj (C D : PreCategory) (F : Functor C D) (x : object C) : object D :=
  @object_of C D F x.
Definition funct_mph (C D : PreCategory) (F : Functor C D) (x y : object C) (m : morphism C x y) :
  morphism D (funct_obj C D F x) (funct_obj C D F y) :=
  @morphism_of C D F x y m.
Definition funct_id (C D : PreCategory) (F : Functor C D) (x : object C) :
  F _1 (identity x) = identity (F _0 x)%object :=
  @identity_of C D F x.
Definition funct_comp (C D : PreCategory) (F : Functor C D) (x y z : object C)
                      (m1 : morphism C x y) (m2 : morphism C y z) :
  F _1 (m2 o m1) = F _1 m2 o F _1 m1 :=
  @composition_of C D F x y z m1 m2.
Definition funct_ctx (C D : PreCategory) (F : Functor C D) (x y : object C)
                     (m1 m2 : morphism C x y) (p : m1 = m2) : F _1 m1 = F _1 m2 :=
  ap (fun m => F _1 m) p.


Ltac reify_hyp H :=
  unfold mphT in H; unfold comp in H; unfold compose_eq in H; unfold assoc in H;
  unfold id in H; unfold left_id in H; unfold right_id in H;
  unfold r_ap in H; unfold l_ap in H;
  unfold mph_inv in H; unfold right_inv in H; unfold left_inv in H;
  unfold funct_obj in H; unfold funct_mph in H; unfold funct_id in H; unfold funct_comp in H; unfold funct_ctx.
Ltac reify :=
  unfold mphT; unfold comp; unfold compose_eq; unfold assoc;
  unfold id; unfold left_id; unfold right_id;
  unfold r_ap; unfold l_ap;
  unfold mph_inv; unfold right_inv; unfold left_inv;
  unfold funct_obj; unfold funct_mph; unfold funct_id; unfold funct_comp; unfold funct_ctx.
