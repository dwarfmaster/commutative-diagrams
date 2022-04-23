
From HoTT Require Import Basics.
From HoTT Require Import Categories.
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

Lemma test (C : PreCategory) (a b c d : C)
      (mab : morphism C a b) (mbc : morphism C b c) (mcd : morphism C c d)
      (mac : morphism C a c) (mad : morphism C a d)
      (Hac : mac = mbc o mab) (Had : mad = mcd o mac) :
  1 o mad o 1 = (mcd o 1) o (mbc o (1 o 1 o mab) o 1).
Proof.
  print_diagram "test".
  unfold comp in H1; rewrite H1.
  unfold comp in H2; rewrite H2.
  rewrite associativity. rewrite <- Hac. exact Had.
Qed.
