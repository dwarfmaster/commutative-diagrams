
From HoTT Require Import Basics.
From HoTT Require Import Categories.
Declare ML Module "diagrams_plugin".
Local Open Scope morphism.

Lemma compose_eq (C : PreCategory) (a b c : C) (m1 m2 : morphism C a b) (m3 m4 : morphism C b c)
      (p1 : m1 = m2) (p2 : m3 = m4) : m3 o m1 = m4 o m2.
Proof. f_ap. Defined.
Definition assoc (C : PreCategory) (a b c d : C)
      (m1 : morphism C a b) (m2 : morphism C b c) (m3 : morphism C c d) :
  m3 o (m2 o m1) = (m3 o m2) o m1 := (associativity C a b c d m1 m2 m3)^.
Definition id (C : PreCategory) (a : C) : morphism C a a := @identity C a.
Definition left_id (C : PreCategory) (a b : C) (m : morphism C a b) :
  id C b o m = m := left_identity C a b m.
Definition right_id (C : PreCategory) (a b : C) (m : morphism C a b) :
  m o id C a = m := right_identity C a b m.

Lemma test (C : PreCategory) (a b c d : C)
      (mab : morphism C a b) (mbc : morphism C b c) (mcd : morphism C c d)
      (mac : morphism C a c) (mad : morphism C a d)
      (Hac : mac = mbc o mab) (Had : mad = mcd o mac) :
  mad = mcd o mbc o mab.
Proof.
  print_diagram "test".
  rewrite associativity. print_diagram "test".
  rewrite <- Hac. exact Had.
Qed.
