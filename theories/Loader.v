
From HoTT Require Import Basics.
From HoTT Require Import Categories.
Declare ML Module "diagrams_plugin".
Local Open Scope morphism.

Lemma compose_eq (C : PreCategory) (a b c : C) (m1 m2 : morphism C a b) (m3 m4 : morphism C b c)
      (p1 : m1 = m2) (p2 : m3 = m4) : m3 o m1 = m4 o m2.
Proof. f_ap. Defined.

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
