
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

Ltac reify_hyp H :=
  unfold mphT in H; unfold comp in H; unfold compose_eq in H; unfold assoc in H;
  unfold id in H; unfold left_id in H; unfold right_id in H;
  unfold r_ap in H; unfold l_ap in H.

Lemma test_concat (C : PreCategory) (a b : C) (m1 m2 m3 : morphism C a b)
      (H12 : 1 o m1 = m2 o 1) (H32 : 1 o m3 o 1 = m2) : m1 = m3.
Proof.
  print_diagram "test"; reify_hyp Hsolv. exact Hsolv.
Defined.

Lemma test_context (C : PreCategory) (a b c d : C)
      (mab : morphism C a b) (mbc : morphism C b c) (mcd : morphism C c d)
      (mac : morphism C a c) (mad : morphism C a d)
      (Hac : mac o 1 o 1 o 1 = mbc o 1 o mab) (Had : mad o 1 = mcd o mac) :
  1 o mad o 1 = (mcd o 1) o (mbc o (1 o 1 o mab) o 1).
Proof.
  print_diagram "test"; reify_hyp Hsolv. exact Hsolv.
Defined.

Lemma test_mono (C : PreCategory) (a b c : C)
      (mono : morphism C b c) (m1 m2 : morphism C a b)
      (Hmono : IsMonomorphism mono) (H : mono o m1 = mono o m2) :
  m1 = m2.
Proof.
  print_diagram "test"; reify_hyp Hsolv. exact Hsolv.
Defined.

Lemma test_epi (C : PreCategory) (a b c : C)
      (epi : morphism C a b) (m1 m2 : morphism C b c)
      (Hepi : IsEpimorphism epi) (H : m1 o epi = m2 o epi) :
  m1 = m2.
Proof.
  print_diagram "test"; reify_hyp Hsolv. exact Hsolv.
Defined.

Lemma test_id_l (C : PreCategory) (a b c : C)
      (i1 : morphism C b c) (i2 : morphism C c b)
      (m : morphism C a b) (H : i2 o i1 = 1) :
  m = i2 o i1 o m.
Proof.
  print_diagram "test"; reify_hyp Hsolv. exact Hsolv.
Defined.

Lemma test_id_r (C : PreCategory) (a b c : C)
      (i1 : morphism C a b) (i2 : morphism C b a)
      (m : morphism C b c) (H : i1 o i2 = 1) :
  m = m o i1 o i2.
Proof.
  print_diagram "test"; reify_hyp Hsolv. exact Hsolv.
Defined.

Lemma test_id (C : PreCategory) (a b c d : C)
      (i1 : morphism C a b) (i2 : morphism C b a) (Hi : i1 o i2 = 1)
      (j1 : morphism C c d) (j2 : morphism C d c) (Hj : j2 o j1 = 1)
      (m : morphism C b c) : m = j2 o j1 o m o i1 o i2.
Proof.
  print_diagram "test"; reify_hyp Hsolv. exact Hsolv.
Defined.

Lemma test_iso (C : PreCategory) (a b c d : C)
      (iso1 : morphism C a b) (iso2 : morphism C c d)
      (m1 : morphism C a c) (m2 : morphism C b d)
      (Hiso1 : IsIsomorphism iso1) (Hiso2 : IsIsomorphism iso2)
      (H : (morphism_inverse iso2) o m2 o iso1 = m1) : m2 o iso1 = iso2 o m1.
Proof.
  assert (iso2 o iso2^-1 = 1) by (apply right_inverse).
  print_diagram "test". reify_hyp Hsolv. exact Hsolv.
Defined.
Print test_iso.
