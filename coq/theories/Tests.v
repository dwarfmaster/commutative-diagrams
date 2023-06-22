
From HoTT Require Import Basics.
From HoTT Require Import Categories.
From HoTT Require Import Categories.Category.Morphisms.
Require Import CommutativeDiagrams.Loader.
Local Open Scope morphism.

Ltac norm := unshelve (diagram norm); reify.

Lemma test_concat (C : PreCategory) (a b : C) (m1 m2 m3 : morphism C a b)
      (H12 : 1 o m1 = m2 o 1) (H32 : 1 o m3 o 1 = m2) : m1 = m3.
Proof. diagram solve. Defined.

Lemma test_functor (C D : PreCategory) (a : C) (F : Functor C D)
                 : F _1 (identity a) = identity (F _0 a).
Proof. norm. reflexivity. Defined.

Lemma test_context (C : PreCategory) (a b c d : C)
      (mab : morphism C a b) (mbc : morphism C b c) (mcd : morphism C c d)
      (mac : morphism C a c) (mad : morphism C a d)
      (Hac : mac o 1 o 1 o 1 = mbc o 1 o mab) (Had : mad o 1 = mcd o mac) :
  1 o mad o 1 = (mcd o 1) o (mbc o (1 o 1 o mab) o 1).
Proof. diagram solve. Defined.

(* Lemma test_mono (C : PreCategory) (a b c : C) *)
(*       (mono : morphism C b c) (m1 m2 : morphism C a b) *)
(*       (Hmono : IsMonomorphism mono) (H : mono o m1 = mono o m2) : *)
(*   m1 = m2. *)
(* Proof. diagram solve. Defined. *)

(* Lemma test_epi (C : PreCategory) (a b c : C) *)
(*       (epi : morphism C a b) (m1 m2 : morphism C b c) *)
(*       (Hepi : IsEpimorphism epi) (H : m1 o epi = m2 o epi) : *)
(*   m1 = m2. *)
(* Proof. diagram solve. Defined. *)

Lemma test_id_l (C : PreCategory) (a b c : C)
      (i1 : morphism C b c) (i2 : morphism C c b)
      (m : morphism C a b) (H : i2 o i1 = 1) :
  m = i2 o i1 o m.
Proof. diagram solve 3. Defined.

Lemma test_id_r (C : PreCategory) (a b c : C)
      (i1 : morphism C a b) (i2 : morphism C b a)
      (m : morphism C b c) (H : i1 o i2 = 1) :
  m = m o i1 o i2.
Proof. diagram solve. Defined.

Lemma test_id (C : PreCategory) (a b c d : C)
      (j1 : morphism C c d) (j2 : morphism C d c) (Hj : j2 o j1 = 1)
      (i1 : morphism C a b) (i2 : morphism C b a) (Hi : i1 o i2 = 1)
      (m : morphism C b c) : m = j2 o j1 o m o i1 o i2.
Proof. diagram solve. Defined.

(* Lemma test_basic_iso_r (C : PreCategory) (a b : C) *)
(*       (iso : morphism C a b) (Hiso : IsIsomorphism iso) : *)
(*   iso o iso^-1 = 1. *)
(* Proof. diagram solve 0. Defined. *)

(* Lemma test_basic_iso_l (C : PreCategory) (a b : C) *)
(*       (iso : morphism C a b) (Hiso : IsIsomorphism iso) : *)
(*   iso^-1 o iso = 1. *)
(* Proof. diagram solve 0. Defined. *)

(* Lemma test_iso (C : PreCategory) (a b c d : C) *)
(*       (iso1 : morphism C a b) (iso2 : morphism C c d) *)
(*       (m1 : morphism C a c) (m2 : morphism C b d) *)
(*       (Hiso1 : IsIsomorphism iso1) (Hiso2 : IsIsomorphism iso2) *)
(*       (H : iso2^-1 o m2 o iso1 = m1) : m2 o iso1 = iso2 o m1. *)
(* Proof. diagram solve. Defined. *)

(* Lemma test_norm (C : PreCategory) (a b c d : C) *)
(*       (iso1 : morphism C a b) (m : morphism C b c) (iso2 : morphism C c d) *)
(*       (H1 : IsIsomorphism iso1) (H2 : IsIsomorphism iso2) : *)
(*   (iso2^-1 o (iso2 o 1 o 1) o m) o (1 o (1 o iso1) o iso1^-1 o iso1) = *)
(*     m o iso1. *)
(* Proof. norm. reflexivity. Defined. *)

(* Lemma test_funct_iso (C D E F : PreCategory) (a b : C) *)
(*                      (iso : morphism C a b) (Hiso : IsIsomorphism iso) *)
(*                      (F1 : Functor C D) (F2 : Functor D E) (F3 : Functor E F) *)
(*                      : F3 _1 (F2 _1 (F1 _1 iso)) o F3 _1 (F2 _1 (F1 _1 iso^-1)) = 1. *)
(* Proof. diagram solve 3. Defined. *)

(* Lemma test_funct_iso2 (C D E F : PreCategory) (a b : C) *)
(*                       (iso : morphism C a b) (Hiso : IsIsomorphism iso) *)
(*                       (F1 : Functor C D) (F2 : Functor D E) (F3 : Functor E F) *)
(*                      : F3 _1 (F2 _1 (F1 _1 (iso o iso^-1))) = 1. *)
(* Proof. diagram solve 3. Defined. *)

(* Lemma test_iso_complex (C : PreCategory) (a b c : C) *)
(*                        (mab : morphism C a b) (mbc : morphism C b c) (mca : morphism C c a) *)
(*                        (Hiso : IsIsomorphism (mbc o mab)) (p : (mbc o mab)^-1 = mca) *)
(*                        : mca o mbc o mab = 1. *)
(* Proof. diagram solve 3. Defined. *)
