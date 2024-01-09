
From UniMath Require Import Preamble.
From UniMath Require Import Foundations.NaturalNumbers.
From UniMath Require Import CategoryTheory.Core.Categories.
From UniMath Require Import CategoryTheory.Core.Functors.
Declare ML Module "coq-commutative-diagrams-plugin.plugin".
Local Open Scope cat.

Lemma compose_eq (C : precategory) (a b c : C) (m1 m2 : C⟦a,b⟧) (m3 m4 : C⟦b,c⟧)
                 (p1 : m1 = m2) (p2 : m3 = m4) : m3 ∘ m1 = m4 ∘ m2.
Proof. rewrite p1. rewrite p2. reflexivity. Defined.
Lemma r_ap (C : precategory) (a b c : C) (m : C⟦a,b⟧) (m1 m2 : C⟦b,c⟧)
           (p : m1 = m2) : m1 ∘ m = m2 ∘ m.
Proof. rewrite p. reflexivity. Defined.
Lemma l_ap (C : precategory) (a b c : C) (m1 m2 : C⟦a,b⟧) (m : C⟦b,c⟧)
           (p : m1 = m2) : m ∘ m1 = m ∘ m2.
Proof. rewrite p. reflexivity. Defined.
Lemma funct_ctx (C D : precategory) (F : C ⟶ D) (x y : C) (m1 m2 : C⟦x,y⟧) (p : m1 = m2) :
  # F m1 = # F m2.
Proof. rewrite p. reflexivity. Defined.
Lemma concat_eq (T : UU) (a b c : T) (p1 : a = b) (p2 : b = c) : a = c.
Proof. rewrite p1. exact p2. Defined.
Lemma inv_eq (T : UU) (a b : T) (p : a = b) : b = a.
Proof. rewrite p. reflexivity. Defined.

Notation I := (identity _).

Lemma test (C: precategory) (a b c d : C)
           (m1 m2 m3 : b --> c) (m': a --> b) (m'': c --> d)
           (H1: m1 = I ∘ m2) (H2: m3 = m2)
           (f: a --> b -> a --> c) (Hf: forall(m: a --> b), f m = m1 ∘ m):
  I ∘ I ∘ (m'' ∘ I ∘ (I ∘ m3)) ∘ (m' ∘ I) = I ∘ (m'' ∘ (I ∘ I)) ∘ I ∘ (f m').
Proof.
  diagram run "test.diag".
Defined.
Print test.

Lemma test_nat (C: precategory)
           (x : nat -> C) (fx : forall(n:nat), x n --> x (S n))
           (y : nat -> C) (fy : forall(n:nat), y n --> y (S n))
           (s : forall(n:nat), x n --> y n) (natural: forall n, s (S n) ∘ fx n = fy n ∘ s n):
  s 5 ∘ fx 4 ∘ fx 3 ∘ fx 2 ∘ fx 1 ∘ fx 0 = fy 4 ∘ fy 3 ∘ fy 2 ∘ fy 1 ∘ fy 0 ∘ s 0.
Proof.
  diagram run "nat2.diag"; apply natural.
  (* diagram run "nat.diag". *)
Defined.
Print test_nat.
