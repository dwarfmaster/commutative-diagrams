
From UniMath Require Import Preamble.
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


