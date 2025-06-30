From UniMath Require Import Preamble.
From UniMath Require Import CategoryTheory.Core.Categories.
From UniMath Require Import CategoryTheory.Core.Functors.
From UniMath Require Import Foundations.All.
From CommutativeDiagrams Require Import Loader.
Declare ML Module "coq-commutative-diagrams-plugin.plugin".
Local Open Scope cat.

Definition product_ex (C : precategory) (a b p : C)
  (π1 : C⟦p,a⟧) (π2 : C⟦p,b⟧)
  := ∏(c : C) (f : C⟦c,a⟧) (g : C⟦c,b⟧), ∑(fg : C⟦c,p⟧),
     ∑(_ : π1 ∘ fg = f), π2 ∘ fg = g.

Definition product_uniq (C : precategory) (a b p : C)
  (π1 : C⟦p,a⟧) (π2 : C⟦p,b⟧)
  := ∏(c : C) (f : C⟦c,a⟧) (g : C⟦c,b⟧),
     ∏(fg : C⟦c,p⟧), ∏(fg' : C⟦c,p⟧),
     ∏(_ : π1 ∘ fg = f), ∏(_ : π2 ∘ fg = g),
     ∏(_ : π1 ∘ fg' = f), ∏(_ : π2 ∘ fg' = g),
     fg = fg'.

Lemma demo (C : precategory) (a b p1 p2 : C)
  (π1 : C⟦p1,a⟧) (π2 : C⟦p1,b⟧)
  (Hex1 : product_ex C a b p1 π1 π2)
  (Huniq1 : product_uniq C a b p1 π1 π2)
  (π1' : C⟦p2,a⟧) (π2' : C⟦p2,b⟧)
  (Hex2 : product_ex C a b p2 π1' π2')
  (Huniq2 : product_uniq C a b p2 π1' π2') :
  ∑(i1 : C⟦p1,p2⟧) (i2 : C⟦p2,p1⟧),
  i2 ∘ i1 = identity p1.
Proof.
  unfold product_ex in Hex1. unfold product_uniq in Huniq1.
  unfold product_ex in Hex2. unfold product_uniq in Huniq2.
  eexists. eexists.
  diagram edit "demo.diag".
Qed.
Print demo.

