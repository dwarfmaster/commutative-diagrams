
From HoTT Require Import Basics.
Declare ML Module "diagrams_plugin".

Lemma test : True.
Proof.
  assert (True /\ True) as H. print_diagram "test".
  { split. print_diagram "test". - print_diagram "test". constructor. - constructor. }
  print_diagram "test". destruct H as [ H1 H2 ]. assumption.
Qed.
