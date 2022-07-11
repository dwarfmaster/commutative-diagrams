# Commutative Diagrams

This is a coq plugin that tries to automate the *left to the reader* part of
diagrammatic proofs. It is capable of reasoning up to associativity, unitality,
inverse of isomorphisms, and can handle monomorphisms and epimorphisms.

For now it only works with the *HoTT* category library, but could easily be
ported to any category library in Coq.

# Installation

A `flake.nix` file is provided for nix users. The plugin is also available on
opam as `coq-commutative-diagrams`.

# Usage

`CommutativeDiagrams.Loader` must be imported. It then exposes 3 main tactis:

- `diagram print "name"` will create a file name `name` in the current
  directory containing a graphviz representation of the hypothesis. In other
  words this generates a drawing of the diagram implicitely present in the
  hypotheses.
- `diagram solve [n]` will solve the current goal if it is an equality between
  morphisms that can be deduced from simple diagrammatic manipulation of the
  equalities in the context. Since this is in general undecidable, we use some
  heuristic. You can increase the integer argument if you believe it should be
  able to solve your goal. The integer should be greater than the longest chain
  of morphisms in the reasonning for the equality.
- `diagram norm` will normalise a goal that consists of an equality of
  morphisms. That is to say it will remove identities, compositions of inverses,
  and rewrite associativity to the right. For now, to use it, you need to type
  `unshelve (diagram norm); reify`, but hopefully this will be fixed.

# Features/TODO-list

- [X] Composition
- [X] Associativity
- [X] Unitality
- [X] Monomorphisms
- [X] Epimorphisms
- [X] Isomorphisms
- [ ] Compostion under inverses
- [ ] Functors
- [ ] Natural transformations
- [ ] Equality of objects
- [ ] Products and coproducts of categories
- [ ] Monoidal categories
- [ ] Dual categories
