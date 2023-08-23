# Commutative Diagrams Interface

This is a coq plugin that allows a user to do the diagrammatic reasonnings parts
of categoretical proofs in a graphical manner. It can analyze the proof context
to deduct the categorical diagram being working on, to display it and enable a
graphical way to progress the proof.

## Installation

There are two pieces of software that need to be installed for it to work, the
interface and the Coq plugin.

### Nix

TODO

### The interface

TODO

### The plugin

The plugin is written in OCaml and can be installed using
[opam](https://opam.ocaml.org).

> [!WARNING]
> Due to a [bug](https://github.com/coq/coq/pull/17697) in Coq under linux,
> installing unimath with opam will fail until coq 8.18 is out (and I have
> ported the plugin to it). Until then please use [nix](#nix) to install coq and
> unimath.

First you need to add the coq plugin repository to opam:
```sh
opam repo add coq-released https://coq.inria.fr/opam/released
```

Then you need to install the necessary dependencies. You also need to make sure
the current opam switch is using an ocaml version that is at least `4.14.1`.
```sh
opam install dune coq.8.16.1 coq-unimath.20230321
```

Finally build and install the plugin:
```sh
opam pin add coq-commutative-diagrams https://github.com/dwarfmaster/commutative-diagrams.git
```

Finally, import opam environment in the current shell. Unless you use a mecanism
to automate it, you need to do this for every shell you open.
```sh
eval $(opam env)
```

## Usage

To use it, import `CommutativeDiagrams.Loader` in your Coq file. Now you can use
the `diagram run` and `diagram edit` tactics.

TODO
