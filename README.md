# Commutative Diagrams Interface

This is a coq plugin that allows a user to do the diagrammatic reasonnings
parts of categoretical proofs in a graphical manner. When the goal is an
equality between morphisms, the tactic `diagram server` open an interface
displaying the goal as a graph, allowing to manipulate it from the interface.

## How to run

For now it isn't packaged. If you want to build it yourself, the supported way
is to use [nix
flakes](https://nixos.org/manual/nix/stable/command-ref/new-cli/nix3-flake.html).
Once you have nix installed with flakes enabled, you can simply run `nix
develop .` in the repo to get into a shell with all the tooling installed
(included the supported coq version and the corresponding ocaml version). From
there you can build the interface by running `cargo build` in the `engine`
directory, and the plugin by running `dune build` in the `coq` directory.

If you try to run the coq files, be aware the plugin finds the engine by
looking into the `COMDIAG_ENGINE` environment variable, which should be an
absolute path to the built engine. If built with cargo as described above, you
will find the executable at the path
`engine/target/debug/commutative-diagrams-engine` from the root of the repo.

