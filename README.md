# Commutative Diagrams Interface

This is a coq plugin that allows a user to do the diagrammatic reasonnings parts
of categoretical proofs in a graphical manner. It can analyze the proof context
to deduct the categorical diagram being working on, to display it and enable a
graphical way to progress the proof.

## Architecture

This tool is split in two parts. The most important one, the interface itself,
is a standalone program that handles displaying the graph and acting on it. It
interacts with the proof assistant with an homemade protocol over its
stdin/stdout. Then there is a Coq plugin, that analyze the Coq goal and exposes
features and information from Coq to the interface through the protocol.

The Coq plugin is also responsible for starting the interface, and thus it must
be able to find it. If there is a `COMDIAG_ENGINE` environment variable, it
assumes it contains a path to the interface. Otherwise it looks for a
`commutative-diagrams-engine` in `$PATH`.

## Installation

### With Nix

> [!WARNING]
> Right now the nix option is only available on linux due to the way I wrote the
> nix file.

Using [Nix](https://nixos.org/) is the easiest way to quickly try the plugin if
you have nix installed and [flakes enabled](https://nixos.wiki/wiki/Flakes). Simply run the following command:
```sh
nix develop "github:dwarfmaster/commutative-diagrams#user"
```

This will drop you in a shell with the right coq version installed, and
`OCAMLPATH`, `COQPATH` and `COMDIAG_ENGINE` setup the right way. You can then
start `coqide` from this shell and use it to edit any file you're interested in.

If you want to use other interfaces like proof-general, you can use the
[direnv](https://direnv.net/) integrationg of Emacs/VScode to have them load the
flake automatically.

### With Cargo and Opam

#### The interface

The interface is written in rust and packaged using
[cargo](https://doc.rust-lang.org/cargo/). You can install it using:

```sh
cargo install --git https://github.com/dwarfmaster/commutative-diagrams.git
```

The directory it will be installed in depends of your setup. On Linux, by
default, it is installed in `$HOME/.cargo/bin`. Wherever it actually ends up,
the `cargo install` command should warn you of which directory to add to the
path to be able to run the command.

#### The plugin

The plugin is written in OCaml and can be installed using
[opam](https://opam.ocaml.org).

> [!WARNING]
> Due to a [bug](https://github.com/coq/coq/pull/17697) in Coq under linux,
> installing unimath with opam will fail until coq 8.18 is out (and I have
> ported the plugin to it). Until then please use [nix](#nix) to install coq and
> unimath. As such I haven't been able to fully test to following procedure.

First you need to add the coq plugin repository to opam:
```sh
opam repo add coq-released https://coq.inria.fr/opam/released
```

Then you need to install the necessary dependencies. You also need to make sure
the current opam switch is using an ocaml version that is at least `4.14.1`.
```sh
opam install dune coq.8.17.1 coq-unimath.20230321
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

To import the plugin, use the following coq command:
```coq
From CommutativeDiagrams Require Import Loader.
```

You can now use the `diagram run` and `diagram edit` tactics.

The `diagram run "file"` tactic tries to read commands from file and execute
them on the current goal. If it succeeds, the interface is never opened. If it
fails, or if the the file is empty, the interface is opened to graphically solve
the goal. If there where commands in the file, they are not executed but can be
replayed using the `Edit>Redo` button.

The `diagram edit "file"` always open the interface, allowing to redo part of
the proof and change other, even if it would have succeeded.
