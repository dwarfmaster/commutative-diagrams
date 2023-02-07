{
  description="Coq plugin to automate commutative diagrams";
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-22.11";
    rust = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, rust }: let
    system = "x86_64-linux";
    pkgs = import nixpkgs {
      inherit system; 
      overlays = [ rust.overlays.default ];
    };
    ocamlPackages = pkgs.ocaml-ng.ocamlPackages_4_14;
    coq = pkgs.coq.override {
      version = "8.15";
      coq-version = "8.15";
      customOCamlPackages = ocamlPackages;
    };
    coqPackages = pkgs.mkCoqPackages coq;
    hott = coqPackages.callPackage ./hott.nix {};

    pkg = ocamlPackages.callPackage ./coq {
      coq_8_15 = coq;
      coq-hott_8_15 = hott;
    };

    shell-coq = pkgs.mkShell {
      # Build tools
      nativeBuildInputs = builtins.attrValues {
        inherit (ocamlPackages)
          ocaml 
          findlib
          dune_2
          # ocaml-lsp
          merlin
          ;
      };
      # Dependencies
      inputsFrom = [ pkg ];
    };
    shell-engine = pkgs.mkShell {
      nativeBuildInputs = [
        (pkgs.rust-bin.stable.latest.default.override {
          extensions = [ "rust-analyzer" "rust-src" ];
        })
      ];
    };
    shell = pkgs.mkShell {
      inputsFrom = [ shell-coq shell-engine ];
      nativeBuildInputs = builtins.attrValues {
        inherit (pkgs)
          msgpack-tools
          ;
      };
    };
  in {
    devShells.x86_64-linux = {
      coq-plugin = shell-coq;
      engine = shell-engine;
      default = shell;
    };
    packages.x86_64-linux = {
      commutative-diagrams = pkg;
      default = pkg;
    };
  };
}
