{
  description="Coq plugin to automate commutative diagrams";
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-22.05";
  };

  outputs = { self, nixpkgs }: let
    system = "x86_64-linux";
    pkgs = import nixpkgs { inherit system; };
    ocamlPackages = pkgs.ocaml-ng.ocamlPackages_4_13;
    coq = pkgs.coq.override {
      version = "8.15";
      coq-version = "8.15";
      customOCamlPackages = ocamlPackages;
    };
    coqPackages = pkgs.mkCoqPackages coq;
    hott = coqPackages.callPackage ./hott.nix {};

    pkg = ocamlPackages.callPackage ./default.nix {
      coq_8_15 = coq;
      coq-hott_8_15 = hott;
    };

    shell = pkgs.mkShell {
      # Build tools
      nativeBuildInputs = builtins.attrValues {
        inherit (pkgs)
          cargo
          rustc
          ;
        inherit (ocamlPackages)
          ocaml 
          findlib
          dune_2
          ocaml-lsp
          merlin
          ;
      };
      # Dependencies
      inputsFrom = [ pkg ];
    };
  in {
    devShells.x86_64-linux.default = shell;
    packages.x86_64-linux = {
      commutative-diagrams = pkg;
      default = pkg;
    };
  };
}
