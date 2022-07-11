{
  description="Coq plugin to automate commutative diagrams";
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-22.05";
    hott.url = "github:dwarfmaster/coq-hott-nix";
  };

  outputs = { self, nixpkgs, hott }: let
    pkgs = import nixpkgs { system = "x86_64-linux"; };
    ocamlPackages = pkgs.ocaml-ng.ocamlPackages_4_12;
    coqPackages = pkgs.coqPackages_8_15;

    pkg = ocamlPackages.callPackage ./default.nix {
      coq_8_15 = coqPackages.coq;
      inherit (hott.packages."x86_64-linux") coq-hott_8_15;
    };

    shell = pkgs.mkShell {
      # Build tools
      nativeBuildInputs = builtins.attrValues {
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
