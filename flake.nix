{
  description="Coq plugin to automate commutative diagrams";
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-23.05";
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
    coq = (pkgs.coq.override {
      version = "8.17";
      coq-version = "8.17";
      customOCamlPackages = ocamlPackages;
    }).overrideAttrs (final: prev: {
      patches = (prev.patches or [ ]) ++ [ ./fix-coqmakefile.patch ];
    });
    coqPackages = pkgs.mkCoqPackages coq;
    unimath = coqPackages.callPackage ./unimath.nix {};
    pkg-coq-plugin = ocamlPackages.callPackage ./coq/plugin.nix {
      inherit coq;
    };
    pkg-coq-unimath = coqPackages.callPackage ./coq/unimath.nix {
      inherit unimath;
      coq-commutative-diagrams-plugin = pkg-coq-plugin;
    };

    pkg-engine = pkgs.callPackage ./engine {
      rustPlatform = pkgs.makeRustPlatform {
        cargo = pkgs.rust-bin.stable.latest.minimal;
        rustc = pkgs.rust-bin.stable.latest.minimal;
      };
    };

    shell-coq = pkgs.mkShell {
      # Build tools
      nativeBuildInputs = builtins.attrValues {
        inherit (ocamlPackages)
          ocaml 
          findlib
          dune_3
          # ocaml-lsp
          merlin
          ;
        coqide = coqPackages.coqide;
      };
      # Dependencies
      inputsFrom = [ pkg-coq-plugin pkg-coq-unimath ];
    };
    shell-engine = pkgs.mkShell {
      nativeBuildInputs = [
        (pkgs.rust-bin.stable.latest.default.override {
          extensions = [ "rust-analyzer" "rust-src" ];
        })
        pkgs.fontconfig.dev
        pkgs.freetype.dev
        pkgs.alsa-lib.dev
        pkgs.systemd.dev
        pkgs.xorg.libX11
        pkgs.xorg.libXcursor
        pkgs.xorg.libXrandr
        pkgs.xorg.libXi
        pkgs.vulkan-tools
        pkgs.vulkan-headers
        pkgs.vulkan-loader
        pkgs.vulkan-validation-layers
      ];
    };
    shell = pkgs.mkShell {
      inputsFrom = [ shell-coq shell-engine ];
      nativeBuildInputs = builtins.attrValues {
        inherit (pkgs)
          msgpack-tools
          ;
      };
      PKG_CONFIG_PATH = pkgs.lib.concatStringsSep ":" [
        "${pkgs.fontconfig.dev}/lib/pkgconfig"
        "${pkgs.freetype.dev}/lib/pkgconfig"
        "${pkgs.alsa-lib.dev}/lib/pkgconfig"
        "${pkgs.systemd.dev}/lib/pkgconfig"
      ];
      LD_LIBRARY_PATH = pkgs.lib.concatStringsSep ":" [
        "${pkgs.xorg.libX11}/lib"
        "${pkgs.xorg.libXcursor}/lib"
        "${pkgs.xorg.libXrandr}/lib"
        "${pkgs.xorg.libXi}/lib"
        "${pkgs.vulkan-tools}/lib"
        "${pkgs.vulkan-headers}/lib"
        "${pkgs.vulkan-loader}/lib"
        "${pkgs.vulkan-validation-layers}/lib"
      ];
    };

    shell-user = include-unimath: let
      ocaml-version = ocamlPackages.ocaml.version;
      mkOcamlPath =
        pkgs.lib.concatMapStringsSep 
          ":" 
          (pkg: "${pkg}/lib/ocaml/${ocaml-version}/site-lib");
    in pkgs.mkShell {
      nativeBuildInputs = [
        self.packages.x86_64-linux.commutative-diagrams-coq
        coq
        coqPackages.coqide
        ocamlPackages.zarith
      ] 
      ++ (if include-unimath
          then [ unimath ]
          else []);
      "COMDIAG_ENGINE" = "${self.packages.x86_64-linux.commutative-diagrams-engine}/bin/commutative-diagrams-engine";
      "OCAMLPATH" = mkOcamlPath ([
        self.packages.x86_64-linux.commutative-diagrams-coq
        coq
        ocamlPackages.zarith
      ]
      ++ (if include-unimath
          then [ self.packages.x86_64-linux.commutative-diagrams-coq-unimath ]
          else [ ]));
      "LD_LIBRARY_PATH" = pkgs.lib.concatStringsSep ":" [
        "${pkgs.xorg.libX11}/lib"
        "${pkgs.xorg.libXcursor}/lib"
        "${pkgs.xorg.libXrandr}/lib"
        "${pkgs.xorg.libXi}/lib"
        "${pkgs.vulkan-tools}/lib"
        "${pkgs.vulkan-headers}/lib"
        "${pkgs.vulkan-loader}/lib"
        "${pkgs.vulkan-validation-layers}/lib"
      ];
      "COMDIAG_LOADER_NAME" = if include-unimath then "CommutativeDiagrams.Loader" else "UniMath.CategoryTheory.CommutativeDiagrams";
    };
  in {
    devShells.x86_64-linux = {
      coq-plugin = shell-coq;
      engine = shell-engine;
      default = shell;
      user = shell-user true;
      unimath-user = shell-user false;
    };
    packages.x86_64-linux = {
      commutative-diagrams-coq = pkg-coq-plugin;
      commutative-diagrams-coq-unimath = pkg-coq-unimath;
      commutative-diagrams-engine = pkg-engine;
      inherit unimath;
      default = pkg-engine;
    };
  };
}
