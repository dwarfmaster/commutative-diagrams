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
      version = "8.16";
      coq-version = "8.16";
      customOCamlPackages = ocamlPackages;
    }).overrideAttrs (final: prev: {
      patches = (prev.patches or [ ]) ++ [ ./fix-coqmakefile.patch ];
    });
    coqPackages = pkgs.mkCoqPackages coq;
    unimath = coqPackages.callPackage ./unimath.nix {};

    pkg = ocamlPackages.callPackage ./coq {
      coq_8_16 = coq;
      coq-unimath_8_16 = unimath;
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
      };
      # Dependencies
      inputsFrom = [ pkg ];
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
  in {
    devShells.x86_64-linux = {
      coq-plugin = shell-coq;
      engine = shell-engine;
      default = shell;
    };
    packages.x86_64-linux = {
      commutative-diagrams = pkg;
      unimath = coqPackages.callPackage ./unimath.nix {};
      default = pkg;
    };
  };
}
