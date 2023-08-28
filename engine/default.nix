{ lib, rustPlatform, pkg-config,
  fontconfig, freetype, alsa-lib, systemd,
  xorg, vulkan-tools, vulkan-headers, vulkan-loader, vulkan-validation-layers }:

rustPlatform.buildRustPackage {
  pname = "commutative-diagrams";
  version = "0.5";
  src = ./.;

  cargoSha256 = "sha256-1Ovgh7tSbXrIkcUVYur6fWhIIppYr7orGs3ShUaGoEM=";
  nativeBuildInputs = [
    pkg-config
  ];
  buildInputs = [
    fontconfig
    freetype
    alsa-lib
    systemd
    xorg.libX11
    xorg.libXcursor
    xorg.libXrandr
    xorg.libXi
    vulkan-tools
    vulkan-headers
    vulkan-loader
    vulkan-validation-layers
  ];

  meta = {
    description = "An interface to do category theory proofs graphically";
    homepage = "https://github.com/dwarfmaster/commutative-diagrams";
    license = lib.licenses.mit;
    maintainers = [ lib.maintainers.dwarfmaster ];
  };
}
