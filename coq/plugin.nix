{ lib, buildDunePackage, coq, findlib }:

buildDunePackage {
  pname = "coq-commutative-diagrams-plugin";
  version = "0.1.0";

  src = ./.;

  buildInputs = [
    coq
    findlib
  ];
  nativeBuildInputs = [
    coq
  ];

  meta = {
    homepage = "https://github.com/dwarfmaster/commutative-diagrams";
    description = "Coq plugin to work with commutative diagrams";
    license = lib.licenses.mit;
    maintainers = [ lib.maintainers.dwarfmaster ];
  };
}
