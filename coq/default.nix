{ buildDunePackage, coq_8_16, coq-unimath_8_16, zarith, findlib, lib }:

buildDunePackage {
  pname = "coq-commutative-diagrams";
  version = "0.1.0";
  duneVersion = "3";

  src = ./.;

  checkInputs = [ ];
  nativeBuildInputs = [
    coq_8_16
  ];
  buildInputs = [
    coq_8_16
    coq-unimath_8_16
    zarith
    findlib
  ];

  meta = {
    homepage = "https://github.com/dwarfmaster/commutative-diagrams";
    description = "Coq plugin to work with commutative diagrams";
    license = lib.licenses.mit;
    maintainers = [ lib.maintainers.dwarfmaster ];
  };
}
