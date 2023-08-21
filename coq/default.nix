{ buildDunePackage, coq_8_16, coq-unimath_8_16, zarith, lib }:

buildDunePackage {
  pname = "commutative-diagrams";
  version = "0.1.0";
  duneVersion = "3";

  src = ./.;

  checkInputs = [ ];
  buildInputs = [
    coq_8_16
    coq-unimath_8_16
    zarith
  ];

  meta = {
    homepage = "https://github.com/dwarfmaster/commutative-diagrams";
    description = "Coq plugin to work with commutative diagrams";
    license = lib.licenses.mit;
    maintainers = [ lib.maintainers.dwarfmaster ];
  };
}
