{ buildDunePackage, coq_8_15, coq-hott_8_15, zarith, lib }:

buildDunePackage {
  pname = "commutative-diagrams";
  version = "0.1.0";
  duneVersion = "2";

  src = ./.;

  checkInputs = [ ];
  buildInputs = [
    coq_8_15
    coq-hott_8_15
    zarith
  ];

  meta = {
    homepage = "https://github.com/dwarfmaster/commutative-diagrams";
    description = "Coq plugin to work with commutative diagrams";
    license = lib.licenses.mit;
    maintainers = [ lib.maintainers.dwarfmaster ];
  };
}
