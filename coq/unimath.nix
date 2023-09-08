{ lib, coq, mkCoqDerivation, unimath, coq-commutative-diagrams-plugin }:

mkCoqDerivation {
  pname = "commutative-diagrams-unimath";
  version = "0.1.0";

  src = ./.;

  useDune = true;
  checkInputs = [ ];
  buildInputs = [
    unimath
    coq-commutative-diagrams-plugin
  ];
  nativeBuildInputs = [
    # coq.ocamlPackages.findlib
  ];

  meta = {
    homepage = "https://github.com/dwarfmaster/commutative-diagrams";
    description = "Coq plugin to work with commutative diagrams - Unimath";
    license = lib.licenses.mit;
    maintainers = [ lib.maintainers.dwarfmaster ];
  };
}
