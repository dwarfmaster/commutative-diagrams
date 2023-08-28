{ lib, coq, mkCoqDerivation, unimath }:

mkCoqDerivation {
  pname = "commutative-diagrams";
  version = "0.1.0";

  src = ./.;

  useDune = true;
  mlPlugin = true;
  checkInputs = [ ];
  buildInputs = [
    unimath
  ];
  nativeBuildInputs = [
    coq.ocamlPackages.findlib
  ];
  postInstall = ''
    mkdir -p $OCAMLFIND_DESTDIR
    mv $out/lib/coq-commutative-diagrams $OCAMLFIND_DESTDIR
  '';

  meta = {
    homepage = "https://github.com/dwarfmaster/commutative-diagrams";
    description = "Coq plugin to work with commutative diagrams";
    license = lib.licenses.mit;
    maintainers = [ lib.maintainers.dwarfmaster ];
  };
}
