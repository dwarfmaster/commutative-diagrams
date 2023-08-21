{ lib, mkCoqDerivation, coq, version ? null }:

let
  oneOf = a: v: builtins.elem v a;
in mkCoqDerivation {
  pname = "UniMath";
  domain = "github.com";
  owner = "UniMath";
  inherit version;
  defaultVersion = lib.switch coq.coq-version [
    { case = oneOf [ "8.16" "8.16.1" "8.17" ];
      out = coq.coq-version; }
  ] null;
  releaseRev = v: "v20230420";
  release."8.16".sha256 = "0ni1fjl97rvc3frsq0n5kkvnvp8ndnz8m07jkkz3l5kv0wr8wdac";
  release."8.16.1".sha256 = "0ni1fjl97rvc3frsq0n5kkvnvp8ndnz8m07jkkz3l5kv0wr8wdac";
  release."8.17".sha256 = "0ni1fjl97rvc3frsq0n5kkvnvp8ndnz8m07jkkz3l5kv0wr8wdac";

  meta = {
    description = "A coq library aiming to formalize a substantial body of mathematics using the univalent point of view";
    # license = lib.licenses.bsd2;
    maintainers = [ lib.maintainers.dwarfmaster ];
  };
}
