with import <nixpkgs> {};
mkShell {
  buildInputs = (with ocaml-ng.ocamlPackages_4_07;
  [
    ocaml
    merlin
  ]);
  OCAMLRUNPARAM = "bt";
  FINDLIB_CONF = "/dev/null";
}
