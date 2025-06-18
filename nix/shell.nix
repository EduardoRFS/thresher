{ pkgs, grainhack }:

with pkgs; with ocamlPackages; mkShell {
  inputsFrom = [ grainhack ];
  packages = [
    # Make developer life easier
    # formatters
    nixfmt
    ocamlformat

    # OCaml developer tooling
    ocaml
    dune_3
    ocaml-lsp
  ];
}
