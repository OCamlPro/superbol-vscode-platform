{ pkgs ? import ./nix {} }:

let 
  inherit (pkgs) ocamlPackages;
in

pkgs.mkShell {
  nativeBuildInputs = with ocamlPackages; [
    ocaml
    dune_3
    menhir
  ];

  buildInputs = with ocamlPackages; [
    json-data-encoding
  ];
}
