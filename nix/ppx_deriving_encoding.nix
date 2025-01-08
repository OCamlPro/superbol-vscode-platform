{ sources, lib, ocamlPackages }:

ocamlPackages.buildDunePackage {
  strictDeps = true;
  pname = "ppx_deriving_encoding";
  inherit (sources.ppx_deriving_encoding) version;

  minimalOCamlVersion = "4.08";
  duneVersion = "3";

  src = sources.ppx_deriving_encoding;

  propagatedBuildInputs = with ocamlPackages; [
    ppxlib
    json-data-encoding
  ];

  meta = with lib; {
    inherit (sources.ppx_deriving_encoding) homepage description;
  };
}

