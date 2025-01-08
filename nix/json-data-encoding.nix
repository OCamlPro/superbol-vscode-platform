{ sources, lib, ocamlPackages }:

ocamlPackages.buildDunePackage {
  strictDeps = true;
  pname = "json-data-encoding";
  inherit (sources.json-data-encoding) version;

  minimalOCamlVersion = "4.10";
  duneVersion = "3";

  src = sources.json-data-encoding;

  propagatedBuildInputs = with ocamlPackages; [
    uri
    hex
  ];

  meta = with lib; {
    inherit (sources.json-data-encoding) homepage description;
  };
}


