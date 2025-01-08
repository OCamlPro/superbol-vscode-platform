{ sources, lib, ocamlPackages }:

ocamlPackages.buildDunePackage {
  strictDeps = true;
  pname = "yojson";
  inherit (sources.yojson) version;

  minimalOCamlVersion = "4.03";
  duneVersion = "3";

  src = sources.yojson;

  propagatedBuildInputs = with ocamlPackages; [
    seq
  ];

  meta = with lib; {
    inherit (sources.yojson) homepage description;
  };
}


