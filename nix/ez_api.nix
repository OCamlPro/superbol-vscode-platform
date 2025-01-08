{ sources, lib, ocamlPackages }:

ocamlPackages.buildDunePackage {
  strictDeps = true;
  pname = "ez_api";
  inherit (sources.ez_api) version;

  minimalOCamlVersion = "4.08";
  duneVersion = "3";

  src = sources.ez_api;

  propagatedBuildInputs = with ocamlPackages; [
    json-data-encoding
    lwt
    ezjsonm
    uuidm
  ];

  meta = with lib; {
    inherit (sources.ez_api) homepage description;
  };
}

