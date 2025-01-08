{ sources, lib, ocamlPackages }:

ocamlPackages.buildDunePackage {
  strictDeps = true;
  pname = "menhir";
  inherit (sources.menhir) version;

  minimalOCamlVersion = "4.03";
  duneVersion = "3";

  src = sources.menhir;

  propagatedBuildInputs = [
    ocamlPackages.menhirSdk
    ocamlPackages.menhirLib
  ];

  meta = with lib; {
    inherit (sources.menhir) homepage description;
  };
}

