{ sources, lib, ocamlPackages }:

ocamlPackages.buildDunePackage {
  strictDeps = true;
  pname = "menhirSdk";
  inherit (sources.menhirSdk) version;

  minimalOCamlVersion = "4.03";
  duneVersion = "3";

  src = sources.menhirSdk;

  propagatedBuildInputs = [
  ];

  meta = with lib; {
    inherit (sources.menhirSdk) homepage description;
  };
}


