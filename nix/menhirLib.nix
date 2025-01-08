{ sources, lib, ocamlPackages }:

ocamlPackages.buildDunePackage {
  strictDeps = true;
  pname = "menhirLib";
  inherit (sources.menhirLib) version;

  minimalOCamlVersion = "4.03";
  duneVersion = "3";

  src = sources.menhirLib;

  propagatedBuildInputs = [
  ];

  meta = with lib; {
    inherit (sources.menhirLib) homepage description;
  };
}



