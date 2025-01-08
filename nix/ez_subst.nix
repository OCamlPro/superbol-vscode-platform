{ sources, lib, ocamlPackages }:

ocamlPackages.buildDunePackage {
  strictDeps = true;
  pname = "ez_subst";
  inherit (sources.ez_subst) version;

  minimalOCamlVersion = "4.08";
  duneVersion = "3";

  src = sources.ez_subst;

  propagatedBuildInputs = with ocamlPackages; [
  ];

  meta = with lib; {
    inherit (sources.ez_subst) homepage description;
  };
}

