{ sources, lib, ocamlPackages }:

ocamlPackages.buildDunePackage {
  strictDeps = true;
  pname = "ez_cmdliner";
  inherit (sources.ez_cmdliner) version;

  minimalOCamlVersion = "4.08";
  duneVersion = "3";

  src = sources.ez_cmdliner;

  propagatedBuildInputs = with ocamlPackages; [
    ocplib_stuff
    ez_subst
    cmdliner
  ];

  meta = with lib; {
    inherit (sources.ez_cmdliner) homepage description;
  };
}

