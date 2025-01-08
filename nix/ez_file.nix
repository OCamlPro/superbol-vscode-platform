{ sources, lib, ocamlPackages }:

ocamlPackages.buildDunePackage {
  strictDeps = true;
  pname = "ez_file";
  inherit (sources.ez_file) version;

  minimalOCamlVersion = "4.07";
  duneVersion = "3";

  src = sources.ez_file;

  propagatedBuildInputs = with ocamlPackages; [
    re
    ocplib_stuff
  ];

  meta = with lib; {
    inherit (sources.ez_file) homepage description;
  };
}

