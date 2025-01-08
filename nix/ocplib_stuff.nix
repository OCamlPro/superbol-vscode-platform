{ sources, lib, ocamlPackages }:

ocamlPackages.buildDunePackage {
  strictDeps = true;
  pname = "ocplib_stuff";
  inherit (sources.ocplib_stuff) version;

  minimalOCamlVersion = "4.07";
  duneVersion = "3";

  src = sources.ocplib_stuff;

  propagatedBuildInputs = [ ];

  meta = with lib; {
    inherit (sources.ocplib_stuff) homepage description;
  };
}
