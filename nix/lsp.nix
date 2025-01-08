{ sources, lib, ocamlPackages }:

ocamlPackages.buildDunePackage {
  strictDeps = true;
  pname = "lsp";
  inherit (sources.lsp) version;

  minimalOCamlVersion = "4.14";
  duneVersion = "3";

  src = sources.lsp;

  propagatedBuildInputs = with ocamlPackages; [
    jsonrpc
    ppx_yojson_conv_lib
    uutf
    yojson
  ];

  meta = with lib; {
    inherit (sources.lsp) homepage description;
  };
}

