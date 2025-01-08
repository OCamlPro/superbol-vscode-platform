{ sources ? import ./nix/sources.nix,
  pkgs ? import ./nix { inherit sources; }
}:

let
  inherit (pkgs) lib ocamlPackages;
  version = "dev"; /*TODO: Edit*/
  src = lib.cleanSource ./.;

  pretty = ocamlPackages.buildDunePackage {
    pname = "pretty";
    inherit version src;

    minimalOcamlVersion = "4.14";
    duneVersion = "3";

    propagatedBuildInputs = with ocamlPackages; [
      fmt
      ez_file
    ];
  };

  cobol_common = ocamlPackages.buildDunePackage {
    pname = "cobol_common";
    inherit version src;

    minimalOcamlVersion = "4.14";
    duneVersion = "3";

    propagatedBuildInputs = with ocamlPackages; [
      ppxlib
      ppx_deriving
      pretty
      ocplib_stuff
    ];

  };

  cobol_config = ocamlPackages.buildDunePackage {
    pname = "cobol_config";
    inherit version src;

    minimalOcamlVersion = "4.14";
    duneVersion = "3";

    nativeBuildInputs = [
      ocamlPackages.menhir
    ];

    propagatedBuildInputs = with ocamlPackages; [
      cobol_common
      menhirLib
    ];
  };


  cobol_indent = ocamlPackages.buildDunePackage {
    pname = "cobol_indent";
    inherit version src;

    minimalOcamlVersion = "4.14";
    duneVersion = "3";

    propagatedBuildInputs = with ocamlPackages; [
      cobol_config
    ];
  };

  cobol_ptree = ocamlPackages.buildDunePackage {
    pname = "cobol_ptree";
    inherit version src;

    minimalOcamlVersion = "4.14";
    duneVersion = "3";

    propagatedBuildInputs = with ocamlPackages; [
      cobol_common
    ];
  };

  cobol_data = ocamlPackages.buildDunePackage {
    pname = "cobol_data";
    inherit version src;

    minimalOcamlVersion = "4.14";
    duneVersion = "3";

    propagatedBuildInputs = with ocamlPackages; [
      zarith
      cobol_config
      cobol_ptree
    ];
  };

  ebcdic_lib = ocamlPackages.buildDunePackage {
    pname = "ebcdic_lib";
    inherit version src;

    minimalOcamlVersion = "4.14";
    duneVersion = "3";

    propagatedBuildInputs = with ocamlPackages; [];
  };

  cobol_preproc = ocamlPackages.buildDunePackage {
    pname = "cobol_preproc";
    inherit version src;

    minimalOcamlVersion = "4.14";
    duneVersion = "3";

    nativeBuildInputs = [
      ocamlPackages.menhir
    ];

    propagatedBuildInputs = with ocamlPackages; [
      cobol_data

      menhirSdk
      ppx_import
    ];

    /* Used to build generated OCaml files */
    configurePhase = ''
      dune build src/lsp/cobol_preproc
    '';

  };

  cobol_unit = ocamlPackages.buildDunePackage {
    pname = "cobol_unit";
    inherit version src;

    minimalOcamlVersion = "4.14";
    duneVersion = "3";

    nativeBuildInputs = [
      ocamlPackages.menhir
    ];

    propagatedBuildInputs = with ocamlPackages; [
      cobol_data
    ];

  };

  cobol_parser = ocamlPackages.buildDunePackage {
    pname = "cobol_parser";
    inherit version src;

    minimalOcamlVersion = "4.14";
    duneVersion = "3";

    nativeBuildInputs = [
      ocamlPackages.menhir
    ];

    /* Used to build generated OCaml files */
    configurePhase = ''
      dune build src/lsp/cobol_parser
    '';

    propagatedBuildInputs = with ocamlPackages; [
      ebcdic_lib
      cobol_ptree
      cobol_preproc
      cobol_unit

      menhirSdk
    ];

  };

  cobol_typeck = ocamlPackages.buildDunePackage {
    pname = "cobol_typeck";
    inherit version src;

    minimalOcamlVersion = "4.14";
    duneVersion = "3";

    propagatedBuildInputs = with ocamlPackages; [
      cobol_data
      cobol_parser
    ];

  };


  cobol_cfg = ocamlPackages.buildDunePackage {
    pname = "cobol_cfg";
    inherit version src;

    minimalOcamlVersion = "4.14";
    duneVersion = "3";

    propagatedBuildInputs = with ocamlPackages; [
      cobol_typeck
      ocamlgraph
    ];

  };

  sql_ast = ocamlPackages.buildDunePackage {
    pname = "sql_ast";

    inherit version src;

    minimalOcamlVersion = "4.14";
    duneVersion = "3";

    propagatedBuildInputs = with ocamlPackages; [
      cobol_common
    ];
  };

  sql_parser = ocamlPackages.buildDunePackage {
    pname = "sql_parser";
    inherit version src;

    minimalOcamlVersion = "4.14";
    duneVersion = "3";

    nativeBuildInputs = [
      ocamlPackages.menhir
    ];

    propagatedBuildInputs = with ocamlPackages; [
      sql_ast
      cobol_parser
    ];
  };

  superbol_preprocs = ocamlPackages.buildDunePackage {
    pname = "superbol_preprocs";
    inherit version src;

    minimalOcamlVersion = "4.14";
    duneVersion = "3";

    propagatedBuildInputs = with ocamlPackages; [
      sql_parser
    ];
  };

  ez_toml = ocamlPackages.buildDunePackage {
    pname = "ez_toml";
    inherit version src;

    minimalOcamlVersion = "4.14";
    duneVersion = "3";

    nativeBuildInputs = [
      ocamlPackages.menhir
    ];

    propagatedBuildInputs = with ocamlPackages; [
      iso8601
      ez_file
      menhirLib
      ocplib_stuff
    ];
  };

  ezr_toml = ocamlPackages.buildDunePackage {
    pname = "ezr_toml";
    inherit version src;

    minimalOcamlVersion = "4.14";
    duneVersion = "3";

    propagatedBuildInputs = with ocamlPackages; [
      ez_toml
      pretty
    ];
  };

  superbol_project = ocamlPackages.buildDunePackage {
    pname = "superbol_project";
    inherit version src;

    minimalOcamlVersion = "4.14";
    duneVersion = "3";

    propagatedBuildInputs = with ocamlPackages; [
      cobol_preproc
      ezr_toml
    ];
  };

  cobol_lsp = ocamlPackages.buildDunePackage {
    pname = "cobol_lsp";
    inherit version src;

    minimalOcamlVersion = "4.14";
    duneVersion = "3";

    propagatedBuildInputs = with ocamlPackages; [
      superbol_project
      superbol_preprocs
      cobol_cfg
      cobol_indent
      toml
      jsonrpc
      lsp
    ];
  };

  ppx_cobcflags = ocamlPackages.buildDunePackage {
    pname = "ppx_cobcflags";
    inherit version src;

    minimalOcamlVersion = "4.14";
    duneVersion = "3";

    propagatedBuildInputs = with ocamlPackages; [
      ppxlib
    ];

  };

  vscode-json = ocamlPackages.buildDunePackage {
    pname = "vscode-json";
    inherit version src;

    minimalOcamlVersion = "4.14";
    duneVersion = "3";

    propagatedBuildInputs = with ocamlPackages; [
      ez_file
      ppx_deriving
      ppx_deriving_encoding
      ezjsonm
    ];
  };

  superbol_free_lib = ocamlPackages.buildDunePackage {
    pname = "superbol_free_lib";
    inherit version src;

    minimalOcamlVersion = "4.14";
    duneVersion = "3";

    propagatedBuildInputs = with ocamlPackages; [
      cobol_lsp
      ppx_cobcflags
      vscode-json

      ez_api
      ez_cmdliner
      ppx_deriving_encoding
    ];

  };

  superbol-free = ocamlPackages.buildDunePackage {
    pname = "superbol-free";
    inherit version src;

    minimalOcamlVersion = "4.14";
    duneVersion = "3";

    propagatedBuildInputs = [
      superbol_free_lib
    ];
  };

  superbol-studio-oss = ocamlPackages.buildDunePackage {
    pname = "superbol-studio-oss";
    inherit version src;

    minimalOcamlVersion = "4.14";
    duneVersion = "3";

    buildInputs = [
      superbol-free
    ];
  };
in

{
  inherit
    superbol-studio-oss
    superbol-free
    superbol_free_lib
    vscode-json
    ppx_cobcflags
    cobol_lsp
    superbol_project
    ezr_toml
    ez_toml
    superbol_preprocs
    sql_parser
    sql_ast
    cobol_cfg
    cobol_typeck
    cobol_parser
    cobol_unit
    cobol_preproc
    ebcdic_lib
    cobol_data
    cobol_ptree
    cobol_indent
    cobol_config
    cobol_common
    pretty;
}

