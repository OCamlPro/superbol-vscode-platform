{ sources ? import ./sources.nix } : 

import sources.nixpkgs {
  overlays = [
    (_: pkgs: { inherit sources; })
    (_: pkgs: {
        ocamlPackages = pkgs.ocaml-ng.ocamlPackages_4_14.overrideScope (self: super: {
          ez_api = pkgs.callPackage ./ez_api.nix { };
          ez_cmdliner = pkgs.callPackage ./ez_cmdliner.nix { };
          ez_file = pkgs.callPackage ./ez_file.nix { };
          ez_subst = pkgs.callPackage ./ez_subst.nix { };
          json-data-encoding = pkgs.callPackage ./json-data-encoding.nix { };
          lsp = pkgs.callPackage ./lsp.nix { };
          menhir = pkgs.callPackage ./menhir.nix { };
          menhirLib = pkgs.callPackage ./menhirLib.nix { };
          menhirSdk = pkgs.callPackage ./menhirSdk.nix { };
          ocplib_stuff = pkgs.callPackage ./ocplib_stuff.nix { };
          ppx_deriving_encoding = pkgs.callPackage ./ppx_deriving_encoding.nix { };
        });
    })
  ];
}
