(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*                                                                        *)
(*  Copyright (c) 2023 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This source code is licensed under the MIT license found in the       *)
(*  LICENSE.md file in the root directory of this source tree.            *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

open Vscode_json
open Manifest

let vscode_engine = "1.64.0"


let marketplace =
  Manifest.marketplace
    "OCamlPro"
    ~icon:"assets/superbol-128.png"
    ~categories: [
      "Formatters" ;
      "Programming Languages" ;
      "Linters" ;
      "Snippets" ;
      "Debuggers";
      "Other";
    ]

let package =
  Manifest.package
    "SuperBOL"
    ~displayName: "SuperBOL Studio OSS"
    ~description: "Provides a COBOL mode in Visual Studio Code, based on the \
                   SuperBOL LSP server for COBOL, a source level debugger, and \
                   commands for GnuCOBOL 3.2+"
    ~license: "MIT"
    ~version: Version.version
    ~repository: {
      type_ = Some "git" ;
      url = "https://github.com/OCamlPro/superbol-studio-oss"
    }
    ~homepage: "https://get-superbol.com"
    ~author: {
      author_name = "SuperBOL at OCamlPro" ;
      author_email = Some "contact@ocamlpro.com"
    }
    ~keywords: [ "cobol" ; "gnucobol" ]
    ~main: "./_dist/superbol-vscode-platform-bundle"
    ~scripts: [
      "compile",
      "make compile";

      "package",
      "make vsix-package";

      "deploy:vsce",
      "make deploy-vsce";

      "deploy:ovsx",
      "make deploy-ovsx";
    ]
    ~dependencies: [
      "@vscode/debugadapter", "^1.61.0";
      "@vscode/debugprotocol", "^1.61.0";
      "vscode-languageclient", "8.0.2";
      "polka", "^1.0.0-next.22";
      "sirv", "^2.0.2";

      (* for the debug extension: *)
      "n-readlines", "^1.0.0";
    ]
    ~devDependencies: [
      "@types/vscode", "^" ^ vscode_engine;
      "esbuild", "^0.15.16";
      "fs-extra", "^10.0.1";
      "mocha", "^9.2.2";
      "npm-run-all", "^4.1.5";
      "ovsx", "^0.1.0";
      "prettier", "^2.5.1";
      "@vscode/vsce", "^2.15.0";
      "@vscode/test-electron", "^1.6.1";

      (* for the debug extension: *)
      "typescript", "^5.1.6";
      "@types/node", "^20.3.2";
    ]

let cob_extensions_pattern = "[cC]{ob,OB,bl,BL,py,PY,bx,BX,bsql}"
let contributes =
  Manifest.contributes ()
    ~languages: [
      Manifest.language "cobol"
        ~aliases: ["COBOL"]
        ~filenamePatterns: ["*." ^ cob_extensions_pattern]
        ~configuration: "./syntaxes/language-configuration.json";
      Manifest.language "COBOL_GNU_LISTFILE"
        ~aliases: ["LISTFILE"]
        ~extensions: [".lst"]
        ~configuration: "./syntaxes/list-n-dump-configuration.json";
      Manifest.language "COBOL_GNU_DUMPFILE"
        ~aliases: ["DUMPFILE"]
        ~extensions: [".dump"]
        ~configuration: "./syntaxes/list-n-dump-configuration.json";
    ]
    ~grammars: [
      Manifest.grammar ()
        ~language: "cobol"
        ~scopeName: "source.cobol"
        ~path: "./syntaxes/COBOL.tmLanguage.json"
        ~embeddedLanguages: [
          "meta.embedded.block.sql", "sql";
        ];
      Manifest.grammar ()
        ~language: "COBOL_GNU_LISTFILE"
        ~scopeName: "source.gnucobol_listfile"
        ~path: "./syntaxes/listfile.tmLanguage.json";
      Manifest.grammar ()
        ~language: "COBOL_GNU_DUMPFILE"
        ~scopeName: "source.gnucobol_dumpfile"
        ~path: "./syntaxes/dumpfile.tmLanguage.json"
    ]
    ~debuggers: [
      Manifest.debugger "gdb"
        ~label: "SuperBOL Debugger for GnuCOBOL"
        ~languages: ["cobol"; "COBOL"]
        ~program: "./_dist/superbol-vscode-gdb.js"
        ~runtime: "node"
        ~configurationAttributes:
          (Manifest.any {| {
                "launch": {
                        "required": [],
                        "properties": {
                                "target": {
                                        "type": "string",
                                        "description": "Path to debugged executable",
                                        "default": "${file}"
                                },
                                "arguments": {
                                        "type": "string",
                                        "description": "Extra arguments for executable",
                                        "default": null
                                },
                                "cwd": {
                                        "type": "string",
                                        "description": "Path to project",
                                        "default": "${workspaceFolder}"
                                },
                                "gdb-path": {
                                        "type": "string",
                                        "description": "Path to gdb",
                                        "default": "gdb"
                                },
                                "libcob-path": {
                                        "type": "string",
                                        "description": "Path to libcob",
                                        "default": null
                                },
                                "group": {
                                        "type": "array",
                                        "description": "Compilation groups of executable",
                                        "default": []
                                },
                                "env": {
                                        "type": "object",
                                        "description": "Environment variables",
                                        "default": null
                                },
                                "coverage": {
                                        "type": "boolean",
                                        "description": "Enable code coverage",
                                        "default": true
                                },
                                "verbose": {
                                        "type": "boolean",
                                        "description": "Debug GDB",
                                        "default": false
                                },
                                "gdbtty": {
                                        "type": [
                                                "boolean",
                                                "string"
                                        ],
                                        "description": "Enable external display for debug",
                                        "default": false,
                                        "enum": [
                                                true,
                                                false,
                                                "vscode",
                                                "xterm",
                                                "gnome-terminal",
                                                "xfce4-terminal",
                                                "konsole",
                                                "external"
                                        ]
                                }
                        }
                },
                "attach": {
                        "required": [],
                        "properties": {
                                "target": {
                                        "type": "string",
                                        "description": "Path to executable",
                                        "default": "${file}"
                                },
                                "arguments": {
                                        "type": "string",
                                        "description": "Extra arguments for executable",
                                        "default": null
                                },
                                "cwd": {
                                        "type": "string",
                                        "description": "Path to project",
                                        "default": "${workspaceFolder}"
                                },
                                "gdb-path": {
                                        "type": "string",
                                        "description": "Path to gdb",
                                        "default": "gdb"
                                },
                                "libcob-path": {
                                        "type": "string",
                                        "description": "Path to libcob",
                                        "default": null
                                },
                                "group": {
                                        "type": "array",
                                        "description": "Compilation groups of executable",
                                        "default": []
                                },
                                "env": {
                                        "type": "object",
                                        "description": "Environment variables",
                                        "default": null
                                },
                                "verbose": {
                                        "type": "boolean",
                                        "description": "Debug GDB",
                                        "default": false
                                },
                                "pid": {
                                        "type": "string",
                                        "description": "PID of the executable",
                                        "default": null
                                },
                                "remote-debugger": {
                                        "type": "string",
                                        "description": "GDB Server host:port",
                                        "default": null
                                }
                        }
                }
        } |})
        ~configurationSnippets: [
          Manifest.any {| {
                                "label": "SuperBOL: debug (launch)",
                                "description": "New SuperBOL launch request",
                                "body": {
                                        "name": "${2:SuperBOL: debug (launch)}",
                                        "type": "gdb",
                                        "request": "launch",
                                        "preLaunchTask": "SuperBOL: build (debug)",
                                        "target": "$${_:{file}}",
                                        "arguments": "",
                                        "cwd": "$${_:{workspaceFolder}}",
                                        "group": [],
                                        "coverage": true,
                                        "verbose": false,
                                        "gdbtty": true
                                }
                        } |};
          Manifest.any {| {
                                "label": "SuperBOL: debug (attach local)",
                                "description": "Attach to a local debug session",
                                "body": {
                                        "name": "${2:SuperBOL: debug (attach local)}",
                                        "type": "gdb",
                                        "request": "attach",
                                        "pid": "${3:0}",
                                        "target": "$${_:{file}}",
                                        "arguments": "",
                                        "cwd": "$${_:{workspaceFolder}}",
                                        "group": [],
                                        "verbose": false
                                }
                        } |};
          Manifest.any {| {
                                "label": "SuperBOL: debug (attach remote)",
                                "description": "Attach to a remote debug session",
                                "body": {
                                        "name": "${2:SuperBOL: debug (attach remote)}",
                                        "type": "gdb",
                                        "request": "attach",
                                        "remote-debugger": "${3:host:port}",
                                        "target": "$${_:{file}}",
                                        "arguments": "",
                                        "cwd": "$${_:{workspaceFolder}}",
                                        "group": [],
                                        "verbose": false
                                }
                        } |};
        ]
    ]
    ~breakpoints: [
      Manifest.breakpoint "cobol";
    ]
    ~configuration:
      (let with_superbol_toml_note md =
         md ^ "\n\n*(May be overriden in project-specific `superbol.toml` files)*."
       in
       Manifest.configuration ~title:"SuperBOL"
         [

           (* Note: entries that belong to the `superbol.cobol` section is
              transmitted to the LSP server, and needs to exactly mirror the
              `[cobol]` section in `superbol.toml`.  There, convention for keys
              is to use dash-separated words in lowercase.  *)

           Manifest.PROPERTY.enum "superbol.cobol.dialect"
             ~cases:Cobol_config.DIALECT.all_canonical_names
             ~markdownDescription:
               (with_superbol_toml_note
                  "Default COBOL dialect; \"default\" is equivalent to \
                   \"gnucobol\".")
             ~default:(`String Cobol_config.(DIALECT.to_string Default))
             ~order:1;

           Manifest.PROPERTY.enum "superbol.cobol.source-format"
             ~title:"Source Reference-format"
             ~markdownDescription:
               (with_superbol_toml_note "Default source reference-format.")
             ~cases:Cobol_config.Options.all_format_names
             ~default:(`String "auto")
             ~order:2;

           Manifest.PROPERTY.array "superbol.cobol.copybooks"
             ~title:"Copybook Paths"
             ~items:(`O [
                 "type", `String "object";
                 "required", `A [`String "dir"];
                 "properties", `O [
                   "dir", `O [
                     "type", `String "string";
                     "description", `String "Path to copybooks";
                   ];
                   "file-relative", `O [
                     "type", `String "boolean";
                   ];
                 ];
               ])
             ~default:(`A [                 (* Should match `default_libpath` in
                                               `superbol_project/project_config.ml` *)
                 `O [
                   "dir", `String ".";
                 ];
               ])
             ~markdownDescription:
               (with_superbol_toml_note "List of copybooks paths.")
             ~order:3;

           Manifest.PROPERTY.strings "superbol.cobol.copyexts"
             ~markdownDescription:
               (with_superbol_toml_note
                  "File extensions for copybook resolution")
             ~default:Cobol_common.Copybook.copybook_extensions
             ~order:4;

           (* Paths *)

           Manifest.PROPERTY.string "superbol.cobc-path"
             ~title:"GnuCOBOL Compiler Executable"
             ~default:"cobc"
             ~scope:"machine-overridable"
             ~description:"Path to the GnuCOBOL compiler executable."
             ~order:11;

           Manifest.PROPERTY.string "superbol.lsp-path"
             ~title:"SuperBOL Executable"
             ~default:""
             ~scope:"machine"
             ~markdownDescription:
               "Name of the `superbol-free` executable if available in PATH; may \
                be an absolute path otherwise. Leave empty to use the bundled \
                `superbol-free`, if available."
             ~order:12;

           (* Flags *)

           Manifest.PROPERTY.bool "superbol.forceSyntaxDiagnostics"
             ~default:false
             ~markdownDescription:
               "Force reporting of syntax diagnostics for dialects other than \
                ``COBOL85``."
             ~order:21;

           Manifest.PROPERTY.bool "superbol.cacheInGlobalStorage"
             ~default:false
             ~markdownDescription:
               "Use storage provided by Visual Studio Code for caching.  When \
                this setting is set to *false*, the cache related to the \
                contents of a given workspace folder *f* is stored in a file \
                named *f*`/_superbol/lsp-cache`.\n\nNote: cache files are not \
                removed automatically, whatever their location."
             ~order:22;

           (* Debugger-specific: *)

           Manifest.PROPERTY.bool
             "superbol.debugger.display-variable-attributes"
             ~title:"Display Variable Attributes"
             ~default:false
             ~scope:"resource"
             ~description:"Display storage property and field attributes \
                           (e.g. size of alphanumerics, digits and scale of \
                           numerics)."
             ~order:31;

           Manifest.PROPERTY.null_string "superbol.debugger.libcob-path"
             ~title:"GnuCOBOL Runtime Library"
             ~scope:"machine-overridable"
             ~description:"Path to the GnuCOBOL runtime library file."
             ~order:32;

           Manifest.PROPERTY.string "superbol.debugger.gdb-path"
             ~title:"GNU Debugger Executable"
             ~default:"gdb"
             ~scope:"machine-overridable"
             ~description:"Path to the GNU debugger executable."
             ~order:33;

         ])
    ~taskDefinitions: [
      Manifest.taskDefinition
        "superbol"
        ~properties: [
          Manifest.PROPERTY.bool "for-debug"
            ~description:"Build for debugging";

          Manifest.PROPERTY.bool "for-coverage"
            ~description:"Enable instrumentation for coverage";

          Manifest.PROPERTY.bool "executable"
            ~description:"Build an executable program instead of a module"
            ~default:true;

          Manifest.PROPERTY.null_string "cobc-path"
            ~title:"GnuCOBOL Compiler Executable"
            ~markdownDescription:
              "Path to the GnuCOBOL compiler executable; when `null`, defaults \
               to the value of \"superbol.cobc-path\" from the workspace \
               configuration, if defined, to \"cobc\" otherwise.";

          Manifest.PROPERTY.null_string "listings-target"
            ~title:"Output file or directory for preprocessed program listings"
            ~markdownDescription:
              "Path to a directory where preprocessed program listings are \
               generated; no listing is saved when `null`";

          Manifest.PROPERTY.array "extra-args"
            ~description:"Additional arguments passed to `cobc`";
        ]
    ]
    ~configurationDefaults: [
      "[cobol]",
      Defaults [
        "files.autoGuessEncoding", `Bool false;
        "editor.insertSpaces", `Bool true;
        "editor.formatOnType", `Bool false;
        "editor.autoIndent", `String "full";
        "editor.detectIndentation", `Bool false;
        (* TODO: see if we can tune that directly from the client, checking the
           configured format beforehand. *)
        (* "editor.rulers", `A [ *)
        (*   `Float 6.; *)
        (*   `Float 7.; *)
        (*   `Float 72.; *)
        (* ]; *)
        "editor.semanticHighlighting.enabled", `Bool true;
        "editor.wordSeparators", `String "`~!#$%^&*()=+[{]}\\|;:'\",.<>/?";
      ];
      "[sql]",
      Defaults [
        "editor.wordSeparators", `String "`~!@#$%^&*()=+[{]}\\|;:'\",.<>/?";
      ]
    ]
    ~semanticTokenScopes: [
      `O [
        "language", `String "cobol";
        "scopes", `O [
          (* Here, we map semantic token types to textmate scope names that are
             defined in `COBOL.tmLanguage.json`: this aligns the theming
             attributes that are associated with the former types with the ones
             that are associated with the latter. *)
          "keyword", `A [`String "keyword.verb.cobol"];
          "macro", `A [`String "keyword.control.directive.cobol"];
        ];
      ]
    ]
    ~problemPatterns: [
      Manifest.problemPattern
        (Some "^(.*): ?(\\d+): (error|warning): ([^[]*)(\\[(.*)\\])?$")
        ~name:"gnucobol"
        ~file:1
        ~line:2
        ~severity:3
        ~message:4
        ~code:6;
      Manifest.problemPattern
        (Some "^(.*):(\\d+):\\s?(warning|Warnung|[wW]aarschuwing|[aA]lerta|avertissement|упозорење)\\s?:([^[]*)(\\[(.*)\\])?$")
        ~name:"gnucobol-warning"
        ~file:1
        ~line:2
        ~message:4
        ~code:6;
      Manifest.problemPattern
        (Some "^(.*): ?(\\d+):\\s?(error|Fehler|[fF]out|[eE]rrores|[eE]rrores|erreur|грешка)\\s?:\\s?([^[]*)(\\[(.*)\\])?$")
        ~name:"gnucobol-error"
        ~file:1
        ~line:2
        ~message:4
        ~code:6;
      Manifest.problemPattern
        (Some "^(.*): ?(\\d+): (note|Anmerkung|[nN]ota): ([^[]*)(\\[(.*)\\])?$")
        ~name:"gnucobol-note"
        ~file:1
        ~line:2
        ~message:4
        ~code:6;
    ]
    ~problemMatchers:[
      Manifest.problemMatcher ()
        ~name:"gnucobol"
        ~owner:"cobol"
        ~fileLocation:["autodetect"]
        ~pattern:[Manifest.ProblemName "$gnucobol"]
        ~source:"GnuCOBOL";
      Manifest.problemMatcher ()
        ~name:"gnucobol-warning"
        ~owner:"cobol"
        ~fileLocation:["autodetect"]
        ~pattern:[Manifest.ProblemName "$gnucobol-warning"]
        ~severity:"warning"
        ~source:"GnuCOBOL";
      Manifest.problemMatcher ()
        ~name:"gnucobol-error"
        ~owner:"cobol"
        ~fileLocation:["autodetect"]
        ~pattern:[Manifest.ProblemName "$gnucobol-error"]
        ~severity:"error"
        ~source:"GnuCOBOL";
      Manifest.problemMatcher ()
        ~name:"gnucobol-note"
        ~owner:"cobol"
        ~fileLocation:["autodetect"]
        ~pattern:[Manifest.ProblemName "$gnucobol-note"]
        ~severity:"info"
        ~source:"GnuCOBOL";
    ]
    ~commands: [
      Manifest.command ()
        ~command:"superbol.server.restart"
        ~title:"Restart Language Server"
        ~category:"SuperBOL";
      Manifest.command ()
        ~command:"superbol.write.project.config"
        ~title:"Write Project Configuration"
        ~category:"SuperBOL"
      (* ~enablement:"!inDebugMode" *);
      Manifest.command ()
        ~command:"superbol.coverage.show"
        ~title:"Show Coverage"
        ~category:"SuperBOL";
      Manifest.command ()
        ~command:"superbol.coverage.hide"
        ~title:"Hide Coverage"
        ~category:"SuperBOL";
      Manifest.command ()
        ~command:"superbol.coverage.reload"
        ~title:"Update Coverage"
        ~category:"SuperBOL";
      Manifest.command ()
        ~command:"superbol.cfg.open"
        ~title:"Show Control-flow"
        ~category:"SuperBOL";
      Manifest.command ()
        ~command:"superbol.cfg.open.arc"
        ~title:"Show Control-flow as an Arc-diagram"
        ~category:"SuperBOL";
    ]
    ~tomlValidation: [
      Manifest.tomlValidation
        ~fileMatch:"superbol.toml"
        (* TODO: change this address to a more permanent one; also, substitute `master` for a version tag *)
        ~url:"https://raw.githubusercontent.com/OCamlPro/superbol-studio-oss/master/schemas/superbol-schema-0.1.4.json";
    ]
    ~menus: [
      "editor/context",
      [menu ~command:"superbol.cfg.open" ~group:"superbol"
         ~when_:"editorTextFocus && editorLangId == 'cobol'" ();
       menu ~command:"superbol.cfg.open.arc" ~group:"superbol"
         ~when_:"editorTextFocus && editorLangId == 'cobol'" ()]
    ]

let manifest =
  Manifest.vscode
    package
    ~marketplace
    ~engines: ("^" ^ vscode_engine)
    ~activationEvents: [
      "onLanguage:cobol"; (* Note: optional since VS Code 1.74, as in
                             `contributes`) *)
      (* XXX: should we really expect mixed-case file extensions like
         `prog.coB`? *)
      "workspaceContains:**/*." ^ cob_extensions_pattern;
      "workspaceContains:{_superbol,superbol.toml}";
      (* "onDebug"; *) (* <- not relevant yet *)
    ]
    ~extensionKind: [
      "workspace";                                 (* <- run on the workspace *)
    ]
    ~contributes
    ~extensionDependencies: [
      "JacquesLucke.gcov-viewer";
    ]
