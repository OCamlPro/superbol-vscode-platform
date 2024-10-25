(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2022-2023 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

module EXEC_MAP = Cobol_preproc.Options.EXEC_MAP

let exec_scanners =
  Cobol_parser.Options.{
    exec_scanner_fallback = Generic.scanner;  (* for now; TODO: Call.scanner? *)
    (* NB: Kept empty for now (the LSP does not yet benefit from this
       preprocessor) *)
    (* exec_scanners = Cobol_preproc.Options.EXEC_MAP.empty; *)
    exec_scanners = Cobol_preproc.Options.EXEC_MAP.singleton "SQL" Esql.scanner;
  }

let more scanners =
  List.fold_left begin fun acc (name, scanner) ->
    Cobol_parser.Options.{
      exec_scanner_fallback = acc.exec_scanner_fallback;
      exec_scanners = EXEC_MAP.add name scanner acc.exec_scanners
    }
  end exec_scanners scanners
