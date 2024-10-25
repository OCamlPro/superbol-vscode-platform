(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2021-2023 OCamlPro SAS                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the                       *)
(*  OCAMLPRO-NON-COMMERCIAL license.                                      *)
(*                                                                        *)
(**************************************************************************)

val preproc :
  filename:string ->
  ?sql_in_copybooks:bool ->
  ?copy_path:string list ->
  ?copy_exts:string list ->
  ?contents:string ->
  source_format:Cobol_indent.Types.source_format ->
  cobol_unit:Cobol_unit.Types.cobol_unit ->
  unit ->
  string
