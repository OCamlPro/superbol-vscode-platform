(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2021-2023 OCamlPro SAS                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the                       *)
(*  OCAMLPRO-NON-COMMERCIAL license.                                      *)
(*                                                                        *)
(**************************************************************************)

val generate :
  filename:string ->
  contents:string ->
  cobol_unit:Cobol_unit.Types.cobol_unit ->
  (Types.loc option * Types.statements) list ->
  string
