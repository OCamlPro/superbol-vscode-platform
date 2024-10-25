(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2021-2023 OCamlPro SAS                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the                       *)
(*  OCAMLPRO-NON-COMMERCIAL license.                                      *)
(*                                                                        *)
(**************************************************************************)

open EzCompat

val error : ?loc:Types.loc -> ('a, unit, string, 'b) format4 -> 'a

val warning : ?loc:Types.loc -> ('a, unit, string, unit) format4 -> 'a

val loc_of_edit : filename:string -> Cobol_indent.Types.token_descr -> Types.loc

val string_of_token : Cobol_indent.Types.token -> string

val add_dot : with_dot:bool -> Buffer.t -> unit

val resolve_copy : config:Types.config -> string -> string

val extract_filename : string -> string

val replace_colon_words :
  cobol_unit:Cobol_unit.Types.cobol_unit -> string -> string

val cob_var_extractor_folder : Sql_ast.cobol_var list Sql_ast.Visitor.folder
