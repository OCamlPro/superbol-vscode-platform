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

type error =
  | Failure of string

exception Error of error

type loc = { filename : string ;
             line : int ;
             char : int }

type sql_type =
  | Binary of int
  | Varbinary of int
  | Char of int
  | Varchar of int
  | Float of int * int option (* FLOAT(8) or FLOAT(4,2) *)

type declaration =
  | SQL_type_is of { importance:string; name:string; sql_type: sql_type }

(* These statements show how we could keep information and modify the
   corresponding places in the code *)

type statements =
  | PROCEDURE_DIVISION_DOT of { end_loc : loc }
  | WORKING_STORAGE of { defined: bool }
  | LINKAGE_SECTION of { defined: bool }
  | EXEC_SQL of { end_loc : loc ;
                   with_dot : bool ;
                   tokens : Sql_ast.esql_instruction ;
                 }
  | BEGIN_PROCEDURE_DIVISION of { enabled : bool ref }
  | END_PROCEDURE_DIVISION
  | COPY of { end_loc : loc ; filename : string ; contents : string }
(*   | IS_SQLVAR of { end_loc : loc } *)
  | DECLARATION of { end_loc:loc; declaration : declaration }
  | EXEC_SQL_IGNORE of { end_loc: loc; begin_of_ignore_loc: loc }


type handle = {
  mutable handle_abend : string ;
}

type gen_context = {
  b : Buffer.t ;
  main_filename : string ;
}

type config = {
  scanner_config: Cobol_indent.Types.config ;
  sql_in_copybooks : bool;
  copy_path : ( string * string StringMap.t) list Lazy.t ;
  copy_exts : string list ;
  verbosity : int ;
}
