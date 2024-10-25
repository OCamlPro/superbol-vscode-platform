(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2021-2023 OCamlPro SAS                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the                       *)
(*  OCAMLPRO-NON-COMMERCIAL license.                                      *)
(*                                                                        *)
(**************************************************************************)
type t

type variable_information =
  { length : int;
    vartype : int;
    scale : int;
    flags : int;
    ind_addr : int
  }
(*return working_storage_section (only declaration) * new_var_map*)
val transform :
  cobol_unit:Cobol_unit.Types.cobol_unit ->
  (Types.loc option * Types.statements) list -> string ->
  (Generated_type.trans_stm list * t)

val find_opt : t -> string -> variable_information option
