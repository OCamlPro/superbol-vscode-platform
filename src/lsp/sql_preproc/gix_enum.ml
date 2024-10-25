(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2021-2023 OCamlPro SAS                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the                       *)
(*  OCAMLPRO-NON-COMMERCIAL license.                                      *)
(*                                                                        *)
(**************************************************************************)

(*
#define CBL_FIELD_FLAG_NONE		(uint32_t)0x0
#define CBL_FIELD_FLAG_VARLEN	(uint32_t)0x80
#define CBL_FIELD_FLAG_BINARY	(uint32_t)0x100
#define CBL_FIELD_FLAG_AUTOTRIM	(uint32_t)0x200
*)

module Flag = struct
  let none = 0x0
  let varlen = 0x80
  let binary = 0x100
  let autotrim = 0x200
end
