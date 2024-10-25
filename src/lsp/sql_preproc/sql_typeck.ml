(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2021-2023 OCamlPro SAS                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the                       *)
(*  OCAMLPRO-NON-COMMERCIAL license.                                      *)
(*                                                                        *)
(**************************************************************************)

open Cobol_data.Types

let get_x_info (cu : Cobol_unit.Types.cobol_unit) name_str =
  (* TODO: this needs to include refs from EXEC SQL INCLUDE stmt : TSQL042A *)
  (* May raise Not_found | Cobol_unit.Qualmap.Ambiguous _ *)
  Cobol_unit.Qualmap.find
    (Cobol_unit.Qual.name
       (Cobol_common.Srcloc.flagit name_str Cobol_common.Srcloc.dummy) )
    cu.unit_data.data_items.named

let get_length cu name =
  try
    let x_info = get_x_info cu name in
    match x_info with
    | Data_field { def = { payload = { field_size; field_layout; _ }; _ }; _ } ->
        let size = Cobol_data.Memory.(as_bits field_size / 8) in
        begin match field_layout with
        | Elementary_field { usage = Packed_decimal pic; _ } ->
            begin match pic.category with
            | FixedNum { digits; _ } -> digits
            | _ -> size end
        | _ -> size end
    | _ -> 0
  with
  | Not_found ->
    (*     Pretty.out " \"%s\" not found " name; *)
    0
  | Cobol_unit.Qualmap.Ambiguous _ ->
    (*     Pretty.out " \"%s\" not found. qualname nel lazy_t found" name; *)
    0

type cobol_types =
  | UNKNOWN
  | COBOL_TYPE_UNSIGNED_NUMBER
  (*ex: PIC 9(018).*)
  | COBOL_TYPE_SIGNED_NUMBER_TS   (* trailing separate *)
  (*pas d'exemple dans les tests de gix que j'ai réussi a preprocesser*)
  | COBOL_TYPE_SIGNED_NUMBER_TC   (* trailing combined *)
  (*ex: PIC S9(09)
        PIC S9(018)*)
  | COBOL_TYPE_SIGNED_NUMBER_LS   (* leading separate  *)
  (*pas d'exemple*)
  | COBOL_TYPE_SIGNED_NUMBER_LC   (* leading combined  *)
  (*pas d'exemple*)
  | COBOL_TYPE_UNSIGNED_NUMBER_PD (* packed decimal    *)
  (*ex: PIC 9(018)        COMP-3.
        PIC 9(018)V9(12)  COMP-3*)
  | COBOL_TYPE_SIGNED_NUMBER_PD   (* packed decimal    *)
  (*ex: PIC S9(018)V9(12) COMP-3
        PIC S9(018)V9(12) COMP-3.
        PIC S99V99 COMP-3.
        03 FLD01      PIC S9(4) USAGE COMP-3.    (???? USAGE?) *)
  | COBOL_TYPE_ALPHANUMERIC
  | COBOL_TYPE_UNSIGNED_BINARY
  | COBOL_TYPE_SIGNED_BINARY
  (*pas d'exemple*)
  | COBOL_TYPE_JAPANESE
  (*pas d'exemple*)
  | COBOL_TYPE_GROUP
  (*pas d'exemple*)
  | COBOL_TYPE_FLOAT
  (*pas d'exemple*)
  | COBOL_TYPE_DOUBLE
  (*pas d'exemple*)
  | COBOL_TYPE_NATIONAL
  (*pas d'exemple*)

let cobol_types_to_int = function
  | UNKNOWN -> 0
  | COBOL_TYPE_UNSIGNED_NUMBER -> 1
  | COBOL_TYPE_SIGNED_NUMBER_TS -> 2
  | COBOL_TYPE_SIGNED_NUMBER_TC -> 3
  | COBOL_TYPE_SIGNED_NUMBER_LS -> 4
  | COBOL_TYPE_SIGNED_NUMBER_LC -> 5
  | COBOL_TYPE_UNSIGNED_NUMBER_PD -> 8
  | COBOL_TYPE_SIGNED_NUMBER_PD -> 9
  | COBOL_TYPE_ALPHANUMERIC -> 16
  | COBOL_TYPE_UNSIGNED_BINARY -> 22
  | COBOL_TYPE_SIGNED_BINARY -> 23
  | COBOL_TYPE_JAPANESE -> 24
  | COBOL_TYPE_GROUP -> 25
  | COBOL_TYPE_FLOAT -> 26
  | COBOL_TYPE_DOUBLE -> 27
  | COBOL_TYPE_NATIONAL -> 28

let get_type cu name =
  let cobol_type =
    try
      let x_info = get_x_info cu name in
      match x_info with
      | Data_field { def = { payload = { field_layout; _ }; _ }; _ } -> begin
        match field_layout with
        | Elementary_field { usage = Packed_decimal picture; _ } ->
            (match picture.category with
            | FixedNum { with_sign=true; _ } ->
            COBOL_TYPE_SIGNED_NUMBER_PD
            | FixedNum { with_sign=false; _ } ->
            COBOL_TYPE_UNSIGNED_NUMBER_PD
            | _ -> UNKNOWN)
        | Elementary_field { usage = Display picture; _ } -> (
          match picture.category with
          | Alphabetic _ -> COBOL_TYPE_ALPHANUMERIC (*?*)
          | Alphanumeric _ -> COBOL_TYPE_ALPHANUMERIC
          | Boolean _ -> COBOL_TYPE_UNSIGNED_BINARY (*?*)
          | National _ -> COBOL_TYPE_NATIONAL
          | FixedNum { with_sign; _ } ->
            if with_sign then
              COBOL_TYPE_SIGNED_NUMBER_TC
            else
              COBOL_TYPE_UNSIGNED_NUMBER
          | FloatNum _ -> UNKNOWN )
        | Elementary_field _
        | Struct_field _ ->
          UNKNOWN
      end
      | _ -> UNKNOWN
    with
    | Not_found ->
      (*     Pretty.out " \"%s\" not found " name; *)
      UNKNOWN
    | Cobol_unit.Qualmap.Ambiguous _ ->
      (*     Pretty.out " \"%s\" not found. qualname nel lazy_t found" name; *)
      UNKNOWN
  in
  cobol_types_to_int cobol_type

let get_scale cu name =
  try
    let x_info = get_x_info cu name in
    match x_info with
    | Data_field { def = { payload = { field_layout; _ }; _ }; _ } -> begin
      match field_layout with
      | Elementary_field { usage = Packed_decimal picture; _ }
      | Elementary_field { usage = Display picture; _ } -> (
        match picture.category with
        | FixedNum { scale; _ }
        | FloatNum { scale; _ } ->
          scale
        | _ -> 0 )
      | Elementary_field _
      | Struct_field _ ->
        0
    end
    | _ -> 0
  with
  | Not_found ->
    (*     Pretty.out " \"%s\" not found " name; *)
    0
  | Cobol_unit.Qualmap.Ambiguous _ ->
    (*     Pretty.out " \"%s\" not found. qualname nel lazy_t found" name; *)
    0

let get_elementary_component cu name =
  let rec aux { field_layout; field_qualname; _ } =
    let name = match field_qualname with
      | None -> []
      | Some { payload = Qual (name, _) ; _ }
      | Some { payload = Name name; _ } ->
        [name.payload]
    in
    match field_layout with
    | Elementary_field _ -> name
    | Struct_field { subfields } ->
      List.rev @@
      NEL.fold_left [] subfields
        ~f:begin fun acc (item_def: _ Cobol_common.with_loc) ->
          match item_def.payload with
          | Field field_def ->
            aux field_def @ acc
          | Table { table_field = { payload; _ }; _ } ->
            aux payload @ acc
        end
  in
  try
    let x_info = get_x_info cu name in
    match x_info with
    | Data_field { def = { payload; _ }; _ } ->
      aux payload
    | _ -> [name]
  with
  | Not_found | Cobol_unit.Qualmap.Ambiguous _ -> [name]


(*TODO*)
let get_flags _cu _name = 0

(*TODO*)
let get_ind_addr _cu _name = 0

let print_name (cu : Cobol_unit.Types.cobol_unit) =
  let x_info = get_x_info cu "VBFLD" in
  match x_info with
  | Data_field { def = { payload = { field_layout; field_size; _ }; _ }; _ } ->
    Pretty.out "Size of VBFLD is %u Bytes@."
      Cobol_data.Memory.(as_bits field_size / 8);
    begin
      match field_layout with
      | Elementary_field { usage = Display picture; _ } -> (
        Pretty.out "PIC is %a@." Cobol_data.Picture.pp picture;
        match picture.category with
        | FixedNum { digits = _; scale = _; with_sign = _; _ } -> ()
        | _ -> () )
      | Elementary_field _
      | Struct_field _ ->
        ()
    end
  | _ -> ()
