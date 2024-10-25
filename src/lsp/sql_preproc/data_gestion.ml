(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2021-2023 OCamlPro SAS                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the                       *)
(*  OCAMLPRO-NON-COMMERCIAL license.                                      *)
(*                                                                        *)
(**************************************************************************)

open Sql_ast
open Types
open Generated_type
module StringMap = Map.Make (String)

type variable_information =
  { length : int;
    vartype : int;
    scale : int;
    flags : int;
    ind_addr : int (* probably unused since my refactor *)
  }

type t = variable_information StringMap.t

let get_length = function
  | Binary size | Varbinary size | Varchar size | Char size -> size
  | Float (digit, scale_opt) ->
      Printf.eprintf "WARNING : Untested length of FLOAT field\n";
      digit + Option.value ~default:0 scale_opt

let get_type = function
  | Float _ -> Sql_typeck.(cobol_types_to_int COBOL_TYPE_FLOAT) (* not sure of that, may be double? *)
  | _ -> Sql_typeck.(cobol_types_to_int COBOL_TYPE_ALPHANUMERIC)

let get_flags = function
  | Binary _ -> Gix_enum.Flag.binary
  | Varbinary _ -> Gix_enum.Flag.binary lor Gix_enum.Flag.varlen
  | Varchar _ -> Gix_enum.Flag.varlen
  | Char _ | Float _ -> Gix_enum.Flag.none

let add_var ?(length = 0) ?(vartype = 0) ?(scale = 0) ?(flags = 0)
    ?(ind_addr = 0) map name =
  StringMap.add name { length; vartype; scale; flags; ind_addr } map

let num = ref 0

let transform_stm ~cobol_unit map (_, stm) filename =
  let prefix = Generated_type.Printer.preproc_prefix ^ " " in
  let create_new_var content ?(remplace=true) () =
    let new_content = if remplace then "\"" ^ Misc.replace_colon_words ~cobol_unit content ^ "\"" else content in
    let size = String.length new_content - 2 in
    (*Because " are part of this string"*)
    num := !num + 1;
    let var_name = "SQ" ^ string_of_int !num in
    let field =
      let prefix = prefix ^ "   " in
      [ Simple_var_declaration
          { prefix;
            var_importance = "02";
            var_name = None;
            var_type = "X(" ^ string_of_int size ^ ")";
            var_content = Some new_content
          };
        Simple_var_declaration
          { prefix;
            var_importance = "02";
            var_name = None;
            var_type = "X(1)";
            var_content = Some "X\"00\""
          }
      ]
    in
    ( [ Declaration
          (Field_var_declaration
             { prefix; var_importance = "01"; var_name; field } )
      ],
      add_var ~length:size map ("SQ" ^ string_of_int !num) )
  in
  let add_cur cur_name map ws filename =
    let pre_cur_name = "GIXSQL-CI-F-" ^ Misc.extract_filename filename ^ "-" in
    let ws =
      Declaration
        (Simple_var_declaration
           { prefix;
             var_importance = "01";
             var_name = Some (pre_cur_name ^ cur_name);
             var_type = "X";
             var_content = None
           } )
      :: ws
    in
    let map = add_var ~length:0 map (pre_cur_name ^ cur_name) in
    (ws, map)
  in

  let rec trans_sql tokens =
    match tokens with
    | At (_, sql) -> trans_sql sql
    | SelectInto { select; select_options; _ } ->
      let ws, map =
        create_new_var
          (Format.asprintf "SELECT %a%a" Sql_ast.Printer.pp_select_lst select
             Sql_ast.Printer.pp_select_options_lst select_options ) ()
      in
      (ws, map)
    | Begin ->
      let ws, map = create_new_var "BEGIN" () in
      (ws, map)
    | StartTransaction ->
      let ws, map = create_new_var "START TRANSACTION" () in
      (ws, map)
    | Sql sql -> (
      match sql with
      | Sql_ast.SqlInstr "VAR" ::
        Sql_ast.SqlInstr varname ::
        Sql_ast.SqlInstr "IS" ::
        Sql_ast.SqlInstr typ ::
        Sql_ast.SqlInstr "(" ::
        Sql_ast.SqlInstr size ::
        Sql_ast.SqlInstr ")" :: _ ->
          (* patch to fix TSQL005C, this is surely not generic enough
            and should probably be treated in parse.ml ? *)
          Printf.eprintf "Warning : EXEC SQL VAR with %s %s %s\n" varname typ size;
          let flags =
            if String.starts_with ~prefix:"VAR" typ
            then Gix_enum.Flag.autotrim
            else Gix_enum.Flag.none
          in
          let length = int_of_string size in
          let typ = match typ with
            | "BINARY" -> Binary length
            | "VARBINARY" -> Varbinary length
            | "VARCHAR" -> Varchar length
            | "CHAR" -> Char length
            | "FLOAT" -> Float (length, None)
            | unknown -> Pretty.failwith "Unknow type %s" unknown
          in
          let vartype = get_type typ in
          ([], add_var ~vartype ~length ~flags map varname)
      | Sql_ast.SqlInstr w :: _ when w = "VAR" ->
          Printf.eprintf "Warning : ignoring EXEC SQL VAR statement\n";
        ([], map)
        (*TODO: GIX uses this to add flag on certain variable
        (see TSQL005C, where VCFLD2 has flag varlen due to an EXEC SQL VAR statement)
         *)
      | _ ->
        let ws, map =
          create_new_var (Format.asprintf "%a" Sql_ast.Printer.pp_sql sql) ()
        in
        (ws, map) )
    | Insert _
    | Savepoint _
    | ReleaseSavepoint _
    | Delete _ ->
      let ws, map =
        create_new_var
        (Format.asprintf "%a" Sql_ast.Printer.pp_esql tokens) ()
      in
      (ws, map)
    | ExecuteImmediate sql -> (
      match sql with
      | [ Sql_ast.SqlVarToken (CobolVar (CobVarNotNull _)) ] -> ([], map)
      | _ ->
        let ws, map =
          let content =
            let content = Format.asprintf "%a" Sql_ast.Printer.pp_sql sql in
            if String.starts_with ~prefix:"'" content ||
               String.starts_with ~prefix:"\"" content
            then String.sub content 1 (String.length content - 2)
            else content
          in
          create_new_var content ()
        in
        (ws, map) )
    | Rollback (rb_work_or_tran, rb_args) -> begin
      match (rb_work_or_tran, rb_args) with
      | _, Some (To savepoint) ->
        let ws, map =
          create_new_var ("ROLLBACK TO SAVEPOINT " ^ savepoint.payload) ()
        in
        (ws, map)
      | _ -> ([], map)
    end
    | DeclareCursor cur -> begin
      match cur with
      | DeclareCursorSql (cur_name, query)
      | DeclareCursorWhithHold (cur_name, query) -> (*TODO: WhithHold specificity if there are any*)
        let ws, map =
          create_new_var
            (Format.asprintf "%a" Sql_ast.Printer.pp_sql_query query) ()
        in
        let ws, map = add_cur cur_name.payload map ws filename in

        (ws, map)
      | DeclareCursorVar (cur_name, var_name) ->
        let ws, map =
          create_new_var (Format.asprintf "\"@%a\"" Sql_ast.Printer.pp_var var_name) ~remplace:false ()
        in
        let ws, map = add_cur cur_name.payload map ws filename in

        (ws, map)
    end
    | Exeption _ ->
      create_new_var (Format.asprintf "%a" Sql_ast.Printer.pp_esql tokens) ()
    | _ -> ([], map)
  in

  let trans_declaration declaration =
    let SQL_type_is { importance; name; sql_type } =
      declaration in
    let vartype = get_type sql_type in
    let flags = get_flags sql_type in
    let field_length = (get_length sql_type) in
    let map = add_var
        ~length:field_length
        ~vartype
        ~flags
        map name
    in
    let field_length = string_of_int field_length in
    (match sql_type with
     | Binary _
     | Char _ ->
       [ Declaration
           (Simple_var_declaration
              { prefix;
                var_importance = importance;
                var_name = Some name;
                var_type = "X(" ^ field_length ^ ")";
                var_content = None
              } )
       ]
     | Float (digit, scale) ->
       let var_type =
         let scale = match scale with
         | None -> ""
         | Some scale -> Printf.sprintf "V9(%d)" scale
         in
         Printf.sprintf "S9(%d)%s" digit scale
       in
       [ Declaration
           (Simple_var_declaration
              { prefix;
                var_importance = importance;
                var_name = Some name;
                var_type;
                var_content = None
              } )
       ]
     | Varbinary _
     | Varchar _ ->
       let field =
         let prefix = prefix ^ "   " in
         [ Simple_var_declaration
             { prefix;
               var_importance = "49";
               var_name = Some (name ^ "-LEN");
               var_type = "9(8) COMP-5";
               var_content = None
             };
           Simple_var_declaration
             { prefix;
               var_importance = "49";
               var_name = Some (name ^ "-ARR");
               var_type = "X(" ^ field_length ^ ")";
               var_content = None
             }
         ]
       in
       let decl =
         Field_var_declaration
           { prefix; var_importance = importance; var_name = name; field }
       in [ Declaration decl ]) , map
  in

  match stm with
  | EXEC_SQL { tokens; _ } -> trans_sql tokens
  | DECLARATION { declaration; _ } -> trans_declaration declaration
  | _ -> ([], map)

let transform ~cobol_unit sql_statements filename =
  let rec transform_rec map sql_statements =
    match sql_statements with
    | h :: t ->
      let ws, map = transform_stm ~cobol_unit map h filename in
      let ws', map = transform_rec map t in
      (ws @ ws', map)
    | [] -> ([], map)
  in
  let init_map = StringMap.empty in
  transform_rec init_map sql_statements

let find_opt map str = StringMap.find_opt str map
