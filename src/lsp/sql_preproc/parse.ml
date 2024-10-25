(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2021-2023 OCamlPro SAS                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the                       *)
(*  OCAMLPRO-NON-COMMERCIAL license.                                      *)
(*                                                                        *)
(**************************************************************************)

open Ez_file.V1
open Cobol_indent.Types
open Types

let rec find_dot tokens =
  match tokens with
  | [] -> Misc.error "Found end of file while looking for ending dot"
  | (DOT, e) :: tokens -> (e, tokens)
  | _ :: tokens -> find_dot tokens

let parse ~config ~filename ~contents =
  let program_id = ref None in
  let sql_statements = ref [] in
  let procedure_division_found = ref None in
  let working_storage_found = ref false in
  let linkage_section_found = ref false in

  let sql_add_statement ?loc tokens =
    sql_statements := (loc, tokens) :: !sql_statements
  in

  let rec iter tokens =
    match tokens with
    | [] -> ()
    | (PROGRAM_ID, loc) :: (DOT, _) :: (IDENT name, _) :: tokens ->
      begin
        match !program_id with
        | None -> program_id := Some name
        | Some _ ->
          Misc.error ~loc "multiple programs in the same file are not supported"
      end;
      iter tokens
    (*Exemple : 01 VCFLD SQL TYPE IS VARCHAR(100).*)
    | (INTEGER importance, loc)
      :: (IDENT name, _)
      :: (IDENT "SQL", _)
      :: (IDENT "TYPE", _)
      :: (IDENT "IS", _)
      :: (IDENT typ, _)
      :: (LPAREN, _)
      :: (INTEGER size, _)
      :: (RPAREN, end_loc)
      :: tokens ->
      let size = int_of_string size in
      let sql_type = match typ with
        | "BINARY" -> Binary size
        | "VARBINARY" -> Varbinary size
        | "VARCHAR" -> Varchar size
        | "CHAR" -> Char size
        | "FLOAT" -> Float (size, None)
        | unknown -> Pretty.failwith "Unknow type %s" unknown
      in
      let declaration =
        SQL_type_is { importance; name; sql_type }
      in
      sql_add_statement ~loc (DECLARATION { end_loc; declaration });
      iter tokens
    (*Exemple : 01 VCFLD SQL TYPE IS FLOAT(4,2).*)
    | (INTEGER importance, loc)
      :: (IDENT name, _)
      :: (IDENT "SQL", _) :: (IDENT "TYPE", _) :: (IDENT "IS", _)
      :: (IDENT "FLOAT", _) :: (LPAREN, _)
      :: (NUMBER digits_n_scale, _) :: (RPAREN, end_loc) :: tokens ->
      let digits, scale =
        match String.split_on_char ',' digits_n_scale with
        | [digits; scale] -> int_of_string digits, int_of_string scale
        | _ ->
          Pretty.failwith
          "ERROR: invalid argument for FLOAT sql type : FLOAT(%s)" digits_n_scale
      in
      let declaration =
        SQL_type_is { importance; name; sql_type=Float (digits, Some scale) }
      in
      sql_add_statement ~loc (DECLARATION { end_loc; declaration });
      iter tokens
    | (PROCEDURE, loc) :: (DIVISION, _) :: tokens ->
      let end_loc, tokens = find_dot tokens in
      if not !working_storage_found then
        sql_add_statement ~loc (WORKING_STORAGE { defined = false });
      if not !linkage_section_found then
        sql_add_statement ~loc (LINKAGE_SECTION { defined = false });
      sql_add_statement ~loc (PROCEDURE_DIVISION_DOT { end_loc });
      assert (!procedure_division_found = None);
      let ok = ref true in
      sql_add_statement
        ~loc:{ loc with line = loc.line + 1 }
        (BEGIN_PROCEDURE_DIVISION { enabled = ok });

      procedure_division_found := Some ok;
      linkage_section_found := false;
      working_storage_found := false;
      iter tokens
    | (IDENTIFICATION, loc) :: (DIVISION, _) :: tokens ->
      begin
        match !procedure_division_found with
        | None -> ()
        | Some _ ->
          sql_add_statement ~loc END_PROCEDURE_DIVISION;
          procedure_division_found := None
      end;
      linkage_section_found := false;
      working_storage_found := false;
      iter tokens
    | (END, loc) :: (PROGRAM, _) :: tokens ->
      begin
        match !procedure_division_found with
        | None -> ()
        | Some _ ->
          sql_add_statement ~loc END_PROCEDURE_DIVISION;
          procedure_division_found := None
      end;
      iter tokens
    | (END, loc) :: (DECLARATIVES, _) :: (DOT, _) :: tokens ->
      begin
        match !procedure_division_found with
        | None -> ()
        | Some enabled ->
          (* we disable the previous location where we found PROCEDURE
             DIVISON, because we don't want to insert code before the
             DECLARATIVES, but always after *)
          enabled := false;
          let enabled = ref true in
          procedure_division_found := Some enabled;
          sql_add_statement
            ~loc:{ loc with line = loc.line + 1 }
            (BEGIN_PROCEDURE_DIVISION { enabled });
          procedure_division_found := None
      end;
      iter tokens
    | (WORKING_STORAGE, _loc) :: (SECTION, _) :: (DOT, loc) :: tokens ->
      working_storage_found := true;
      sql_add_statement ~loc (WORKING_STORAGE { defined = true });
      iter tokens
    | (LINKAGE, _loc) :: (SECTION, _) :: (DOT, loc) :: tokens ->
      if not !working_storage_found then begin
        sql_add_statement ~loc (WORKING_STORAGE { defined = false });
        working_storage_found := true
      end;
      linkage_section_found := true;
      if config.verbosity > 0 then
        Format.fprintf Format.std_formatter "LINKAGE SECTION found at %d\n%!" loc.line;
      sql_add_statement ~loc (LINKAGE_SECTION { defined = true });
      iter tokens
    | (COPY, loc) :: (tok, _) :: (DOT, end_loc) :: tokens
    | (EXEC, loc) :: (IDENT "SQL", _) :: (IDENT "INCLUDE", _) :: (tok, _) :: (END_EXEC, end_loc)  :: tokens
    when config.sql_in_copybooks && (Misc.string_of_token tok != "SQLCA")->
      let file = Misc.string_of_token tok in
      begin
        match Misc.resolve_copy ~config file with
        | exception Not_found ->
          Misc.warning ~loc "Could not locate copybook %S" file;
          iter tokens
        | filename ->
          let contents = EzFile.read_file filename in
          sql_add_statement ~loc (COPY { end_loc; filename; contents });
          tokenize_file ~filename ~contents tokens
      end
    | (EXEC, loc) :: (IDENT "SQL", _) :: (IDENT "IGNORE", loc2) :: tokens ->
      if config.verbosity > 0 then
        Printf.eprintf "EXEC SQL IGNORE found at line %d\n%!" loc.line;
        iter_ignored_sql loc { loc2 with char = loc2.char + 6 } tokens
    | (EXEC, loc) :: (IDENT "SQL", _) :: tokens ->
      if config.verbosity > 0 then
        Printf.eprintf "EXEC SQL found at line %d\n%!" loc.line;
      begin
        match tokens with
        | (_, _loc) :: _ -> iter_sql loc [] tokens
        | [] -> Misc.error ~loc "SQL syntax error on end of file"
      end
    | _ :: tokens -> iter tokens
  and iter_sql loc params tokens =
    match tokens with
    | (END_EXEC, end_loc) :: tokens ->
      let end_loc, with_dot, tokens =
        match tokens with
        | (DOT, end_loc) :: tokens -> (end_loc, true, tokens)
        | tokens -> (end_loc, false, tokens)
      in
      if config.verbosity > 0 then
        Printf.eprintf "END-EXEC found at %d\n%!" end_loc.line;

      let params = List.rev params in
      let sqlStr = "EXEC SQL " ^ String.concat " " params ^ " END-EXEC" in
      let sql = Sql_parser.parseString (Lexing.from_string sqlStr) in
      sql_add_statement ~loc (EXEC_SQL { end_loc; with_dot; tokens = sql });
      iter tokens
    | [] -> failwith "missing END-EXEC."
    | (tok, _) :: tokens ->
      let tok = Misc.string_of_token tok in
      iter_sql loc (tok :: params) tokens
  and iter_ignored_sql loc begin_of_ignore_loc tokens =
    match tokens with
    | (END_EXEC, end_loc) :: tokens ->
      if config.verbosity > 0 then
        Printf.eprintf "END-EXEC found at %d\n%!" end_loc.line;
      sql_add_statement ~loc (EXEC_SQL_IGNORE { end_loc; begin_of_ignore_loc });
      iter tokens
    | _ :: tokens -> iter_ignored_sql loc begin_of_ignore_loc tokens
    | [] -> failwith "missing END-EXEC."
  and tokenize_file ~filename ~contents tokens =
    let { Cobol_indent.Scanner.toks = new_tokens; _ } =
      Cobol_indent.Scanner.tokenize ~filename ~config:config.scanner_config
        ~contents
    in

    let tokens =
      List.rev_append
        (List.rev_map
           (fun (tok, e) -> (tok, Misc.loc_of_edit ~filename e))
           new_tokens )
        tokens
    in

    iter tokens
  in

  tokenize_file ~filename ~contents [];

  (* Only fail if no PROCEDURE DIVISION was found for a main program, not for a copybook...

     if not !procedure_division_found then
     error "PROCEDURE DIVISION. not found";
  *)
  begin
    match !procedure_division_found with
    | None -> ()
    | Some _ -> sql_add_statement END_PROCEDURE_DIVISION
  end;

  List.rev !sql_statements
