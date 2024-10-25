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
open Cobol_indent.Types
open Types

let loc_of_edit ~filename e =
  { filename;
    line = e.tok_edit.edit.lnum;
    char = e.tok_indent + e.tok_edit.edit.offset_orig
  }

let error ?loc fmt =
  Printf.kprintf
    (fun s ->
      Printf.eprintf "Error";
      begin
        match loc with
        | None -> ()
        | Some loc -> Printf.eprintf " at %s:%d" loc.filename loc.line
      end;
      Printf.eprintf ": %s\n%!" s;
      Printf.eprintf "Aborting.\n%!";
      exit 2 )
    fmt

let warning ?loc fmt =
  Printf.kprintf
    (fun s ->
      Printf.eprintf "Warning";
      begin
        match loc with
        | None -> ()
        | Some loc -> Printf.eprintf " at %s:%d" loc.filename loc.line
      end;
      Printf.eprintf ": %s\n%!" s )
    fmt

let string_of_token = function
  | IDENT tok -> tok
  | CHARS tok -> tok
  | INTEGER tok -> tok
  | NUMBER tok -> tok
  | DOT -> "."
  | LPAREN -> "("
  | RPAREN -> ")"
  | EQUALEQUAL -> "=="
  | COMMA -> ","
  | SEMI -> ";"
  | COLON -> ":"
  | EQUAL -> "="
  | MINUS -> "-"
  | PLUS -> "+"
  | GT -> ">"
  | LT -> "<"
  | GTE -> ">="
  | LTE -> "<="
  | DIV -> "/"
  | MUL -> "*"
  | DOLLAR -> "$"
  | AMPER -> "&"
  | SHARP -> "#"
  | tok -> (
    try Hashtbl.find Cobol_indent.Lexer.keyword2string tok with
    | Not_found -> failwith (Cobol_indent.Misc.string_of_token tok) )

let add_dot ~with_dot b = if with_dot then Printf.bprintf b "            .\n"

let resolve_copy ~config file =
  let rec iter_exts exts =
    match exts with
    | [] -> raise Not_found
    | ext :: exts -> (
      let file = String.lowercase_ascii (file ^ ext) in
      match iter_paths file (Lazy.force config.copy_path) with
      | exception Not_found -> iter_exts exts
      | filename -> filename )
  and iter_paths file path =
    match path with
    | [] -> raise Not_found
    | (dir, map) :: path -> (
      match StringMap.find file map with
      | exception Not_found -> iter_paths file path
      | file -> Filename.concat dir file )
  in
  iter_exts config.copy_exts

let cob_var_extractor_folder = object
  inherit [Sql_ast.cobol_var list] Sql_ast.Visitor.folder
  method! fold_cobol_var cob_var acc =
    if List.exists (fun c -> Sql_ast.compare_cobol_var cob_var c == 0) acc
    then Cobol_common.Visitor.skip acc
    else Cobol_common.Visitor.skip (cob_var::acc)
end

let extract_filename path =
  let parts = Str.split (Str.regexp "/") path in
  let filename_with_ext = List.hd (List.rev parts) in
  let filename_parts = Str.split (Str.regexp "\\.") filename_with_ext in
  List.hd filename_parts


let rec build_args_string starting_arg_nb arg_amount =
  if arg_amount < 2
  then "$" ^ string_of_int starting_arg_nb
  else
    "$" ^ string_of_int starting_arg_nb ^ ","
    ^ build_args_string (starting_arg_nb+1) (arg_amount-1)

let replace_colon_words ~cobol_unit str =
  (* Ensure correct behaviour for several :COM:NULL-IND *)
  let buffer = Buffer.create (String.length str) in
  let count = ref 1 in
  let len = String.length str in
  let i = ref 0 in
  let tbl = Hashtbl.create 10 in

  while !i < len do
    if str.[!i] = ':' then (
      let start = !i + 1 in
      incr i;
      while
        !i < len
        && str.[!i] <> ' '
        && str.[!i] <> ','
        && str.[!i] <> ')'
        && str.[!i] <> '('
      do
        incr i
      done;
      let var_name = String.sub str start (!i - start) in
      match Hashtbl.find_opt tbl var_name with
      | Some s ->
        Buffer.add_string buffer s
      | None ->
        let arg_amount = List.length @@
          Sql_typeck.get_elementary_component cobol_unit var_name
        in
        let s = build_args_string !count arg_amount in
        count := !count + arg_amount;
        Hashtbl.add tbl var_name s;
        Buffer.add_string buffer s)
    else (
      Buffer.add_char buffer str.[!i];
      incr i)
  done;

  Buffer.contents buffer
