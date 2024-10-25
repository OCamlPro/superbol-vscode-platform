(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2022-2023 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

{
  open Grammar

  (* keyword table, In alphabetical order *)
    let kwd_table = Hashtbl.create 87
    let _ =
    List.iter (fun (a,b) -> Hashtbl.add kwd_table a b)
        [
        "ALL", ALL;
        "AND", AND;
        "AS", AS;
        "ASC", ASC;
        "AT", AT;
        "BEGIN", BEGIN;
        "BETWEEN", BETWEEN;
        "BY", BY;
        "CHAR", CHAR;
        "CLOSE", CLOSE;
        "COMMIT", COMMIT;
        "CONNECT", CONNECT;
        "CURRENT", CURRENT;
        "CURSOR", CURSOR;
        "DATE", DATE;
        "DECLARE", DECLARE;
        "DEFAULT", DEFAULT;
        "DELETE", DELETE;
        "DESC", DESC;
        "DISCONNECT", DISCONNECT;
        "END", END;
        "EXCEPT", EXCEPT;
        "EXEC", EXEC;
        "EXECUTE", EXECUTE;
        "EXCEPTION", EXCEPTION;
        "FETCH", FETCH;
        "FOR", FOR;
        "FOUND", FOUND;
        "FROM", FROM;
        "GOTO", GOTO;
        "GO", GO;
        "GROUP", GROUP;
        "HAVING", HAVING;
        "HOLD", HOLD;
        "IDENTIFIED", IDENTIFIED;
        "IGNORE", IGNORE;
        "INNER", INNER;
        "IMMEDIATE", IMMEDIATE;
        "IN", IN;
        "INCLUDE", INCLUDE;
        "INSERT", INSERT;
        "INTEGER", INTEGER;
        "INTERSECT", INTERSECT;
        "INTO", INTO;
        "IS", IS;
        "JOIN", JOIN;
        "LEFT", LEFT;
        "NATURAL", NATURAL;
        "NOT", NOT;
        "NULL", NULL;
        "OF", OF;
        "ON", ON;
        "OR", OR;
        "ORDER", ORDER;
        "OPEN", OPEN;
        "OUTER", OUTER;
        "PERFORM", PERFORM;
        "PREPARE", PREPARE;
        "RAISE", RAISE;
        "RELEASE", RELEASE;
        "RESET", RESET;
        "RIGHT", RIGHT;
        "ROLLBACK", ROLLBACK;
        "SAVEPOINT", SAVEPOINT;
        "SECTION", SECTION;
        "SELECT", SELECT;
        "SET", SET;
        "START", START;
        "STATEMENT", STATEMENT;
        "SQL", SQL;
        "SQLERROR", SQLERROR;
        "SQLWARNING", SQLWARNING;
        "TABLE", TABLE;
        "THEN", THEN;
        "TIMESTAMP", TIMESTAMP;
        "TO", TO;
        "TRAN", TRANSACTION;
        "TRANSACTION", TRANSACTION;
        "UPDATE", UPDATE;
        "UNION", UNION;
        "USER", USER;
        "USING", USING;
        "VALUES", VALUES;
        "VARCHAR", VARCHAR;
        "WHEN", WHEN;
        "WHERE", WHERE;
        "WHENEVER", WHENEVER;
        "WITH", WITH;
        "WORK", WORK;
    ]

    let get_keyword s =
      try Hashtbl.find kwd_table (String.uppercase_ascii s)
      with Not_found -> WORD s
}

let number = '-'? ['0'-'9']+ (','['0'-'9']+)?

rule token = parse
  | "END-EXEC"
      { END_EXEC }
  | "end-exec"
      { END_EXEC }
  | "END-EXEC."
      { END_EXEC }
  | ':' (['a'-'z' 'A'-'Z']['A'-'Z' 'a'-'z' '0'-'9' '_' '-']* as s)
      { COBOL_VAR s }
  | '\\' (['a'-'z' 'A'-'Z']['A'-'Z' 'a'-'z' '0'-'9' '_']* as s)
      { BACKSLASH_VAR s }
  | ['a'-'z' 'A'-'Z' '0'-'9']['A'-'Z' 'a'-'z' '0'-'9' '-' '_' '*' ]* as s
      { get_keyword s }
  | number as n
      { NUMBER n }
  | '\'' [^ '\'']* '\'' as s
      { STRING s}
  | '\"' [^ '\"']* '\"' as s
      { STRING s}

  | "||"
      { OR }
  | '='
      { EQUAL }
  | '+'
      { PLUS }
  | '-'
      { MINUS }
  | "<="
      { LESS_EQ }
  | ">="
       { GREAT_EQ }
  | "<"
        { LESS }
  | ">"
        { GREAT }
  | ":"
        { COLON }
  | ','
      { COMMA }
  | '.'
      { DOT }
  | '('
      { LPAR }
  | ')'
      { RPAR }
  | '*'
      { STAR }
  | ';'
      { SEMICOLON }
  | ' '
      { token lexbuf }
  | _ as c
      { failwith (Printf.sprintf "unexpected character: %C" c) }
(*   | _ as s
      { TOKEN s } *)
  | eof
      { EOF }

{
  let token lexbuf =
    token lexbuf
}
