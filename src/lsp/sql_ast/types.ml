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
open Cobol_common

(**************************************************************************)
(*                              AST                                       *)
(**************************************************************************)
type sqlVarToken = string with_loc [@@deriving ord]

type cobolVarId = string with_loc [@@deriving ord]

type cobol_var =
  | CobVarNotNull of cobolVarId
  | CobVarCasted of cobolVarId * sql_type
  | CobVarNullIndicator of cobolVarId * cobolVarId
[@@deriving ord]

and variable =
  | SqlVar of sqlVarToken
  | CobolVar of cobol_var
[@@deriving ord]

and literal =
  | LiteralVar of variable
  | LiteralNum of string with_loc
  | LiteralStr of string with_loc
  | LiteralDot of string with_loc list
[@@deriving ord]

and sql_token =
  | SqlInstr of string
  | SqlVarToken of variable
  | SqlLit of literal
  | SqlQuery of sql_query
  | SqlEquality of sql_equal (*TODO: remove*)
  | SqlSearchCondition of search_condition (*TODO: remove*)

and sql_instruction = sql_token list

and complex_literal =
  | SqlCompLit of literal
  | SqlCompAsType of literal * sql_type_name (*ex: SMT AS INT*)
  | SqlCompAsVar of literal * sqlVarToken
  | SqlCompFun of sqlVarToken * sql_op list
  | SqlCompStar

and esql_instruction =
  | At of variable * esql_instruction
  | Sql of sql_instruction
  | Begin
  | BeginDeclare
  | Exeption of try_block
  | EndDeclare
  | StartTransaction
  | Whenever of whenever_condition * whenever_continuation
  | Include of sqlVarToken
  | Connect of connect_syntax
  | Rollback of rb_work_or_tran option * rb_args option
  | Commit of rb_work_or_tran option * bool
  | Savepoint of variable
  | ReleaseSavepoint of variable
  | SelectInto of
      { vars : cobol_var list;
        select : sql_select;
        select_options : sql_select_option list
      }
  | DeclareTable of literal * (sqlVarToken * sql_type) list
  | DeclareCursor of cursor
  | Prepare of sqlVarToken * sql_instruction
  | ExecuteImmediate of sql_instruction
  | ExecuteIntoUsing of
      { executed_string : sqlVarToken;
        opt_into_hostref_list : cobol_var list option;
        opt_using_hostref_list : cobol_var list option
      }
  | Disconnect of variable option (*db_id*)
  | DisconnectAll
  | Open of sqlVarToken * cobol_var list option (*cursor name*)
  | Close of sqlVarToken (*cursor name*)
  | Fetch of sqlVarToken * cobol_var list
  | Insert of table * value list
  | Delete of sql_instruction
  | Update of sqlVarToken * sql_update * update_arg option
  | Ignore of sql_instruction

and try_block =
  { try_instruction : esql_instruction;
    try_exceptions : sql_exception list
  }

and sql_exception = RaiseAndPrint of sqlVarToken * string with_loc * cobol_var

and cursor =
  | DeclareCursorSql of sqlVarToken * sql_query
  | DeclareCursorVar of sqlVarToken * variable
  | DeclareCursorWhithHold of sqlVarToken * sql_query

and table =
  | Table of sqlVarToken
  | TableLst of sqlVarToken * sqlVarToken list

and value =
  | ValueNull
  | ValueDefault
  | ValueList of literal list

and rb_work_or_tran =
  | Work
  | Transaction

and rb_args =
  | Release
  | To of sqlVarToken

and connect_syntax =
  | Connect_to_idby of
      { dbname : cobolVarId;
        db_conn_id : variable option;
        username : cobolVarId;
        db_data_source : cobolVarId;
        password : cobolVarId
      }
  | Connect_to of
      { db_data_source : cobolVarId;
        db_conn_id : variable option;
        username : cobolVarId;
        password : cobolVarId option
      }
  | Connect_using of { db_data_source : cobolVarId }
  | Connect_user of
      { username : cobolVarId;
        password : cobolVarId;
        db_conn_id : variable option;
        db_data_source : cobolVarId option
      }
  | Connect_reset of variable option

(*WHENEVER*)
and sql_type =
  { sql_type : sql_type_name;
    size : literal option;
    not_null : bool;
    with_default : bool
  }

and sql_type_name =
  | Char
  | Date
  | Integer
  | Timestamp
  | VarChar

and whenever_condition =
  | Not_found_whenever
  | SqlError_whenever
  | SqlWarning_whenever

and whenever_continuation =
  | Continue
  | Perform of sqlVarToken (*A label in cob program*)
  | Goto of sqlVarToken (*TODO doc*)

and update_arg =
  | WhereCurrentOf of sqlVarToken
  | WhereUpdate of search_condition
  | UpdateSql of sql_instruction

(*SQL*)
and sql_query =
  | SelectUnion of sql_query * sql_query
  | SelectExcept of sql_query * sql_query
  | SelectIntersect of sql_query * sql_query
  | SelectQuery of sql_select * sql_select_option list

and sql_select_option =
  | From of from_stm
  | Where of search_condition
  | OrderBy of sql_orderBy list
  | GroupBy of literal list
  | Having of search_condition

and from_stm = table_ref list

and table_ref =
  | FromLitAs of table_ref * literal
  | FromLit of literal
  | FromFun of sqlVarToken * literal
  | FromSelect of sql_query
  | Join of table_ref * join * table_ref * join_option option

and join =
  | InnerJoin
  | NaturalJoin
  | LeftJoin
  | RightJoin

and join_option =
  | JoinOn of search_condition
  | JoinUsing of sqlVarToken list

and sql_orderBy =
  | Asc of literal
  | Desc of literal

and sql_select = sql_op list

and sql_update = sql_equal list

and sql_equal = sqlVarToken * sql_op

and sql_op =
  | SqlOpLit of complex_literal
  | SqlOpBinop of (sql_binop * complex_literal * sql_op)

and sql_binop =
  | Add
  | Minus
  | Times
  | Or

and search_condition =
  | WhereConditionOr of search_condition * search_condition
  | WhereConditionAnd of search_condition * search_condition
  | WhereConditionNot of search_condition
  | WhereConditionCompare of sql_compare
  | WhereConditionIn of sql_condition_in
  | WhereConditionBetween of between_condition
  | WhereConditionIsNull of variable

and between_condition = Between of literal * literal * literal

and sql_condition_in = InVarLst of literal * complex_literal list

and sql_compare =
  | CompareQuery of complex_literal * comp_operator * sql_instruction
  | CompareLit of complex_literal * comp_operator * complex_literal

and comp_operator =
  | Less
  | Great
  | LessEq
  | GreatEq
  | EqualComp
  | Diff
[@@deriving ord]

(**************************************************************************)
(*                              COMPARE                                   *)
(**************************************************************************)
let compare = compare_esql_instruction

(**************************************************************************)
(*                              PRETTY PRINT                              *)
(**************************************************************************)
module Printer = struct
  let rec list_comma (fmt : Format.formatter)
      (g : 'a list * (Format.formatter -> 'a -> unit)) : unit =
    let x, f = g in
    match x with
    | [] -> Format.fprintf fmt ""
    | [ ele ] -> Format.fprintf fmt "%a" f ele
    | ele :: t -> Format.fprintf fmt "%a, %a" f ele list_comma (t, f)

  let rec pp fmt x = Format.fprintf fmt "EXEC SQL %a END-EXEC" pp_esql x

  and pp_esql fmt = function
    | At (v, instr) -> Format.fprintf fmt "AT %a %a" pp_var v pp_esql instr
    | Sql instr -> pp_sql fmt instr
    | Begin -> Format.fprintf fmt "BEGIN"
    | Exeption e -> Format.fprintf fmt "BEGIN %a END;" pp_exception e
    | BeginDeclare -> Format.fprintf fmt "BEGIN DECLARE SECTION"
    | EndDeclare -> Format.fprintf fmt "END DECLARE SECTION"
    | StartTransaction -> Format.fprintf fmt "START TRANSACTION"
    | Whenever (c, k) ->
      Format.fprintf fmt "WHENEVER %a %a" pp_whenever_condtion c
        pp_whenever_continuation k
    | Include i -> Format.fprintf fmt "INCLUDE %s" i.payload
    | Connect c -> Format.fprintf fmt "CONNECT %a" pp_connect c
    | Rollback (rb_work_or_tran, rb_args) ->
      Format.fprintf fmt "ROLLBACK %a %a" pp_some_rb_work_or_tran
        rb_work_or_tran pp_rb_args rb_args
    | Commit (rb_work_or_tran, b) ->
      let s =
        match b with
        | true -> "RELEASE"
        | false -> ""
      in
      Format.fprintf fmt "COMMIT %a %s" pp_some_rb_work_or_tran rb_work_or_tran
        s
    | Savepoint s -> Format.fprintf fmt "SAVEPOINT %a" pp_var s
    | ReleaseSavepoint s -> Format.fprintf fmt "RELEASE SAVEPOINT %a" pp_var s
    | SelectInto { vars; select; select_options } ->
      Format.fprintf fmt "SELECT %a INTO %a %a" pp_select_lst select pp_cob_lst
        vars pp_select_options_lst select_options
    | DeclareTable (var, sql) ->
      Format.fprintf fmt "DECLARE %a TABLE (%a)" pp_lit var pp_declare sql
    | DeclareCursor cursor -> pp_cursor fmt cursor
    | Prepare (str, sql) ->
      Format.fprintf fmt "PREPARE %s FROM %a" str.payload pp_sql sql
    | ExecuteImmediate sql ->
      Format.fprintf fmt "EXECUTE IMMEDIATE %a" pp_sql sql
    | ExecuteIntoUsing
        { executed_string; opt_into_hostref_list; opt_using_hostref_list } ->
      Format.fprintf fmt "EXECUTE %s %a %a" executed_string.payload
        pp_some_cob_lst
        (opt_into_hostref_list, "INTO")
        pp_some_cob_lst
        (opt_using_hostref_list, "USING")
    | Disconnect sdbname ->
      Format.fprintf fmt "DISCONNECT %a" pp_some_var (sdbname, "")
    | DisconnectAll -> Format.fprintf fmt "DISCONNECT ALL"
    | Open (cursor, lst) ->
      Format.fprintf fmt "OPEN %s %a" cursor.payload pp_some_cob_lst
        (lst, "USING")
    | Close cursor -> Format.fprintf fmt "CLOSE %s" cursor.payload
    | Fetch (sql, var) ->
      Format.fprintf fmt "FETCH %s INTO %a" sql.payload pp_cob_lst var
    | Insert (tab, v) ->
      Format.fprintf fmt "INSERT INTO %a VALUES %a" pp_table tab pp_value v
    | Delete sql -> Format.fprintf fmt "DELETE %a" pp_sql sql
    | Update (table, equallst, swhere) ->
      Format.fprintf fmt "UPDATE %s SET %a %a" table.payload pp_sql_update
        equallst pp_where_arg swhere
    | Ignore lst -> Format.fprintf fmt "IGNORE %a" pp_sql lst

  and pp_exception fmt e =
    Format.fprintf fmt "%a; EXCEPTION %a" pp_esql e.try_instruction
      pp_exception_list e.try_exceptions

  and pp_exception_list fmt l =
    let pp_one_exception fmt = function
      | RaiseAndPrint (name, str, cob_var) ->
        Format.fprintf fmt "WHEN %s THEN RAISE EXCEPTION %s, %a" name.payload
          str.payload pp_cob_var cob_var
    in
    List.iter (Format.fprintf fmt " %a; " pp_one_exception) l

  and pp_cursor fmt = function
    | DeclareCursorSql (var, sql) ->
      Format.fprintf fmt "DECLARE %s CURSOR FOR %a" var.payload pp_sql_query sql
    | DeclareCursorVar (var, v) ->
      Format.fprintf fmt "DECLARE %s CURSOR FOR %a" var.payload pp_var v
    | DeclareCursorWhithHold (var, sql) ->
      Format.fprintf fmt "DECLARE %s CURSOR WITH HOLD FOR %a" var.payload
        pp_sql_query sql

  and pp_table fmt = function
    | Table t -> Format.fprintf fmt "%s" t.payload
    | TableLst (t, lst) ->
      let f = pp_sqlVarToken in
      let pp_aux fmt lst = list_comma fmt (lst, f) in
      Format.fprintf fmt "%s(%a)" t.payload pp_aux lst

  and pp_sqlVarToken fmt x = Format.fprintf fmt "%s" x.payload

  and pp_value fmt x = list_comma fmt (x, pp_one_value)

  and pp_declare fmt x = list_comma fmt (x, pp_var_type)

  and pp_var_type fmt (l, t) =
    Format.fprintf fmt "%s\t %a" l.payload pp_sql_type t

  and pp_sql_type_name fmt test =
    match test with
    | Char -> Format.fprintf fmt "CHAR"
    | Date -> Format.fprintf fmt "DATE"
    | Integer -> Format.fprintf fmt "INTEGER"
    | Timestamp -> Format.fprintf fmt "TIMESTAMP"
    | VarChar -> Format.fprintf fmt "VARCHAR"
  and pp_sql_type fmt { sql_type; size; not_null; with_default } =

    pp_sql_type_name fmt sql_type;
    ( match size with
    | Some lit -> Format.fprintf fmt " (%a)" pp_lit lit
    | None -> () );
    if not_null then Format.fprintf fmt " NOT NULL";
    if with_default then Format.fprintf fmt " WITH DEFAULT"

  and pp_one_value fmt = function
    | ValueDefault -> Format.fprintf fmt "DEFAULT"
    | ValueNull -> Format.fprintf fmt "NULL"
    | ValueList l -> (
      match l with
      | [ x ] -> Format.fprintf fmt "(%a)" pp_lit x
      | [] -> Format.fprintf fmt ""
      | _ -> Format.fprintf fmt "(%a)" pp_list_lit l )

  and pp_where_arg fmt = function
    | Some (WhereCurrentOf swhere) ->
      Format.fprintf fmt "WHERE CURRENT OF %s" swhere.payload
    | Some (WhereUpdate e) -> Format.fprintf fmt "WHERE %a" pp_sql_condition e
    | Some (UpdateSql sql) -> pp_sql fmt sql
    | None -> ()

  and pp_sql_update_aux fmt (var, op) =
    Format.fprintf fmt "%s = %a" var.payload pp_sql_op op

  and pp_sql_update fmt x = list_comma fmt (x, pp_sql_update_aux)

  and pp_sql_op fmt = function
    | SqlOpBinop (op, sql1, sql2) ->
      Format.fprintf fmt "%a %s %a" pp_complex_literal sql1 (pp_binop op)
        pp_sql_op sql2
    | SqlOpLit l -> Format.fprintf fmt "%a" pp_complex_literal l

  and pp_sql_some_condition fmt = function
    | Some s -> Format.fprintf fmt "WHERE %a" pp_sql_condition s
    | None -> Format.fprintf fmt ""

  and pp_sql_condition fmt = function
    | WhereConditionAnd (s1, s2) ->
      Format.fprintf fmt "%a AND %a" pp_sql_condition s1 pp_sql_condition s2
    | WhereConditionOr (s1, s2) ->
      Format.fprintf fmt "%a OR %a" pp_sql_condition s1 pp_sql_condition s2
    | WhereConditionNot s -> Format.fprintf fmt "NOT %a" pp_sql_condition s
    | WhereConditionCompare c ->
      let rec pp_compare fmt = function
        | CompareLit (l1, c, l2) ->
          Format.fprintf fmt "%a %s %a" pp_complex_literal l1
            (comp_op_to_string c) pp_complex_literal l2
        | CompareQuery (l1, c, s) ->
          Format.fprintf fmt "%a %s (%a)" pp_complex_literal l1
            (comp_op_to_string c) pp_sql s
      and comp_op_to_string = function
        | Less -> "<"
        | Great -> ">"
        | LessEq -> "<="
        | GreatEq -> ">="
        | EqualComp -> "="
        | Diff -> "<>"
      in
      Format.fprintf fmt "%a" pp_compare c
    | WhereConditionIn s -> Format.fprintf fmt "%a" pp_condition_in s
    | WhereConditionBetween s -> Format.fprintf fmt "%a" pp_condition_between s
    | WhereConditionIsNull v -> Format.fprintf fmt "%a IS NULL" pp_var v

  and pp_condition_in fmt x =
    let pp_aux fmt lst = list_comma fmt (lst, pp_complex_literal) in
    match x with
    | InVarLst (l, vlist) ->
      Format.fprintf fmt "%a IN (%a)" pp_lit l pp_aux vlist

  and pp_condition_between fmt = function
    | Between (l, l1, l2) ->
      Format.fprintf fmt "%a BETWEEN %a AND %a" pp_lit l pp_lit l1 pp_lit l2

  and pp_complex_literal fmt = function
    | SqlCompLit v -> Format.fprintf fmt "%a" pp_lit v
    | SqlCompAsType (l, v) -> Format.fprintf fmt "%a AS %a" pp_lit l pp_sql_type_name v
    | SqlCompAsVar (l, v) -> Format.fprintf fmt "%a AS %s" pp_lit l v.payload
    | SqlCompFun (funName, args) ->
      let pp_args fmt lst = list_comma fmt (lst, pp_sql_op) in
      Format.fprintf fmt "%s(%a)" funName.payload pp_args args
    | SqlCompStar -> Format.fprintf fmt "*"

  and pp_binop = function
    | Add -> "+"
    | Minus -> "-"
    | Times -> "*"
    | Or -> "||"

  and pp_some_cob_lst fmt = function
    | Some x, s -> Format.fprintf fmt "%s %a" s pp_cob_lst x
    | None, _ -> Format.fprintf fmt ""

  and pp_cob_lst fmt x = list_comma fmt (x, pp_cob_var)

  and pp_cob_var fmt = function
    | CobVarNotNull c -> Format.fprintf fmt ":%s" c.payload
    | CobVarCasted (c, t) ->
      Format.fprintf fmt ":%s::%a" c.payload pp_sql_type t
    | CobVarNullIndicator (c, ni) ->
      Format.fprintf fmt ":%s:%s" c.payload ni.payload

  and pp_cob_var_id fmt c = Format.fprintf fmt ":%s" c.payload

  and pp_some_rb_work_or_tran fmt = function
    | Some p -> pp_rb_work_or_tran fmt p
    | None -> Format.fprintf fmt ""

  and pp_rb_work_or_tran fmt = function
    | Work -> Format.fprintf fmt "WORK"
    | Transaction -> Format.fprintf fmt "TRANSACTION"

  and pp_rb_args fmt = function
    | Some Release -> Format.fprintf fmt "RELEASE"
    | Some (To variable) ->
      Format.fprintf fmt "TO SAVEPOINT %s" variable.payload
    | None -> Format.fprintf fmt ""

  and pp_some_cob_var fmt (x, s) =
    match x with
    | Some v -> Format.fprintf fmt "%s %a" s pp_cob_var_id v
    | None -> Format.fprintf fmt ""

  and pp_connect fmt c =
    match c with
    | Connect_to_idby { dbname; db_conn_id; username; db_data_source; password }
      ->
      Format.fprintf fmt "TO %a %a USER %a USING %a IDENTIFIED BY %a"
        pp_cob_var_id dbname pp_some_var (db_conn_id, "AS") pp_cob_var_id
        username pp_cob_var_id db_data_source pp_cob_var_id password
    | Connect_to { db_data_source; db_conn_id; username; password } ->
      Format.fprintf fmt "TO %a %a USER %a %a" pp_cob_var_id db_data_source
        pp_some_var (db_conn_id, "AS") pp_cob_var_id username pp_some_cob_var
        (password, "USING")
    | Connect_using { db_data_source } ->
      Format.fprintf fmt "USING %a" pp_cob_var_id db_data_source
    | Connect_user { username; password; db_conn_id; db_data_source } ->
      Format.fprintf fmt "%a IDENTIFIED BY %a %a %a" pp_cob_var_id username
        pp_cob_var_id password pp_some_var (db_conn_id, "AT") pp_some_cob_var
        (db_data_source, "USING")
    | Connect_reset name -> Format.fprintf fmt "RESET%a" pp_some_var (name, "")

  and pp_whenever_condtion fmt = function
    | Not_found_whenever -> Format.fprintf fmt "NOT FOUND"
    | SqlError_whenever -> Format.fprintf fmt "SQLERROR"
    | SqlWarning_whenever -> Format.fprintf fmt "SQLWARNING"

  and pp_whenever_continuation fmt = function
    | Continue -> Format.fprintf fmt "CONTINUE"
    | Perform label -> Format.fprintf fmt "PERFORM %s" label.payload
    | Goto stmt_label -> Format.fprintf fmt "GOTO %s" stmt_label.payload

  and pp_some_sql fmt = function
    | Some p -> pp_sql fmt p
    | None -> Format.fprintf fmt ""

  and pp_sql fmt = function
    | [ h ] -> Format.fprintf fmt "%a" pp_one_token h
    | h :: t -> Format.fprintf fmt "%a %a" pp_one_token h pp_sql t
    | [] -> Format.fprintf fmt ""

  and pp_one_token fmt = function
    | SqlInstr s -> Format.fprintf fmt "%s" s
    | SqlVarToken c -> Format.fprintf fmt "%a" pp_var c
    | SqlLit l -> Format.fprintf fmt "%a" pp_lit l
    | SqlQuery s -> Format.fprintf fmt "%a" pp_sql_query s
    | SqlEquality e -> Format.fprintf fmt "%a" pp_sql_update_aux e
    | SqlSearchCondition c -> Format.fprintf fmt "%a" pp_sql_condition c

  and pp_sql_query fmt = function
    | SelectQuery (s, o) ->
      Format.fprintf fmt "SELECT %a %a" pp_select_lst s pp_select_options_lst o
    | SelectUnion (s1, s2) ->
      Format.fprintf fmt "%a UNION %a" pp_sql_query s1 pp_sql_query s2
    | SelectExcept (s1, s2) ->
      Format.fprintf fmt "%a EXCEPT %a" pp_sql_query s1 pp_sql_query s2
    | SelectIntersect (s1, s2) ->
      Format.fprintf fmt "%a INTERSECT %a" pp_sql_query s1 pp_sql_query s2

  and pp_select_options_lst fmt lst =
    let pp_one_option fmt = function
      | From f -> Format.fprintf fmt "FROM %a" pp_from f
      | Where w -> Format.fprintf fmt "WHERE %a" pp_sql_condition w
      | OrderBy ob -> Format.fprintf fmt "ORDER BY %a" pp_orderBy ob
      | GroupBy gb -> Format.fprintf fmt "GROUP BY %a" pp_group_by gb
      | Having w -> Format.fprintf fmt "HAVING %a" pp_sql_condition w
    in
    List.iter (Format.fprintf fmt " %a" pp_one_option) lst

  and pp_from fmt f = list_comma fmt (f, pp_table_ref)

  and pp_table_ref fmt = function
    | FromLit l -> Format.fprintf fmt "%a" pp_lit l
    | FromLitAs (l, a) -> Format.fprintf fmt "%a AS %a" pp_table_ref l pp_lit a
    | FromFun (v, t) -> Format.fprintf fmt "%a %a" pp_sqlVarToken v pp_lit t
    | FromSelect s -> Format.fprintf fmt "(%a)" pp_sql_query s
    | Join (tr1, join, tr2, opt) ->
      Format.fprintf fmt "%a %s JOIN %a %a" pp_table_ref tr1 (str_join join)
        pp_table_ref tr2 pp_table_opt_option opt

  and str_join = function
    | InnerJoin -> "INNER"
    | NaturalJoin -> "NATURAL"
    | LeftJoin -> "LEFT"
    | RightJoin -> "RIGHT"

  and pp_table_opt_option fmt = function
    | Some w -> pp_table_opt fmt w
    | None -> Format.fprintf fmt ""

  and pp_table_opt fmt = function
    | JoinOn sc -> Format.fprintf fmt "ON %a" pp_sql_condition sc
    | JoinUsing lstvar ->
      let pp_aux fmt x = list_comma fmt (x, pp_sqlVarToken) in
      Format.fprintf fmt "USING %a" pp_aux lstvar

  and pp_group_by fmt x = Format.fprintf fmt "%a" pp_list_lit x

  and pp_orderBy fmt x =
    let pp_aux fmt = function
      | Asc v -> Format.fprintf fmt "%a ASC" pp_lit v
      | Desc v -> Format.fprintf fmt "%a DESC" pp_lit v
    in
    list_comma fmt (x, pp_aux)

  and pp_select_lst fmt l = list_comma fmt (l, pp_sql_op)

  and pp_some_var fmt (x, s) =
    match x with
    | Some v -> Format.fprintf fmt "%s %a" s pp_var v
    | None -> Format.fprintf fmt ""

  and pp_var fmt = function
    | SqlVar v -> Format.fprintf fmt "%s" v.payload
    | CobolVar c -> pp_cob_var fmt c

  and pp_list_lit fmt x = list_comma fmt (x, pp_lit)

  and pp_lit fmt = function
    | LiteralNum n -> Format.fprintf fmt "%s" n.payload
    | LiteralStr n -> Format.fprintf fmt "%s" n.payload
    | LiteralVar n -> Format.fprintf fmt "%a" pp_var n
    | LiteralDot lst ->
      let rec pp_aux fmt = function
        | [] -> Format.fprintf fmt ""
        | [ ele ] -> Format.fprintf fmt "%s" ele.payload
        | ele :: t -> Format.fprintf fmt "%s.%a" ele.payload pp_aux t
      in
      pp_aux fmt lst
end
