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
