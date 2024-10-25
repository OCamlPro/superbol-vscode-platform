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

open Types
open Cobol_common.Visitor
open Cobol_common.Visitor.INFIX                         (* for `>>` (== `|>`) *)

(* --- *)

class ['a] folder = object
  inherit ['a] Fold.folder
  method fold_cobol_var_id: (cobolVarId, 'a) fold = default
  method fold_sql_var_token: (sqlVarToken, 'a) fold = default
  method fold_cobol_var: (cobol_var, 'a) fold = default
  method fold_variable: (variable, 'a) fold = default
  method fold_literal: (literal, 'a) fold = default
  method fold_sql_token: (sql_token, 'a) fold = default
  method fold_sql_instruction: (sql_instruction, 'a) fold = default
  method fold_complex_literal: (complex_literal, 'a) fold = default
  method fold_esql_instruction: (esql_instruction, 'a) fold = default
  method fold_try_block: (try_block, 'a) fold = default
  method fold_sql_exception: (sql_exception, 'a) fold = default
  method fold_cursor: (cursor, 'a) fold = default
  method fold_table: (table, 'a) fold = default
  method fold_value: (value, 'a) fold = default
  method fold_rb_work_or_tran: (rb_work_or_tran, 'a) fold = default
  method fold_rb_args: (rb_args, 'a) fold = default
  method fold_connect_syntax: (connect_syntax, 'a) fold = default
  method fold_sql_type: (sql_type, 'a) fold = default
  method fold_sql_type_name: (sql_type_name, 'a) fold = default
  method fold_whenever_condition: (whenever_condition, 'a) fold = default
  method fold_whenever_continuation: (whenever_continuation, 'a) fold = default
  method fold_update_arg: (update_arg, 'a) fold = default
  method fold_sql_query: (sql_query, 'a) fold = default
  method fold_sql_select_option: (sql_select_option, 'a) fold = default
  method fold_from_stm: (from_stm, 'a) fold = default
  method fold_table_ref: (table_ref, 'a) fold = default
  method fold_join: (join, 'a) fold = default
  method fold_join_option: (join_option, 'a) fold = default
  method fold_sql_orderby: (sql_orderBy, 'a) fold = default
  method fold_sql_select: (sql_select, 'a) fold = default
  method fold_sql_update: (sql_update, 'a) fold = default
  method fold_sql_equal: (sql_equal, 'a) fold = default
  method fold_sql_op: (sql_op, 'a) fold = default
  method fold_sql_binop: (sql_binop, 'a) fold = default
  method fold_search_condition: (search_condition, 'a) fold = default
  method fold_between_condition: (between_condition, 'a) fold = default
  method fold_sql_condition_in: (sql_condition_in, 'a) fold = default
  method fold_sql_compare: (sql_compare, 'a) fold = default
  method fold_comp_operator: (comp_operator, 'a) fold = default
end

let fold_cobol_var_id (v: _ #folder) = leaf v#fold_cobol_var_id
let fold_sql_var_token (v: _ #folder) = leaf v#fold_sql_var_token

let rec fold_cobol_var (v: _ #folder) =
  handle v#fold_cobol_var
    ~continue:begin function
      | CobVarNotNull c -> fun x -> fold_cobol_var_id v c x
      | CobVarCasted (c, t) ->
          fun x -> fold_cobol_var_id v c x >> fold_sql_type v t
      | CobVarNullIndicator (c, i) ->
          fun x -> fold_cobol_var_id v c x >> fold_cobol_var_id v i
    end
and fold_variable (v: _ #folder) =
    handle v#fold_variable
      ~continue: begin function
        | SqlVar var -> fold_sql_var_token v var
        | CobolVar var -> fold_cobol_var v var
      end
and fold_literal (v: _ #folder) =
    handle v#fold_literal
      ~continue:begin function
        | LiteralVar var -> fold_variable v var
        | LiteralDot _ | LiteralStr _ | LiteralNum _ -> Fun.id
      end
and fold_sql_token (v: _ #folder) =
    handle v#fold_sql_token
      ~continue:begin function
        | SqlInstr _ -> Fun.id
        | SqlVarToken var -> fold_variable v var
        | SqlLit lit -> fold_literal v lit
        | SqlQuery q -> fold_sql_query v q
        | SqlEquality eq -> fold_sql_equal v eq
        | SqlSearchCondition sc -> fold_search_condition v sc
      end
and fold_sql_instruction (v: _ #folder) =
    handle v#fold_sql_instruction ~continue:(fold_list ~fold:fold_sql_token v)
and fold_complex_literal (v: _ #folder) =
    handle v#fold_complex_literal
      ~continue:begin function
        | SqlCompLit lit -> fold_literal v lit
        | SqlCompAsType (lit, type_name) ->
          fun x -> x >> fold_literal v lit >> fold_sql_type_name v type_name
        | SqlCompAsVar (lit, var) ->
          fun x -> x >> fold_literal v lit >> fold_sql_var_token v var
        | SqlCompFun (var, op_list) ->
          fun x -> x >> fold_sql_var_token v var >> fold_list ~fold:fold_sql_op v op_list
        | SqlCompStar -> Fun.id
      end
and fold_esql_instruction (v: _ #folder) =
    handle v#fold_esql_instruction
      ~continue:begin fun esql x ->
        match esql with
        | At (var, esql) ->
          fold_variable v var x
          >> fold_esql_instruction v esql
        | Ignore sql
        | ExecuteImmediate sql
        | Delete sql
        | Sql sql -> fold_sql_instruction v sql x
        | Begin | BeginDeclare | EndDeclare | StartTransaction | DisconnectAll -> x
        | Exeption try_block -> fold_try_block v try_block x
        | Whenever (cond, conti) ->
          fold_whenever_condition v cond x
          >> fold_whenever_continuation v conti
        | Close var
        | Include var -> fold_sql_var_token v var x
        | Connect connect_syntax -> fold_connect_syntax v connect_syntax x
        | Rollback (rb_work_or_tran_opt, rb_args_opt) ->
          fold_option ~fold:fold_rb_work_or_tran v rb_work_or_tran_opt x
          >> fold_option ~fold:fold_rb_args v rb_args_opt
        | Commit ((rb_work_or_tran_opt, _)) ->
          fold_option ~fold:fold_rb_work_or_tran v rb_work_or_tran_opt x
        | SelectInto { vars; select; select_options } ->
          fold_list ~fold:fold_cobol_var v vars x
          >> fold_sql_select v select
          >> fold_list ~fold:fold_sql_select_option v select_options
        | DeclareTable (lit, var_type_list) ->
          fold_literal v lit x
          >> fold_list ~fold:(fun v (var, typ) x ->
              x >> fold_sql_var_token v var
              >> fold_sql_type v typ) v var_type_list
        | DeclareCursor cursor ->
          fold_cursor v cursor x
        | Prepare (var, sql) ->
          fold_sql_var_token v var x
          >> fold_sql_instruction v sql
        | ExecuteIntoUsing { executed_string; opt_into_hostref_list; opt_using_hostref_list } ->
          fold_sql_var_token v executed_string x
          >> fold_option ~fold:(fold_list ~fold:fold_cobol_var) v opt_into_hostref_list
          >> fold_option ~fold:(fold_list ~fold:fold_cobol_var) v opt_using_hostref_list
        | Disconnect var_opt ->
          fold_option ~fold:fold_variable v var_opt x
        | Open (var, cob_vars) ->
          fold_sql_var_token v var x
          >> fold_option ~fold:(fold_list ~fold:fold_cobol_var) v cob_vars
        | Fetch (var, cob_vars) ->
          fold_sql_var_token v var x
          >> fold_list ~fold:fold_cobol_var v cob_vars
        | Insert (table, values) ->
          fold_table v table x
          >> fold_list ~fold:fold_value v values
        | Update (var, sql_update, update_arg_opt) ->
          fold_sql_var_token v var x
          >> fold_sql_update v sql_update
          >> fold_option ~fold:fold_update_arg v update_arg_opt
        | Savepoint variable
        | ReleaseSavepoint variable ->
          fold_variable v variable x
      end
and fold_try_block (v: _ #folder) =
  handle v#fold_try_block
  ~continue:begin fun { try_instruction; try_exceptions } x ->
    fold_esql_instruction v try_instruction x
    >> fold_list ~fold:fold_sql_exception v try_exceptions
  end
and fold_sql_exception (v: _ #folder) =
    handle v#fold_sql_exception
      ~continue:begin fun (RaiseAndPrint(var, _, cob_var)) x ->
        x
        >> fold_sql_var_token v var
        >> fold_cobol_var v cob_var
      end
and fold_cursor (v: _ #folder) =
    handle v#fold_cursor
      ~continue:begin function
        | DeclareCursorWhithHold (var, query)
        | DeclareCursorSql (var, query) ->
          fun x -> x >> fold_sql_var_token v var >> fold_sql_query v query
        | DeclareCursorVar (var, variable) ->
          fun x -> x >> fold_sql_var_token v var >> fold_variable v variable
      end
and fold_table (v: _ #folder) =
    handle v#fold_table
      ~continue:begin function
        | Table var -> fold_sql_var_token v var
        | TableLst (var, vars) ->
          fun x -> x >> fold_sql_var_token v var >> fold_list ~fold:fold_sql_var_token v vars
      end
and fold_value (v: _ #folder) =
    handle v#fold_value
      ~continue:begin function
        | ValueNull | ValueDefault -> Fun.id
        | ValueList literals ->
          fold_list ~fold:fold_literal v literals
      end
and fold_rb_work_or_tran (v: _ #folder) = leaf v#fold_rb_work_or_tran
and fold_rb_args (v: _ #folder) =
    handle v#fold_rb_args
      ~continue:begin function
        | Release -> Fun.id
        | To var -> fold_sql_var_token v var
      end
and fold_connect_syntax (v: _ #folder) =
    handle v#fold_connect_syntax
      ~continue:begin fun connect_syntax x ->
        match connect_syntax with
        | Connect_to_idby { dbname; db_conn_id; username; db_data_source; password } ->
          fold_cobol_var_id v dbname x
          >> fold_option ~fold:fold_variable v db_conn_id
          >> fold_cobol_var_id v username
          >> fold_cobol_var_id v db_data_source
          >> fold_cobol_var_id v password
        | Connect_to {  db_conn_id; username; db_data_source; password } ->
          fold_option ~fold:fold_variable v db_conn_id x
          >> fold_cobol_var_id v username
          >> fold_cobol_var_id v db_data_source
          >> fold_option ~fold:fold_cobol_var_id v password
        | Connect_using { db_data_source } ->
          fold_cobol_var_id v db_data_source x
        | Connect_user {  db_conn_id; username; db_data_source; password } ->
          fold_option ~fold:fold_variable v db_conn_id x
          >> fold_cobol_var_id v username
          >> fold_option ~fold:fold_cobol_var_id v db_data_source
          >> fold_cobol_var_id v password
        | Connect_reset var_opt ->
          fold_option ~fold:fold_variable v var_opt x
      end
and fold_sql_type (v: _ #folder) =
    handle v#fold_sql_type
      ~continue: begin fun { sql_type; size; _ } x ->
        x
        >> fold_sql_type_name v sql_type
        >> fold_option ~fold:fold_literal v size
      end
and fold_sql_type_name (v: _ #folder) = leaf v#fold_sql_type_name
and fold_whenever_condition (v: _ #folder) = leaf v#fold_whenever_condition
and fold_whenever_continuation (v: _ #folder) =
    handle v#fold_whenever_continuation
      ~continue: begin function
        | Continue -> Fun.id
        | Perform var | Goto var ->
          fold_sql_var_token v var
      end
and fold_update_arg (v: _ #folder) =
    handle v#fold_update_arg
      ~continue: begin function
        | WhereCurrentOf var -> fold_sql_var_token v var
        | WhereUpdate search_cond -> fold_search_condition v search_cond
        | UpdateSql sql -> fold_sql_instruction v sql
      end
and fold_sql_query (v: _ #folder) =
    handle v#fold_sql_query
      ~continue: begin function
        | SelectIntersect (q, q2)
        | SelectExcept (q, q2)
        | SelectUnion (q, q2) ->
          fun x -> x >> fold_sql_query v q >> fold_sql_query v q2
        | SelectQuery (select, select_opts) ->
          fun x -> x >> fold_sql_select v select >> fold_list ~fold:fold_sql_select_option v select_opts
      end
and fold_sql_select_option (v: _ #folder) =
    handle v#fold_sql_select_option
      ~continue: begin function
        | From from -> fold_from_stm v from
        | Having search_cond
        | Where search_cond -> fold_search_condition v search_cond
        | OrderBy orderBys -> fold_list ~fold:fold_sql_orderby v orderBys
        | GroupBy literals -> fold_list ~fold:fold_literal v literals
      end
and fold_from_stm (v: _ #folder) =
    handle v#fold_from_stm
      ~continue:(fold_list ~fold:fold_table_ref v)
and fold_table_ref (v: _ #folder) =
    handle v#fold_table_ref
      ~continue:begin function
        | FromLitAs (table_ref, lit) ->
          fun x -> x >> fold_table_ref v table_ref >> fold_literal v lit
        | FromLit lit -> fun x -> fold_literal v lit x
        | FromFun (var, lit) ->
          fun x -> x >> fold_sql_var_token v var >> fold_literal v lit
        | FromSelect query -> fun x -> fold_sql_query v query x
        | Join (table_ref, join, table_ref2, join_option_opt) ->
          fun x ->
            fold_table_ref v table_ref x
            >> fold_join v join
            >> fold_table_ref v table_ref2
            >> fold_option ~fold:fold_join_option v join_option_opt
      end
and fold_join (v: _ #folder) = leaf v#fold_join
and fold_join_option (v: _ #folder) =
    handle v#fold_join_option
      ~continue:begin function
        | JoinOn search_cond -> fun x -> fold_search_condition v search_cond x
        | JoinUsing vars -> fun x -> fold_list ~fold:fold_sql_var_token v vars x
      end
and fold_sql_orderby (v: _ #folder) =
    handle v#fold_sql_orderby
      ~continue:begin function
        | Asc lit | Desc lit -> fun x -> fold_literal v lit x
      end
and fold_sql_select (v: _ #folder) =
    handle v#fold_sql_select
      ~continue:(fold_list ~fold:fold_sql_op v)
and fold_sql_update (v: _ #folder) =
    handle v#fold_sql_update
      ~continue:(fold_list ~fold:fold_sql_equal v)
and fold_sql_equal (v: _ #folder) =
    handle v#fold_sql_equal
      ~continue:begin fun (var, op) x ->
        fold_sql_var_token v var x >> fold_sql_op v op end
and fold_sql_op (v: _ #folder) =
    handle v#fold_sql_op
      ~continue:begin function
        | SqlOpLit complex_lit ->
          fun x -> fold_complex_literal v complex_lit x
        | SqlOpBinop (binop, complex_lit, op) ->
          fun x ->
            fold_sql_binop v binop x
            >> fold_complex_literal v complex_lit
            >> fold_sql_op v op
      end
and fold_sql_binop (v: _ #folder) = leaf v#fold_sql_binop
and fold_search_condition (v: _ #folder) =
    handle v#fold_search_condition
      ~continue:begin function
        | WhereConditionOr (search_cond, search_cond2)
        | WhereConditionAnd (search_cond, search_cond2) ->
          fun x ->
            fold_search_condition v search_cond x
            >> fold_search_condition v search_cond2
        | WhereConditionNot search_cond ->
          fun x -> fold_search_condition v search_cond x
        | WhereConditionCompare comp ->
          fun x -> fold_sql_compare v comp x
        | WhereConditionIn cond_in -> fun x -> fold_sql_condition_in v cond_in x
        | WhereConditionBetween between_cond ->
          fun x -> fold_between_condition v between_cond x
        | WhereConditionIsNull variable -> fun x -> fold_variable v variable x
      end
and fold_between_condition (v: _ #folder) =
    handle v#fold_between_condition
      ~continue:begin fun (Between (lit, lit2, lit3)) x ->
        fold_literal v lit x
        >> fold_literal v lit2
        >> fold_literal v lit3
      end
and fold_sql_condition_in (v: _ #folder) =
    handle v#fold_sql_condition_in
      ~continue:begin fun (InVarLst (lit, complex_lits)) x ->
        fold_literal v lit x
        >> fold_list ~fold:fold_complex_literal v complex_lits
      end
and fold_sql_compare (v: _ #folder) =
    handle v#fold_sql_compare
      ~continue:begin function
        | CompareQuery (complex_lit, comp_op, sql_instr) ->
          fun x ->
            fold_complex_literal v complex_lit x
            >> fold_comp_operator v comp_op
            >> fold_sql_instruction v sql_instr
        | CompareLit (complex_lit, comp_op, complex_lit2) ->
          fun x ->
            fold_complex_literal v complex_lit x
            >> fold_comp_operator v comp_op
            >> fold_complex_literal v complex_lit2
      end
and fold_comp_operator (v: _ #folder) = leaf v#fold_comp_operator
