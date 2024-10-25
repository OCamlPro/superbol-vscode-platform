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
open Types
open Sql_ast

let c_formatted_string str = "\"" ^ str ^ "\" & x\"00\""
let empty_c_string = "x\"00\""

let comment str =
  Generated_type.Added
    { content = [ Generated_type.Comment { content = str } ];
      error_treatment = None;
      with_dot = false
    }

let line_if_not_empty line =
  if String.length (String.trim (String.sub line 7 (String.length line - 7))) > 0
  then [Generated_type.NoChange { content = line }]
  else []

let generate ~filename ~contents ~cobol_unit sql_statements =
  let linkage_section = comment "" in
  let begin_procedure_division ~loc:_ =
    (* We might want to add something at the begining of PROCEDURE DIVISION ? *)
    [ comment "" ]
  in

  (* split lines and numerotate them *)
  let lines = EzString.split contents '\n' in
  let lines = List.mapi (fun i line -> (filename, i + 1, line)) lines in

  (*string to add at the end of every sql processed*)

  (* The result will be stored in this buffer: *)
  let _final_loc = { filename; line = -1; char = 0 } in

  let error_treatment = ref None in
  let old_statements = ref [] in
  let cursor_declaration = ref [] in
  let in_pro_div = ref false in
  let num = ref 0 in

  (*GET FUNCTION*)
  (*TODOOOO*)
  let working_storage_section, new_var_map =
    let ws, nvm = Data_handling.transform ~cobol_unit sql_statements filename in
    ( [ comment ">Begin generated WORKING-STORAGE SECTION";
        Generated_type.Added
          { content = ws; error_treatment = None; with_dot = false };
        comment "> End genererated WORKING-STORAGE SECTION"
      ],
      nvm )
  in

  let cob_var_id_opt (cob_var : cobolVarId option) =
    match cob_var with
    | Some cob -> Some cob.payload
    | None -> None
  in

  (*TODO: CobVarCasted shoould act like the casted type (rn it's ignored)*)
  let cob_var_opt = function
    | Some var -> (
      match var with
      | CobVarNotNull cobolVarId -> Some cobolVarId.payload
      | CobVarCasted (var, _) -> Some var.payload
      | CobVarNullIndicator (var, _) -> Some var.payload )
    | None -> None
  in

  let var_opt = function
    | Some var -> (
      match var with
      | SqlVar sqlVarToken -> Some (c_formatted_string sqlVarToken.payload)
      | CobolVar cobol_var -> cob_var_opt (Some cobol_var) )
    | None -> None
  in

  let get_length ?(negative_if_varlen=false) str =
    match Data_handling.find_opt new_var_map str with
    | Some a ->
      if a.flags land Gix_enum.Flag.varlen > 0
      then
        if negative_if_varlen
        then -(a.length + 4)
        else a.length + 4
      else a.length
    | None ->
      let len = Sql_typeck.get_length cobol_unit str in
      if negative_if_varlen &&
         Sql_typeck.is_varying_len cobol_unit str &&
         len > 0
      then -len
      else len
  in

  let get_some_cob_var_length (cob_var : cobolVarId option) =
    match cob_var with
    | Some x -> Some (get_length x.payload)
    | None -> None
  in

  let get_some_length var =
    match var with
    | Some (CobolVar (CobVarNotNull cobol_var)) ->
      get_some_cob_var_length (Some cobol_var)
    | _ -> None
  in

  let get_type str =
    match Data_handling.find_opt new_var_map str with
    | Some a -> a.vartype
    | None -> Sql_typeck.get_type cobol_unit str
  in
  let get_scale str =
    match Data_handling.find_opt new_var_map str with
    | Some a -> - a.scale
    | None -> - Sql_typeck.get_scale cobol_unit str
  in
  let get_flags str =
    match Data_handling.find_opt new_var_map str with
    | Some a -> a.flags
    | None -> Sql_typeck.get_flags cobol_unit str
  in
  let get_at_info some_var =
    match some_var with
    | None -> (empty_c_string, 0)
    | Some var -> (
      match var with
      | SqlVar sqlVarToken -> (c_formatted_string sqlVarToken.payload, 0)
      | CobolVar cobol_var -> (
        match cobol_var with
        | CobVarNotNull cobolVarId ->
          (cobolVarId.payload, get_length cobolVarId.payload)
        | CobVarCasted (var, _) -> (var.payload, get_length var.payload)
        | CobVarNullIndicator (var, _) -> (var.payload, get_length var.payload)
        ) )
  in

  (*GENERATE FUNCTION*)
  let generate_start_end_sql prefix smt =
    let startSql =
      Generated_type.CallStatic
        { prefix; fun_name = "GIXSQLStartSQL"; ref_value = [] }
    in
    let endSql =
      Generated_type.CallStatic
        { prefix; fun_name = "GIXSQLEndSQL"; ref_value = [] }
    in
    let trans_stm = (startSql :: smt) @ [ endSql ] in
    trans_stm
  in

  let generatesql_connect_reset ~prefix ?(d_connection_id = empty_c_string)
      ?(connection_id_tl = 0) () =
    let fun_name = "GIXSQLConnectReset" in
    let ref_value =
      let prefix = prefix ^ "    " in
      [ Generated_type.Reference { prefix; var = "SQLCA" };
        Generated_type.Reference { prefix; var = d_connection_id };
        Generated_type.Value { prefix; var = string_of_int connection_id_tl }
      ]
    in
    Generated_type.CallStatic { prefix; fun_name; ref_value }
  in

  let generatesql_connect_aux ~prefix ?(data_source = empty_c_string)
      ?(data_source_tl = 0) ?(d_connection_id = empty_c_string)
      ?(connection_id_tl = 0) ?(d_dbname = empty_c_string) ?(dbname_tl = 0)
      ?(d_username = empty_c_string) ?(username_tl = 0) ?(d_password = empty_c_string)
      ?(password_tl = 0) () =
    let fun_name = "GIXSQLConnect" in
    let ref_value =
      let prefix = prefix ^ "    " in
      [ Generated_type.Reference { prefix; var = "SQLCA" };
        Generated_type.Reference { prefix; var = data_source };
        Generated_type.Value { prefix; var = string_of_int data_source_tl };
        Generated_type.Reference { prefix; var = d_connection_id };
        Generated_type.Value { prefix; var = string_of_int connection_id_tl };
        Generated_type.Reference { prefix; var = d_dbname };
        Generated_type.Value { prefix; var = string_of_int dbname_tl };
        Generated_type.Reference { prefix; var = d_username };
        Generated_type.Value { prefix; var = string_of_int username_tl };
        Generated_type.Reference { prefix; var = d_password };
        Generated_type.Value { prefix; var = string_of_int password_tl }
      ]
    in
    Generated_type.CallStatic { prefix; fun_name; ref_value }
  in

  let generatesql_connect cs prefix =
    (*TODO: Some of these a unsuported in gixSql -> emit a preproc warning
      list of unsuported connection:
      mode 5 and 6 when named ("AT/AS db_conn_id ")
      mode 4 (ex: CONNECT :DBUSR IDENTIFIED BY :DBPWD)*)
    match cs with
    | Connect_reset lit ->
      [ generatesql_connect_reset ~prefix ?d_connection_id:(var_opt lit)
          ?connection_id_tl:(get_some_length lit) ()
      ]
    | Connect_to_idby { dbname; db_conn_id; db_data_source; username; password }
      ->
      [ generatesql_connect_aux ~prefix ~data_source:db_data_source.payload
          ~data_source_tl:(get_length db_data_source.payload)
          ?d_connection_id:(var_opt db_conn_id)
          ?connection_id_tl:(get_some_length db_conn_id)
          ~d_dbname:dbname.payload
          ~dbname_tl:(get_length dbname.payload)
          ~d_username:username.payload
          ~username_tl:(get_length username.payload)
          ~d_password:password.payload
          ~password_tl:(get_length password.payload)
          ()
      ]
    | Connect_to { db_conn_id; db_data_source; username; password } ->
      [ generatesql_connect_aux ~prefix ~data_source:db_data_source.payload
          ~data_source_tl:(get_length db_data_source.payload)
          ?d_connection_id:(var_opt db_conn_id)
          ?connection_id_tl:(get_some_length db_conn_id)
          ~d_username:username.payload
          ~username_tl:(get_length username.payload)
          ?d_password:(cob_var_id_opt password)
          ?password_tl:(get_some_cob_var_length password)
          ()
      ]
    | Connect_using { db_data_source } ->
      [ generatesql_connect_aux ~prefix ~data_source:db_data_source.payload
          ~data_source_tl:(get_length db_data_source.payload)
          ()
      ]
    | Connect_user { db_conn_id; db_data_source; username; password } ->
      [ generatesql_connect_aux ~prefix
          ?data_source:(cob_var_id_opt db_data_source)
          ?data_source_tl:(get_some_cob_var_length db_data_source)
          ?d_connection_id:(var_opt db_conn_id)
          ?connection_id_tl:(get_some_length db_conn_id)
          ~d_username:username.payload
          ~username_tl:(get_length username.payload)
          ~d_password:password.payload
          ~password_tl:(get_length password.payload)
          ()
      ]
  in

  let change_error ~prefix c k =
    let old_not_found_whenever, old_warning_whenever, old_error_whenever =
      match !error_treatment with
      | Some Generated_type.
      { sql_error_whenever; not_found_whenever; sql_warning_whenever ; _ } ->
       not_found_whenever, sql_warning_whenever, sql_error_whenever
      | None -> None, None, None in
    let continuation =
      match k with
      | Sql_ast.Continue -> Generated_type.Continue
      | Sql_ast.Perform x -> Generated_type.Perform x.payload
      | Sql_ast.Goto x -> Generated_type.Goto x.payload
    in
    let new_error =
      match c with
      | Sql_ast.Not_found_whenever ->
        Generated_type.
          { prefix;
            not_found_whenever = Some continuation;
            sql_error_whenever = old_error_whenever;
            sql_warning_whenever = old_warning_whenever
          }
      | Sql_ast.SqlError_whenever ->
        { prefix;
          not_found_whenever = old_not_found_whenever;
          sql_error_whenever = Some continuation;
          sql_warning_whenever = old_warning_whenever
        }
      | Sql_ast.SqlWarning_whenever ->
        { prefix;
          not_found_whenever = old_not_found_whenever;
          sql_error_whenever = old_error_whenever;
          sql_warning_whenever = Some continuation
        }
    in
    error_treatment := Some new_error;
  in

  let get_name_cobol_var (cobol_var : cobol_var) =
    match cobol_var with
    | CobVarNotNull c -> c.payload
    | CobVarCasted (c, _) -> c.payload
    | CobVarNullIndicator (c, _) -> c.payload
  in

  let get_null_indicator (cobol_var : cobol_var) =
    match cobol_var with
    | CobVarNullIndicator (_, n) -> n.payload
    | _ -> "0"
  in


  let generate_set_result_param prefix arg =
    let fun_name = "GIXSQLSetResultParams" in
    let var_name = get_name_cobol_var arg in
    let vars = Sql_typeck.get_elementary_component cobol_unit var_name in
    let aux var_name =
      let ref_value =
        let prefix = prefix ^ "    " in
        [ Generated_type.Value { prefix; var = string_of_int (get_type var_name) };
          Generated_type.Value { prefix; var = string_of_int (get_length var_name) };
          Generated_type.Value { prefix; var = string_of_int (get_scale var_name) };
          Generated_type.Value { prefix; var = string_of_int (get_flags var_name) };
          Generated_type.Reference { prefix; var = var_name };
          Generated_type.Reference
            { prefix; var = get_null_indicator arg }
        ]
      in
      Generated_type.CallStatic { prefix; fun_name; ref_value }
    in
    List.map aux vars
  in

  (* Todo: refactory? *)
  let generate_set_sql_param prefix arg =
    let fun_name = "GIXSQLSetSQLParams" in
    let var_name = get_name_cobol_var arg in
    let vars = Sql_typeck.get_elementary_component cobol_unit var_name in
    let aux h =
      let ref_value =
        let prefix = prefix ^ "    " in
        [ Generated_type.Value { prefix; var = string_of_int (get_type h) };
          Generated_type.Value { prefix; var = string_of_int (get_length h) };
          Generated_type.Value { prefix; var = string_of_int (get_scale h) };
          Generated_type.Value { prefix; var = string_of_int (get_flags h) };
          Generated_type.Reference { prefix; var = h };
          Generated_type.Reference
            { prefix; var = get_null_indicator arg }
        ]
      in
      Generated_type.CallStatic { prefix; fun_name; ref_value }
    in
    List.map aux vars
  in

  let generate_select_into_one ?at prefix param_count into_count =
    let at_name, at_size = get_at_info at in
    let fun_name = "GIXSQLExecSelectIntoOne" in
    let ref_value =
      let var_name =
        num := !num + 1;
        "SQ" ^ string_of_int !num
      in
      let prefix = prefix ^ "    " in
      [ Generated_type.Reference { prefix; var = "SQLCA" };
        Generated_type.Reference { prefix; var = at_name };
        Generated_type.Value { prefix; var = string_of_int at_size };
        Generated_type.Reference { prefix; var = var_name };
        Generated_type.Value
          { prefix; var = string_of_int param_count };
        Generated_type.Value { prefix; var = string_of_int into_count }
      ]
    in
    Generated_type.CallStatic { prefix; fun_name; ref_value }
  in

  let generate_select_into prefix vars select_options select ?at () =
    let selects_into_vars =
      List.flatten @@ List.map (generate_set_result_param prefix) vars
    in
    let cob_vars =
      let option_vars =
        Cobol_common.Visitor.fold_list
          ~fold:Sql_ast.Visitor.fold_sql_select_option
          Misc.cob_var_extractor_folder select_options []
      in
      let select_vars = Sql_ast.Visitor.fold_sql_select
          Misc.cob_var_extractor_folder select []
      in List.rev (option_vars @ select_vars)
    in
    let selects_param_vars = List.flatten @@
      List.map (generate_set_sql_param prefix) cob_vars
    in
    let selects_into = generate_select_into_one prefix
      (List.length selects_param_vars) (List.length selects_into_vars) ?at
    in
    let trans_stm =
      generate_start_end_sql prefix
        (selects_into_vars @ selects_param_vars @ [ selects_into ])
    in
    trans_stm
  in

  let generate_fetch_into_one prefix (sql : sqlVarToken) =
    let var = c_formatted_string
      (Misc.extract_filename filename ^ "_" ^ sql.payload)
    in
    Generated_type.CallStatic
      { prefix;
        fun_name = "GIXSQLCursorFetchOne";
        ref_value =
          (let prefix = prefix ^ "    " in
           [ Generated_type.Reference { prefix; var = "SQLCA" };
             Generated_type.Reference { prefix; var }
           ] )
      }
  in

  let generate_fetch prefix sql cob =
    let fetch_into_vars =
      List.flatten @@ List.map (generate_set_result_param prefix) cob
    in
    let fetch_into = generate_fetch_into_one prefix sql in
    let trans_stm =
      generate_start_end_sql prefix (fetch_into_vars @ [ fetch_into ])
    in
    trans_stm
  in

  let generate_GIXSQLExec prefix name ?at () =
    let at_name, at_size = get_at_info at in
    let fun_name = "GIXSQLExec" in
    let ref_value =
      let prefix = prefix ^ "    " in
      [ Generated_type.Reference { prefix; var = "SQLCA" };
        Generated_type.Reference { prefix; var = at_name };
        Generated_type.Value { prefix; var = string_of_int at_size };
        Generated_type.Reference { prefix; var = name }
      ]
    in
    Generated_type.CallStatic { prefix; fun_name; ref_value }
  in

  let generate_GIXSQLExecParam ?at prefix name params_count =
    let at_name, at_size = get_at_info at in
    let fun_name = "GIXSQLExecParams" in
    let ref_value =
      let prefix = prefix ^ "    " in
      [ Generated_type.Reference { prefix; var = "SQLCA" };
        Generated_type.Reference { prefix; var = at_name };
        Generated_type.Value { prefix; var = string_of_int at_size };
        Generated_type.Reference { prefix; var = name };
        Generated_type.Value
          { prefix; var = string_of_int params_count }
      ]
    in
    Generated_type.CallStatic { prefix; fun_name; ref_value }
  in

  let generate_declare prefix ?at () =
    let var_name =
      num := !num + 1;
      "SQ" ^ string_of_int !num (*I pray for it to be in the good order*)
    in
    let declare = generate_GIXSQLExec prefix var_name ?at () in

    let trans_stm = generate_start_end_sql prefix [ declare ] in
    trans_stm
  in

  let generate_rollback prefix rb_work_or_tran rb_args ?at () =
    match (rb_work_or_tran, rb_args) with
    | None, None ->
      generate_start_end_sql prefix
        [ generate_GIXSQLExec prefix (c_formatted_string "ROLLBACK") ?at () ]
    | _, Some (To _) -> generate_declare prefix ()
    | _ -> [ Generated_type.Todo { prefix } ]
  in

  let generate_commit prefix rb_work_or_tran rb_args ?at () =
    match (rb_work_or_tran, rb_args) with
    | None, false
    | Some Work, false ->
      generate_start_end_sql prefix
        [ generate_GIXSQLExec prefix (c_formatted_string "COMMIT") ?at () ]
    | _ -> [ Generated_type.Todo { prefix } ]
  in

  let generate_simpl_execute_immediat prefix var ?at () =
    let name =
      match var with
      | [ Sql_ast.SqlVarToken (CobolVar (CobVarNotNull var)) ] -> var.payload
      | [ Sql_ast.SqlVarToken (CobolVar (CobVarCasted (var, _))) ] -> var.payload
      | _ ->
        num := !num + 1;
        "SQ" ^ string_of_int !num
    in
    let at_name, at_size = get_at_info at in
    let fun_name = "GIXSQLExecImmediate" in
    let ref_value =
      let prefix = prefix ^ "       " in
      [ Generated_type.Reference { prefix; var = "SQLCA" };
        Generated_type.Reference { prefix; var = at_name };
        Generated_type.Value { prefix; var = string_of_int at_size };
        Generated_type.Reference { prefix; var = name };
        Generated_type.Value { prefix; var =
          string_of_int (get_length ~negative_if_varlen:true name) }
      ]
    in
    [ Generated_type.CallStatic { prefix; fun_name; ref_value } ]
  in

  let add_to_cursor_declaration prefix cur ?at () =
    num := !num + 1;
    let name = "SQ" ^ string_of_int !num in
    let adding = (prefix, cur, at, name) in
    let cd = adding :: !cursor_declaration in
    cursor_declaration := cd
  in

  let create_from_cursor_declaration (prefix, cur, at, var_name) =
    let at_name, at_size = get_at_info at in
    let cur_name, cob_var_lst, var_name, _with_hold =
      match cur with
      | DeclareCursorSql (cur_name, sql) ->
        ( cur_name,
          List.rev @@ Sql_ast.Visitor.fold_sql_query
            Misc.cob_var_extractor_folder sql [],
          var_name,
          false )
      | DeclareCursorWhithHold (cur_name, query) ->
        ( cur_name,
          List.rev @@ Sql_ast.Visitor.fold_sql_query
            Misc.cob_var_extractor_folder query [],
          var_name,
          true )
      | DeclareCursorVar (cur_name, cur_var) ->
        let var =
          match cur_var with
          | SqlVar _ -> var_name
          | CobolVar v -> get_name_cobol_var v
        in
        (cur_name, [], var, false)
    in

    let cursor_name = c_formatted_string
      (Misc.extract_filename filename
      ^ "_" ^ cur_name.payload)
    in

    let fun_name, cursor_declare =
      let prefix = prefix ^ "    " in
      match cob_var_lst with
      | [] ->
        ( "GIXSQLCursorDeclare",
          [ Generated_type.Reference { prefix; var = var_name };
            Generated_type.Value { prefix; var =
            string_of_int (get_length ~negative_if_varlen:true var_name) }
          ] )
      | _ ->
        ( "GIXSQLCursorDeclareParams",
          [ Generated_type.Reference { prefix; var = var_name };
            Generated_type.Value { prefix; var = "0" };
            Generated_type.Value
              { prefix; var = string_of_int (List.length cob_var_lst) }
          ] )
    in
    let adding =
      let ref_value =
        let prefix = prefix ^ "    " in
        [ Generated_type.Reference { prefix; var = "SQLCA" };
          Generated_type.Reference { prefix; var = at_name };
          Generated_type.Value { prefix; var = string_of_int at_size };
          Generated_type.Reference { prefix; var = cursor_name };
          Generated_type.Value { prefix; var = "0" }
        ]
        @ cursor_declare
      in
      (List.flatten @@ List.map (generate_set_sql_param prefix) cob_var_lst)
      @ [ Generated_type.CallStatic { prefix; fun_name; ref_value } ]
    in
    let adding =
      [ Generated_type.Section
          { name =
              "GIXSQL-CI-P-"
              ^ Misc.extract_filename filename
              ^ "-" ^ cur_name.payload
          }
      ]
      @
      match cob_var_lst with
      | [] -> adding
      | _ -> generate_start_end_sql prefix adding
    in
    Generated_type.Added
      { content = adding;
        error_treatment = !error_treatment;
        with_dot = true
      }
  in

  let end_procedure_division cd_list =
    Generated_type.Added
      { content =
          [ Generated_type.Comment
              { content = "> ESQL CURSOR DECLARATIONS (START)" };
            Generated_type.GotoStatement
              { prefix = "           "; target = "GIX-SKIP-CRSR-INIT" }
          ];
        error_treatment = None;
        with_dot = false
      }
    :: List.map create_from_cursor_declaration cd_list
    @ [ Generated_type.Added
          { content =
              [ Generated_type.Section { name = "GIX-SKIP-CRSR-INIT" };
                Generated_type.Comment
                  { content = "> ESQL CURSOR DECLARATIONS (END)" }
              ];
            error_treatment = None;
            with_dot = false
          }
      ]
  in

  let generate_prepare_stm prefix (var_name : sqlVarToken) sql_instr ?at () =
    let at_name, at_size = get_at_info at in
    let sql_name =
      match sql_instr with
      | [ Sql_ast.SqlVarToken (CobolVar (CobVarNotNull cobolVarId)) ] ->
        cobolVarId.payload
      | [ Sql_ast.SqlVarToken (CobolVar (CobVarCasted (cobolVarId, _))) ] ->
        cobolVarId.payload
      | _ -> failwith "Not implemented in Gix"
      (*These case are not implemented in GixSql's runtime *)
    in
    let ref_value =
      let prefix = prefix ^ "    " in
      let len = get_length ~negative_if_varlen:true sql_name in
      [ Generated_type.Reference { prefix; var = "SQLCA" };
        Generated_type.Reference { prefix; var = at_name };
        Generated_type.Value { prefix; var = string_of_int at_size };
        Generated_type.Reference
          { prefix; var = c_formatted_string var_name.payload };
        Generated_type.Reference { prefix; var = sql_name };
        Generated_type.Value { prefix; var = string_of_int len }
      ]
    in
    [ Generated_type.CallStatic
        { prefix; fun_name = "GIXSQLPrepareStatement"; ref_value }
    ]
  in

  (*
     GIXSQL     CALL STATIC "GIXSQLExecPrepared" USING
     GIXSQL         BY REFERENCE SQLCA
     GIXSQL         BY REFERENCE x"00"
     GIXSQL         BY VALUE 0
     GIXSQL         BY REFERENCE "SQLSTMT1" & x"00"
     GIXSQL         BY VALUE 2
     GIXSQL     END-CALL *)
  let generate_exec_prepared prefix (executed_string : sqlVarToken)
      into_hostref_list ?at () =
    let at_name, at_size = get_at_info at in
    let ref_value =
      let prefix = prefix ^ "    " in
      [ Generated_type.Reference { prefix; var = "SQLCA" };
        Generated_type.Reference { prefix; var = at_name };
        Generated_type.Value { prefix; var = string_of_int at_size };
        Generated_type.Reference
          { prefix; var = c_formatted_string executed_string.payload };
        Generated_type.Value
          { prefix; var = string_of_int (List.length into_hostref_list) }
        (*todo*)
      ]
    in
    Generated_type.CallStatic
      { prefix; fun_name = "GIXSQLExecPrepared"; ref_value }
  in

  let generate_exec_prepared_into prefix (executed_string : sqlVarToken)
      into_hostref_list opt_using_hostref_list ?at () =
    let at_name, at_size = get_at_info at in
    let ref_value =
      let prefix = prefix ^ "    " in
      [ Generated_type.Reference { prefix; var = "SQLCA" };
        Generated_type.Reference { prefix; var = at_name };
        Generated_type.Value { prefix; var = string_of_int at_size };
        Generated_type.Reference
          { prefix; var = c_formatted_string executed_string.payload };
        Generated_type.Value
          { prefix; var = string_of_int (List.length into_hostref_list) };
        Generated_type.Value
          { prefix; var = string_of_int (List.length opt_using_hostref_list) }
        (*todo*)
      ]
    in
    Generated_type.CallStatic
      { prefix; fun_name = "GIXSQLExecPreparedInto"; ref_value }
  in

  let generate_execute_into_using prefix executed_string
      ?(opt_into_hostref_list = []) ?(opt_using_hostref_list = []) ?at () =
    let into_hostref_set_result_param = List.flatten @@
      List.map (generate_set_result_param prefix) opt_into_hostref_list
    in
    let using_hostref_set_sql = List.flatten @@
      List.map (generate_set_sql_param prefix) opt_using_hostref_list
    in
    let exec_prepared =
      match opt_into_hostref_list with
      | [] ->
        generate_exec_prepared prefix executed_string opt_using_hostref_list ?at
          ()
      | _ ->
        generate_exec_prepared_into prefix executed_string
          opt_using_hostref_list opt_into_hostref_list ?at ()
    in
    let trans_stm =
      generate_start_end_sql prefix
        ( using_hostref_set_sql @ into_hostref_set_result_param
        @ [ exec_prepared ] )
    in
    trans_stm
  in

  let generate_close_cursor prefix sql_var_token =
    let fun_name = "GIXSQLCursorClose" in
    let ref_value =
      let prefix = prefix ^ "    " in
      [ Generated_type.Reference { prefix; var = "SQLCA" };
        Generated_type.Reference
          { prefix;
            var = c_formatted_string
              (Misc.extract_filename filename
              ^ "_" ^ sql_var_token)
          }
      ]
    in
    Generated_type.CallStatic { prefix; fun_name; ref_value }
  in

  let generate_open_cursor_aux prefix cursor_name =
    let prefix = prefix ^ "   " in
    [ Generated_type.PerformStatement
        { prefix; target = "GIXSQL-CI-P-" ^ cursor_name };
      Generated_type.If
        { prefix;
          condition = "SQLCODE = 0";
          if_stm =
            (let prefix = prefix ^ "   " in
             [ Generated_type.Move
                 { prefix; src = "X"; dest = "GIXSQL-CI-F-" ^ cursor_name }
             ] )
        }
    ]
  in

    let generate_open_cursor prefix (cursor_name : sqlVarToken) cobol_lst =
      (* in TSQL004B, the using statment doesn't seem to change the open statement *)
      ignore(cobol_lst);
      let cursor_name' =
        Misc.extract_filename filename ^ "-" ^ cursor_name.payload
      in
      let if2 =
        let prefix = prefix ^ "   " in
        [ Generated_type.CallStatic
            { prefix;
              fun_name = "GIXSQLCursorOpen";
              ref_value =
                (let prefix = prefix ^ "   " in
                 [ Generated_type.Reference { prefix; var = "SQLCA" };
                   Generated_type.Reference
                     { prefix;
                       var = c_formatted_string
                         (Misc.extract_filename filename
                         ^ "_" ^ cursor_name.payload)
                     }
                 ] )
            }
        ]
      in
      [ Generated_type.If
          { prefix;
            condition = "GIXSQL-CI-F-" ^ cursor_name' ^ " = ' '";
            if_stm = generate_open_cursor_aux prefix cursor_name'
          };
        Generated_type.If
          { prefix;
            condition = "GIXSQL-CI-F-" ^ cursor_name' ^ " = 'X'";
            if_stm = if2
          }
      ]
    in

  let generate_declare_cursor prefix cur =
    let curname =
      match cur with
      | DeclareCursorSql (curname, _)
      | DeclareCursorVar (curname, _)
      | DeclareCursorWhithHold (curname, _) ->
        curname
    in
    let curname = Misc.extract_filename filename ^ "-" ^ curname.payload in
    let if1 = generate_open_cursor_aux prefix curname in
    [ Generated_type.IfElse
        { prefix;
          condition = "GIXSQL-CI-F-" ^ curname ^ " = ' '";
          if_stm = if1;
          else_stm =
            [ Generated_type.Move
                { prefix = prefix ^ "   "; src = "0"; dest = "SQLCODE" }
            ]
        }
    ]
  in

  let generate_insert prefix ?value_list ?at () =
    match value_list with
    | None -> generate_declare prefix ?at ()
    | Some value_list ->
      let set_param_stmts = List.flatten @@
        List.map (generate_set_sql_param prefix) value_list
      in
      let var_name =
        num := !num + 1;
        "SQ" ^ string_of_int !num (*I pray for it to be in the good order*)
      in
      let exec_stmt =
        let param_count = List.length set_param_stmts in
        generate_GIXSQLExecParam prefix var_name param_count ?at
      in
      generate_start_end_sql prefix (set_param_stmts @ [exec_stmt])
  in

  let generate_at prefix sql ?at () =
    match sql with
    | SelectInto { vars; select_options; select } ->
      generate_select_into prefix vars select_options select ?at () (*TODO AT*)
    | Rollback (rb_work_or_tran, rb_args) ->
      generate_rollback prefix rb_work_or_tran rb_args ?at ()
    | Commit (rb_work_or_tran, b) ->
      generate_commit prefix rb_work_or_tran b ?at ()
    | DeclareCursor cur ->
      add_to_cursor_declaration prefix cur ?at ();
      if !in_pro_div then begin
        generate_declare_cursor prefix cur
      end else
        []
    | ExecuteImmediate var -> generate_simpl_execute_immediat prefix var ?at ()
    | Insert (_, value_list) ->
      let value_list = List.rev @@
        Cobol_common.Visitor.fold_list
        ~fold:Sql_ast.Visitor.fold_value Misc.cob_var_extractor_folder
        value_list []
      in generate_insert prefix ~value_list ?at ()
    | Savepoint _
    | ReleaseSavepoint _
    | StartTransaction ->
      generate_declare prefix ?at ()
    | Sql sql -> (
      match sql with
      | Sql_ast.SqlInstr w :: _ when w = "VAR" ->
        []
        (*TODO: find what this should be replaced with. I think Gix juste ignore these instruction, but mabe not*)
      | _ -> generate_declare prefix ?at () )
    | Prepare (var_name, sql_instr) ->
      generate_prepare_stm prefix var_name sql_instr ?at ()
    | ExecuteIntoUsing
        { executed_string; opt_into_hostref_list; opt_using_hostref_list } ->
      generate_execute_into_using prefix executed_string ?opt_into_hostref_list
        ?opt_using_hostref_list ?at ()
    | DeclareTable _ ->
      (*Parser error on DECLARE TABLE statements in Gix, idk if Gix runtime can handle it*)
      (* indeed, gix *seems* to not be doing anything with it, this requires further testing tho *)
      [ Generated_type.Todo { prefix } ]
    | Delete sql_instr ->
      let value_list = List.rev @@
        Sql_ast.Visitor.fold_sql_instruction Misc.cob_var_extractor_folder
        sql_instr []
      in
      generate_insert prefix ~value_list ?at ()
    | Update _ ->
      [ Generated_type.Todo { prefix } ]
      (*Unexeped At, should trigger an error*)
    | _ -> failwith "Unexeped At"
  in

  let rec generatesql ~loc ~line esql_instuction =
    (* tabulation makes this fails somehow, TSQL009A *)
    let prefix = Generated_type.Printer.preproc_prefix
                 ^ String.sub line 6 (loc.char-6) in
    match esql_instuction with
    | Sql _
    | SelectInto _
    | StartTransaction
    | DeclareTable _
    | DeclareCursor _
    | Prepare _
    | ExecuteImmediate _
    | ExecuteIntoUsing _
    | Savepoint _
    | ReleaseSavepoint _
    | Rollback _
    | Commit _
    | Insert _
    | Delete _
    | Update _ ->
      generate_at prefix esql_instuction ()
    | Include sqlvar ->
      (*       let prefix = String.sub line 0 loc.char in *)
      [ Generated_type.Copy { prefix; file_name = sqlvar.payload } ]
    | Connect cs -> generatesql_connect cs prefix
    | Disconnect var ->
      [ generatesql_connect_reset ~prefix ?d_connection_id:(var_opt var)
          ?connection_id_tl:(get_some_length var) ()
      ]
    | DisconnectAll ->
      [ generatesql_connect_reset ~prefix
          ?d_connection_id:(Some (c_formatted_string "*")) ()
      ]
    | Whenever (c, k) ->
      change_error ~prefix c k;
      []
    | Begin -> generate_declare prefix ()
    | BeginDeclare
    | EndDeclare ->
      [] (*do nothing*)
    | Close var -> [ generate_close_cursor prefix var.payload ]
    | At (at, sql) -> begin
      match sql with
      (*Unexpeted At, we print an error and ignore it*)
      (*todo: change Generated_type.NonFatalErrorWarning by proper warning*)
      | Fetch _
      | Open _
      | Close _ ->
        Generated_type.NonFatalErrorWarning
          { content =
              "AT DB-NAME is not allowed for CURSOR access, always used from \
               CURSOR DECLARE"
          }
        :: generatesql ~loc ~line sql
      | _ -> generate_at prefix sql ~at ()
    end
    | Open (sql_var_token, cobol_lst) ->
      generate_open_cursor prefix sql_var_token cobol_lst
    | Fetch (sql, cob) -> generate_fetch prefix sql cob
    | Exeption e ->
      let var_name =
        num := !num + 1;
        "SQ" ^ string_of_int !num (*I pray for it to be in the good order*)
      in
      let cob_var_list = List.rev @@
        Sql_ast.Visitor.fold_try_block Misc.cob_var_extractor_folder e []
      in
      let set_sql_params_stmts = List.flatten @@
      List.map (generate_set_sql_param prefix) cob_var_list
      in
      generate_start_end_sql prefix
      set_sql_params_stmts @
      [generate_GIXSQLExecParam
        prefix var_name (List.length set_sql_params_stmts)]
    | Ignore _ ->
      (*Ignore, not implemented in Gix (but if this is just ignore, this should do the trick)*)
      []
  in

  let rec output lines statements =
    match statements with
    | [] ->
      List.map
        (fun (_, _, line) -> Generated_type.NoChange { content = line })
        lines
    | (begin_loc, stmt) :: statements -> begin
      match begin_loc with
      | None ->
        let res =
          List.map
            (fun (_, _, line) -> Generated_type.NoChange { content = line })
            lines
        in
        begin
          match stmt with
          | END_PROCEDURE_DIVISION ->
            res @ end_procedure_division (List.rev !cursor_declaration)
          | _ -> res
        end
      | Some begin_loc -> output_statement lines begin_loc stmt statements
    end
  and output_statement cur_lines begin_loc stmt statements =
    match cur_lines with
    | [] -> assert false
    | (filename, i, line) :: lines -> (
      if filename <> begin_loc.filename || i < begin_loc.line then begin
        Generated_type.NoChange { content = line }
        :: output_statement lines begin_loc stmt statements
      end else
        match stmt with
        | LINKAGE_SECTION { defined } ->
          if defined then begin
            Generated_type.NoChange { content = line }
            :: ([ linkage_section ] @ output lines statements)
          end else begin
            (* idk why she added that, it makes the test 7A fail *)
            comment "> Add missing LINKAGE SECTION"
            :: Generated_type.Added
                 { content = [ Generated_type.LinkageSection ];
                   error_treatment = None;
                   with_dot = false
                 }
            :: ([ linkage_section ] @ output cur_lines statements)
          end
        | WORKING_STORAGE { defined } ->
          if defined then begin
            Generated_type.NoChange { content = line }
            :: (working_storage_section @ output lines statements)
          end else begin
            comment "> Add missing WORKING-STORAGE SECTION"
            :: Generated_type.Added
                 { content = [ Generated_type.WorkingStorageSection ];
                   error_treatment = None;
                   with_dot = false
                 }
            :: (working_storage_section @ output cur_lines statements)
          end
        | EXEC_SQL_IGNORE { end_loc; begin_of_ignore_loc } ->
            Printf.eprintf "%d,%d\n" begin_of_ignore_loc.line begin_of_ignore_loc.char;
          begin if i = begin_loc.line
            then [comment "ESQL IGNORED SECTION"]
            else [] end
          @
          begin if i = begin_of_ignore_loc.line
            then
              let end_char_pos =
                if i = end_loc.line
                then end_loc.char
                else String.length line
              in
              let start_char_pos = begin_of_ignore_loc.char in
              let len = end_char_pos - start_char_pos in
              let prefix = String.sub line 0 7 in
              let line = String.sub line start_char_pos len in
              line_if_not_empty (prefix ^ line)
            else if end_loc.line = i
            then
              let line = String.sub line 0 end_loc.char in
              line_if_not_empty line
            else [] end
          @
          if i = end_loc.line
          then comment "END OF ESQL IGNORED SECTION"
               :: output lines statements
          else
            (if i > begin_of_ignore_loc.line
            then [Generated_type.NoChange { content = line }]
            else []) @ output_statement lines begin_loc stmt statements
        | EXEC_SQL { end_loc; with_dot; tokens } ->
          old_statements := line :: !old_statements;
          if i = end_loc.line then begin
            let trans_stm = generatesql ~loc:begin_loc ~line tokens in
            let old_stms = List.rev !old_statements in
            let with_dot, error_treatment =
              match trans_stm with
              | [] ->
                ( false,
                  None
                  (*if nothing is generated, we don't need error treatment or dots *)
                )
              | _ -> (
                match tokens with
                | DeclareCursor _
                | At (_, DeclareCursor _) ->
                  (with_dot, None)
                | _ ->
                  ( with_dot, !error_treatment))
            in
            old_statements := [];
            Generated_type.Change
              { old_stms; trans_stm; error_treatment; with_dot }
            :: output lines statements
          end else
            output_statement lines begin_loc stmt statements
        | PROCEDURE_DIVISION_DOT { end_loc } ->
          if i = end_loc.line then begin
            comment ("> REMOVED: " ^ line)
            :: (* for now, just put it back *)
               Generated_type.Added
                 { content = [ Generated_type.ProcedureDivision ];
                   error_treatment = None;
                   with_dot = false
                 }
            :: output lines statements
          end else
            comment ("> REMOVED: " ^ line)
            :: output_statement lines begin_loc stmt statements
        | DECLARATION _ ->
          comment ("> REMOVED: " ^ line) :: output lines statements
          (*
        | IS_SQLVAR { end_loc } ->

             if i = begin_loc.line then begin
               let before_macro = String.sub line 0 begin_loc.char in
               Printf.bprintf ctxt.b "%s%s" before_macro
                 "SOME STRING THAT REPLACE IS SQLVAR";
               if begin_loc.line <> end_loc.line then
                 Printf.bprintf ctxt.b "\n          "
             end;
             if i = end_loc.line then (
               let len = String.length line in
               (* This code won't work with tabulations, because
                  the end_loc.char is wrong in such a case *)
               let after_macro =
                 String.sub line (end_loc.char + 1) (len - end_loc.char - 1)
               in
               Printf.bprintf ctxt.b "%s\n" after_macro;
               output lines statements
             ) else
               output_statement lines begin_loc stmt statements *)
        | BEGIN_PROCEDURE_DIVISION { enabled } ->
          in_pro_div := true;
          ( if !enabled then
              begin_procedure_division ~loc:begin_loc
            else
              [ comment "> BEGIN PROCEDURE DIVISION disabled" ] )
          @ output cur_lines statements
        | END_PROCEDURE_DIVISION ->
          end_procedure_division (List.rev !cursor_declaration)
          @ output cur_lines statements
        | COPY { end_loc; filename; contents } ->
          let added = comment ("> INLINED:" ^ line) in
          if i = end_loc.line then begin
            let copylines = EzString.split contents '\n' in
            let copylines =
              List.mapi (fun i line -> (filename, i + 1, line)) copylines
            in
            let lines = copylines @ lines in
            added :: output lines statements
          end else
            added :: output_statement lines begin_loc stmt statements )
  in
  let result = output lines sql_statements in
  let b = Buffer.create 1000 in
  let ctxt = { b; main_filename = filename } in
  Printf.bprintf ctxt.b "%s"
    (Format.asprintf "%a" Generated_type.Printer.pp result);
  Buffer.contents b
