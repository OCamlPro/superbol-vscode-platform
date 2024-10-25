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

open Cobol_common.Srcloc.TYPES
open Cobol_common.Srcloc.INFIX

module Visitor = Cobol_common.Visitor

module TYPES = struct

  (** Type definitions to help representing and querying (partially) parsed
      COBOL programs. *)


  (** Information returned by {!val:Lookup.element_at_position}. *)
  type element_at_position =
    {
      element_at_position: element_in_context option;
      enclosing_compilation_unit_name: string option;
    }
  and element_in_context =
    | Data_name of
        Cobol_ptree.qualname
    | Data_full_name of
        Cobol_ptree.qualname
    | Data_item of
        {
          full_qn: Cobol_ptree.qualname option;
          def_loc: srcloc;
        }
    | Proc_name of
        {
          qn: Cobol_ptree.qualname;
          in_section: Cobol_unit.Types.procedure_section option;
        }

  type name_definition =
    {
      as_paragraph: paragraph_definition option;   (* link to AST's paragraph *)
      as_item: item_definition option;
    }
  and paragraph_definition =
    Cobol_ptree.paragraph with_loc
  and item_definition =
    Cobol_data.Types.item_definition with_loc

  type copy_operation = string with_loc            (** with loc of [COPY ...] *)

  type approx_typing_info =
    | Alphanum
    | Any
    | Boolean
    | Condition
    | Group
    | Index
    | Numeric
    | NumericEdited
    | ObjectRef
    | Pointer

    type procedure_at_position =
      {
        cu: Cobol_unit.Types.cobol_unit option;
        proc_name: Cobol_ptree.qualname option;
      }
end
open TYPES

(* --- *)

let baseloc_of_qualname: Cobol_ptree.qualname -> srcloc = function
  | Name name
  | Qual (name, _) -> ~@name

(* TODO: manipulate positions that are combined with a filename instead of
   passing `filename` around all the time. *)

(** [lexloc_of_qualname_in ~filename qualname] returns the full location of
    [qualname], from the first qualifier to the last.  This function is
    temporary and is expected to be replaced once a better way of getting this
    location is found. *)
let lexloc_of_qualname_in ~filename (qn: Cobol_ptree.qualname) =
  let rec end_pos: Cobol_ptree.qualname -> Lexing.position = function
    | Name n -> Cobol_common.Srcloc.end_pos_in ~filename ~@n
    | Qual (_, qn) -> end_pos qn
  in
  match qn with
  | Name n -> Cobol_common.Srcloc.lexloc_in ~filename ~@n
  | Qual (n, qn) -> Cobol_common.Srcloc.start_pos_in ~filename ~@n, end_pos qn

(** [qualname_at_pos ~filename qualname pos] returns the qualname built from all
    the qualifiers of [qualname] that are after or at position [pos] ion
    [filename].  This function is temporary and is expected to be replaced once
    a better way of finding the qualname is implemented. *)
let rec qualname_at_pos ~filename (qn: Cobol_ptree.qualname) pos =
  match qn with
  | Name _ ->
      qn
  | Qual (name, qn') ->
      try
        let lexloc = lexloc_of_qualname_in ~filename qn in
        if not (Lsp_position.is_after_lexloc pos lexloc) &&
           not (Lsp_position.is_in_srcloc ~filename pos ~@name)
        then qualname_at_pos ~filename qn' pos
        else qn
      with Invalid_argument _ -> qn                               (* dummy loc *)

(* --- *)

(** [element_at_position pos group] seeks the compilation unit name and qualified
    name at the given position [pos], in typed compilation group [group]. *)
let element_at_position ~uri pos group : element_at_position =
  let filename = Lsp.Uri.to_path uri in
  let open struct

    type acc =
      {
        elt: element_at_position;
        context: context;
      }
    and context =
      | Data_decls
      | Procedure of Cobol_unit.Types.procedure_section option

    let init =
      {
        elt = { element_at_position = None;
                enclosing_compilation_unit_name = None };
        context = Data_decls;                       (* does not really matter *)
      }

    let result acc = acc.elt

  end in

  let enter_context context ({ context = prev_context; _ } as acc) =
    Visitor.do_children_and_then { acc with context }
      (fun acc -> { acc with context = prev_context })
  in

  let on_data_full_name qn ({ elt; _ } as acc) =
    { acc with
      elt = { elt with element_at_position = Some (Data_full_name qn) } }
  and on_data_name qn ({ elt; _ } as acc) =
    { acc with
      elt = { elt with element_at_position = Some (Data_name qn) } }
  and on_data_item ?full_qn def_loc ({ elt; _ } as acc) =
    { acc with
      elt = { elt with element_at_position = Some (Data_item { full_qn;
                                                               def_loc }) } }
  and on_proc_name qn ({ elt; context } as acc) =
    let element_at_position = match context with
      | Data_decls -> Some (Proc_name { qn; in_section = None })   (* unlikely *)
      | Procedure in_section -> Some (Proc_name { qn; in_section })
    in
    { acc with elt = { elt with element_at_position } }
  in

  Cobol_unit.Visitor.fold_unit_group object
    inherit [acc] Cobol_unit.Visitor.folder
    inherit! [acc] Lsp_position.sieve ~filename ~pos

    method! fold_cobol_unit' cu ({ elt; _ } as acc) =
      let name = ~&(~&cu.unit_name) in
      Visitor.do_children @@
      { acc with
        elt = { elt with enclosing_compilation_unit_name = Some name } }

    method! fold_data_definitions _ = enter_context Data_decls
    method! fold_procedure _ = enter_context (Procedure None)
    method! fold_procedure_section s = enter_context (Procedure (Some s))

    method! fold_field_definition' def acc =
      Visitor.do_children @@ match ~&def.field_qualname with
      | None -> on_data_item ~@def acc                               (* FILLER *)
      | Some qn -> on_data_item ~full_qn:~&qn ~@def acc

    method! fold_record_renaming' ren acc =
      Visitor.do_children @@
      on_data_item ~full_qn:~&(~&ren.renaming_name) ~@ren acc

    method! fold_qualname qn acc =
      let qn = qualname_at_pos ~filename qn pos in
      Visitor.skip_children @@ match acc.context with
      | Data_decls ->            (* always fully qualified in data definitions *)
          on_data_full_name qn acc
      | Procedure _ ->            (* for now, no more info, data-name expected *)
          on_data_name qn acc

    method! fold_procedure_name qn acc =
      Visitor.skip_children @@
      on_proc_name (qualname_at_pos ~filename qn pos) acc

    method! fold_exec_block' exec_block acc =
      let acc = match exec_block.payload with
        | Superbol_preprocs.Generic.Generic_exec_block _ ->
          acc
        | Superbol_preprocs.Esql.Esql_exec_block esql ->
          let cob_var_extractor_folder = object
            inherit [Sql_ast.cobolVarId list] Sql_ast.Visitor.folder
            method! fold_cobol_var_id cob_var acc =
              Cobol_common.Visitor.skip (cob_var::acc)
          end in
          let cobol_vars =
            Sql_ast.Visitor.fold_esql_instruction cob_var_extractor_folder
              esql []
          in
          let string_name_opt = List.find_opt begin fun cobol_var_id ->
              Lsp_position.is_in_srcloc ~filename pos ~@cobol_var_id
            end cobol_vars
          in
          let acc = match string_name_opt with
            | Some name -> on_data_name (Name name) acc
            | None -> acc
          in acc
        | _ -> acc
      in
      Visitor.skip_children acc

  end group init |> result

(* --- *)

(* Using Lsp_position.sieve would be overkill for nodes that are close to the AST root, like COBOL units *)
let cobol_unit_at_pos ~filename pos group =
  Cobol_unit.Visitor.fold_unit_group object
    inherit [_] Cobol_unit.Visitor.folder
    method! fold_cobol_unit' cu acc =
      if Lsp_position.is_in_srcloc ~filename pos ~@cu
      then Visitor.skip_children @@ Some ~&cu
      else Visitor.skip_children acc
  end group None

let last_cobol_unit_before_pos ~filename pos group =
  Cobol_unit.Visitor.fold_unit_group object
    inherit [_] Cobol_unit.Visitor.folder
    method! fold_cobol_unit' cu acc =
      let is_in_or_after =
        try
          let lexloc = Cobol_common.Srcloc.lexloc_in ~filename ~@cu in
          Lsp_position.is_in_lexloc pos lexloc ||
          Lsp_position.is_after_lexloc pos lexloc
        with Invalid_argument _ -> false
      in
      if is_in_or_after
      then Visitor.skip_children @@ Some ~&cu
      else Visitor.skip_children acc
  end group None

let copy_at_pos ~filename pos ptree =
  Cobol_ptree.Visitor.fold_compilation_group object
    inherit [copy_operation option] Cobol_ptree.Visitor.folder
    method! fold' { loc; _ } = function
      | Some _ as acc ->
          Visitor.skip_children acc
      | None ->
          match Cobol_common.Srcloc.as_copy loc with
          | Some { loc; _ } as copy
            when Lsp_position.is_in_srcloc ~filename pos loc ->
              Visitor.skip_children copy
          | _ ->
              Visitor.do_children None
  end ptree None

let type_at_pos ~filename (pos: Lsp.Types.Position.t) group : approx_typing_info list =
  let open Cobol_common.Visitor in
  let open Cobol_ptree.Visitor in
  let open struct
    type acc = {
      context: approx_typing_info list;
      value: approx_typing_info list;
      after_pos: bool;
    }
    let init = { context = [Any]; value = [Any]; after_pos = false }
    let result acc = acc.value

    let set context f acc =
      if acc.after_pos
      then acc
      else { (f { acc with context }) with context = acc.context }

    let (@>>@) ctx fold =
      set ctx fold

    let (@>@) ctx fold =
      [ctx] @>>@ fold
  end in
  Cobol_unit.Visitor.fold_unit_group
    object (v)
      inherit [acc] Cobol_unit.Visitor.folder

      method! fold_cobol_unit cu acc =
        acc
        |> Cobol_unit.Visitor.fold_procedure v cu.unit_procedure
        |> skip

      method! fold' { loc; _ } acc =
        if acc.after_pos
        then skip acc
        else
          try
            let start = Cobol_common.Srcloc.start_pos_in ~filename loc in
            let start = start.pos_lnum - 1, start.pos_cnum - start.pos_bol in
            let pos = (pos.line, pos.character) in
            if start <= pos
            then do_children { acc with value = acc.context }
            else skip { acc with after_pos = true }
          with Invalid_argument _ ->
            skip acc

      (* add / subtract *)
      method! fold_basic_arithmetic_operands o acc =
        begin match o with
          | ArithSimple { sources; targets } ->
            acc
            |> Numeric @>@ fold_list ~fold:fold_ident_or_numlit v sources
            |> Numeric @>@ fold_rounded_idents v targets
          | ArithGiving { sources; to_or_from_item; targets } ->
            acc
            |> Numeric @>@ fold_list ~fold:fold_ident_or_numlit v sources
            |> Numeric @>@ fold_ident_or_numlit v to_or_from_item
            |> [Numeric; NumericEdited] @>>@ fold_rounded_idents v targets
          | ArithCorresponding { source; target } ->
            acc
            |> Group   @>@ fold_qualname v source
            |> Group   @>@ fold_rounded_ident v target
        end
        |> skip

      method! fold_allocate_kind k acc =
        begin match k with
          | AllocateDataItem n ->
            acc
            |> Any (* linkage level 01 or 77 *) @>@ fold_name' v n
          | AllocateCharacters e ->
            acc |> Any @>@ fold_expression v e
        end
        |> skip

      method! fold_allocate' { payload = al; _ } acc =
        acc
        |> fold_allocate_kind v al.allocate_kind
        |> Pointer @>@ fold_ident'_opt v al.allocate_returning
        |> skip

      method! fold_call_prefix p acc =
        begin match p with
          | CallGeneral i ->
            acc
            |> Alphanum (* +procedure_pointer *) @>@ fold_ident_or_strlit v i
          | CallProto { called; prototype } ->
            acc
            |> fold_option ~fold:fold_ident_or_strlit v called
            |> fold_call_proto v prototype
        end
        |> skip

      method! fold_cancel' { payload = c; _ } acc =
        acc
        |> Alphanum @>@ fold_list ~fold:fold_ident_or_strlit v c
        |> skip

      method! fold_compute' { payload = c; _ } acc =
        acc
        |> [Numeric; NumericEdited; Boolean]
        @>>@ fold_rounded_idents v c.compute_targets
        |> Any @>@ fold_expr v c.compute_expr
        |> fold_dual_handler v c.compute_on_size_error
        |> skip

      method! fold_divide_operands dop acc =
        begin match dop with
          | DivideInto i ->
            acc
            |> Numeric @>@ fold_scalar v i.divisor
            |> Numeric @>@ fold_rounded_idents v i.dividends
          | DivideGiving g ->
            acc
            |> Numeric @>@ fold_scalar v g.divisor
            |> Numeric @>@ fold_scalar v g.dividend
            |> [Numeric; NumericEdited] @>>@ fold_rounded_idents v g.giving
            |> [Numeric; NumericEdited] @>>@ fold_option ~fold:fold_ident v g.remainder
        end |> skip

      method! fold_entry_by_clause clause acc =
        begin match clause with
          | EntryByReference l ->
            acc |> Any (* linkage lvl 01 77*) @>@ fold_name'_list v l
          | EntryByValue l ->
            acc |> fold_name'_list v l
        end
        |> skip

      method! fold_free' { payload = f; _ } acc =
        acc
        |> Pointer @>@ fold_list ~fold:fold_name' v f
        |> skip

      method! fold_goto' { payload = g; _ } acc =
        begin match g with
          | GoToSimple _ -> acc
          | GoToDepending { depending_on; _ } ->
            acc
            |> Numeric (* int *) @>@ fold_ident v depending_on
          | GoToEntry { depending_on; _ } ->
            acc
            |> Numeric (* int *) @>@ fold_option ~fold:fold_ident v depending_on
        end
        |> skip

      method! fold_inspect' { payload = i; _ } acc =
        acc
        |> (* usage display *) fold_ident v i.inspect_item
        |> fold_inspect_spec v i.inspect_spec
        |> skip

      method! fold_tallying t acc =
        acc
        |> Numeric @>@ fold_qualident v t.tallying_target
        |> fold_list ~fold:fold_tallying_clause' v t.tallying_clauses
        |> skip

      method! fold_invoke' { payload = i; _ } acc =
        acc
        |> ObjectRef (* 4byte *) @>@ fold_ident v i.invoke_target
        |> Alphanum @>@ fold_ident_or_strlit v i.invoke_method
        |> fold_list ~fold:fold_call_using_clause' v i.invoke_using
        |> fold_ident'_opt v i.invoke_returning
        |> skip

      method! fold_move' { payload = m; _ } acc =
        begin match m with
          | MoveCorresponding { from; to_ } ->
            acc
            |> Group @>@ fold_ident v from
            |> Group @>@ fold_list ~fold:fold_ident v to_
          | _ -> acc
        end
        |> skip

      method! fold_multiply_operands mo acc =
        begin match mo with
          | MultiplyBy b ->
            acc
            |> Numeric @>@ fold_scalar v b.multiplier
            |> Numeric @>@ fold_rounded_idents v b.multiplicand
          | MultiplyGiving g ->
            acc
            |> Numeric @>@ fold_scalar v g.multiplier
            |> Numeric @>@ fold_scalar v g.multiplicand
            |> [Numeric; NumericEdited] @>>@ fold_rounded_idents v g.targets
        end
        |> skip

      method! fold_perform_mode pm acc =
        begin match pm with
          | PerformNTimes i ->
            acc |> Numeric @>@ fold_ident_or_intlit v i
          | PerformVarying pv ->
            acc
            |> fold_varying_phrase' v pv.varying
            |> fold_list ~fold:fold_varying_phrase' v pv.after
          | _ -> acc
        end
        |> skip

      method! fold_varying_phrase vp acc =
        acc
        |> Numeric @>@ fold_ident v vp.varying_ident
        |> Numeric @>@ fold_scalar v vp.varying_from
        |> Numeric @>@ fold_option ~fold:fold_scalar v vp.varying_by
        |> fold_condition v vp.varying_until
        |> skip

      method! fold_raise' { payload = r; _ } acc =
        begin match r with
          | RaiseIdent id -> acc |> ObjectRef @>@ fold_ident v id
          | _ -> acc
        end
        |> skip

      method! fold_search' { payload = s; _ } acc =
        acc
        |> fold_qualname v s.search_item
        |> fold_handler v s.search_at_end
        |> [Numeric; Index] @>>@ fold_option ~fold:fold_ident v s.search_varying
        |> fold_list ~fold:fold_search_when_clause' v s.search_when_clauses
        |> skip

      method! fold_string_stmt' { payload = s; _ } acc =
        acc
        |> fold_list ~fold:fold_string_source v s.string_sources
        |> Alphanum @>@ fold_ident v s.string_target
        |> fold_option ~fold:fold_ident v s.string_pointer
        |> fold_dual_handler v s.string_on_overflow
        |> skip

      method! fold_transform' { payload = t; _ } acc =
        acc
        |> (* technically should not be (numeric without display) *)
        fold_ident' v t.transform_ident
        |> Alphanum @>@fold' ~fold:fold_ident_or_nonnum v t.transform_from
        |> Alphanum @>@ fold' ~fold:fold_ident_or_nonnum v t.transform_to
        |> skip

      method! fold_unstring' { payload = u; _ } acc =
        acc
        |> Alphanum @>@ fold_ident v u.unstring_source
        |> Alphanum @>@ fold_list ~fold:fold_unstring_delimiter v u.unstring_delimiters
        |> (* usage display *) fold_list ~fold:fold_unstring_target v u.unstring_targets
        |> Numeric  @>@ fold_option ~fold:fold_ident v u.unstring_pointer
        |> Numeric  @>@ fold_option ~fold:fold_ident v u.unstring_tallying
        |> fold_dual_handler v u.unstring_on_overflow
        |> skip

      method! fold_unstring_target t acc =
        acc
        |> Any      @>@ fold_ident v t.unstring_target
        |> Alphanum @>@ fold_option ~fold:fold_ident v t.unstring_target_delimiter
        |> Numeric  @>@ fold_option ~fold:fold_ident v t.unstring_target_count
        |> skip

    end group init |> result

let proc_at_pos ~filename (pos: Lsp.Types.Position.t) group : procedure_at_position =
  let open Cobol_common.Visitor in
  Cobol_unit.Visitor.fold_unit_group object
    inherit [_] Cobol_unit.Visitor.folder
    inherit! [_] Lsp_position.sieve ~filename ~pos

    method! fold_cobol_unit cu acc =
      do_children { acc with cu = Some cu }

    method! fold_procedure_paragraph { paragraph_name; _ } { cu; _ } =
      let proc_name = match cu, paragraph_name with
        | Some cu, Some qn ->
          Some (Cobol_unit.Qualmap.find_binding
                  ~&qn cu.unit_procedure.named).full_qn
        | _ -> None
      in
      skip { cu; proc_name }

  end group { cu = None; proc_name = None }
