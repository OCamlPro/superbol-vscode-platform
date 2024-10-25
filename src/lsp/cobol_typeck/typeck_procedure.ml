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

open Cobol_unit.Types
open Cobol_common.Srcloc.TYPES
open Cobol_common.Srcloc.INFIX

module Qualmap = Cobol_unit.Qualmap
module Visitor = Cobol_common.Visitor

type output =
  {
    procedure: Cobol_unit.Types.procedure;
    references: Typeck_outputs.references_in_unit;
  }

let procedure_of_compilation_unit cu' =

  let open struct
    type acc =
      {
        blocks: procedure_block Qualmap.t;
        block_list: procedure_block list;
        current_section: section_under_construction with_loc option;
      }
    and section_under_construction =
      {
        sec_name: Cobol_ptree.procedure_name with_loc;
        sec_paragraphs: procedure_paragraph with_loc list;
      }

    let init =
      {
        blocks = Qualmap.empty;
        block_list = [];
        current_section = None;
      }

    let procedure acc =
      {
        named = acc.blocks;
        list = List.rev acc.block_list;
      }

    let name n : Cobol_ptree.qualname = Name n
    let qual n q : Cobol_ptree.qualname = Qual (n, q)

    let section_block
        ({ payload = suc; loc }: section_under_construction with_loc) =
      let section_paragraphs =
        List.fold_left begin fun ({ named; list } as paragraphs) paragraph ->
          match ~&paragraph.paragraph_name with
          | None ->
              { paragraphs with list = paragraph :: list }
          | Some qn ->
              { named = Qualmap.add ~&qn paragraph named;
                list = paragraph :: list }
        end { named = Qualmap.empty; list = [] } suc.sec_paragraphs
      in
      Section ({ section_name = suc.sec_name; section_paragraphs } &@ loc)

    let simple_paragraph (p: Cobol_ptree.paragraph with_loc) acc =
      match acc.current_section, ~&p.paragraph_name with
      | None, Some n ->
          { paragraph_name = Some (name n &@<- n);
            paragraph = p } &@<- p
      | Some { payload = { sec_name = qn; _ }; _ }, Some n ->
          { paragraph_name = Some (qual n ~&qn &@<- n);
            paragraph = p } &@<- p
      | _, None ->
          { paragraph_name = None;
            paragraph = p } &@<- p

    let paragraph_block p =
      Paragraph p

    let commit_section acc =
      match acc.current_section with
      | Some ({ payload = { sec_name = qn; _ }; _ } as section) ->
          let section = section_block section in
          { blocks = Qualmap.add ~&qn section acc.blocks;
            block_list = section :: acc.block_list;
            current_section = None }
      | None ->
          acc

    let start_new_section n s acc =
      let acc = commit_section acc in
      let sec_paragraphs = [simple_paragraph s acc] in
      { acc with
        current_section = Some ({ sec_name = name n &@<- n;
                                  sec_paragraphs } &@<- s) }

    let named_paragraph n p acc =
      let p = simple_paragraph p acc in
      let qn, block_list, current_section =
        match acc.current_section with
        | None ->
            name n,
            paragraph_block p :: acc.block_list,
            None
        | Some s ->
            let loc = Cobol_common.Srcloc.concat ~@s ~@p in
            let sec_paragraphs = p :: ~&s.sec_paragraphs in
            qual n ~&(~&s.sec_name),
            acc.block_list,
            Some ({ ~&s with sec_paragraphs } &@ loc)
      in
      { blocks = Qualmap.add qn (paragraph_block p) acc.blocks;
        block_list;
        current_section }

    let anonymous_paragraph p acc =         (* only at beginning of procedure *)
      { acc with
        block_list = paragraph_block (simple_paragraph p acc) :: acc.block_list }

  end in

  let visitor = object
    inherit [acc] Cobol_ptree.Visitor.folder

    method! fold_informational_paragraphs _ = Visitor.skip
    method! fold_options_paragraph' _ = Visitor.skip
    method! fold_environment_division _ = Visitor.skip
    method! fold_data_division' _ = Visitor.skip
    method! fold_procedure_division _ acc =
      Visitor.do_children_and_then acc
        commit_section                  (* be sure to handle the last section *)

    method! fold_paragraph' p acc =
      Visitor.skip_children @@ match ~&p with
      | { paragraph_is_section = true;
          paragraph_name = Some name; _ } -> start_new_section name p acc
      | { paragraph_name = Some name; _ } -> named_paragraph name p acc
      | { paragraph_name = None; _ }      -> anonymous_paragraph p acc

    (* TODO: nested programs... *)
    method! fold_nested_programs _  = Visitor.skip

  end in

  Cobol_ptree.Visitor.fold_compilation_unit' visitor
    cu' init |> procedure

(* --- *)

let references ~(data_definitions: Cobol_unit.Types.data_definitions) procedure =

  let open struct
    (* TODO: add a context, and gather references to procedures, etc. *)
    type acc =
      {
        current_section: Cobol_unit.Types.procedure_section option;
        refs: Typeck_outputs.references_in_unit;
        diags: Typeck_diagnostics.t;
      }
    let init =
      {
        current_section = None;
        refs = Typeck_outputs.no_refs;
        diags = Typeck_diagnostics.none;
      }
    let references { refs; diags; _ } = refs, diags
  end in

  let baseloc_of_qualname: Cobol_ptree.qualname -> srcloc = function
    | Name name
    | Qual (name, _) -> ~@name
  in

  let error acc err =
    { acc with diags = Proc_error err :: acc.diags }
  in

  let visitor = object (v)
    inherit [acc] Cobol_unit.Visitor.folder

    method! fold_qualname qn acc =                (* TODO: data_name' instead *)
      let loc = baseloc_of_qualname qn in
      Visitor.do_children @@
      (* match Qualmap.find qn data_definitions.data_items.named with *)
      (* | Data_field { def; _ } -> *)
      (*     { acc with *)
      (*       refs = Typeck_outputs.register_data_field_ref ~loc def acc.refs } *)
      (* | Data_renaming { def; _ } -> *)
      (*     { acc with *)
      (*       refs = Typeck_outputs.register_data_renaming_ref ~loc def acc.refs } *)
      (* | Data_condition { def; _ } -> *)
      (*     { acc with *)
      (*       refs = Typeck_outputs.register_condition_name_ref ~loc def acc.refs } *)
      (* | Table_index { qualname = qn; _ } -> *)
      (*     { acc with *)
      (*       refs = Typeck_outputs.register_data_qualref ~loc ~&qn acc.refs } *)
      try
        let bnd = Qualmap.find_binding qn data_definitions.data_items.named in
        { acc with
          refs = Typeck_outputs.register_data_qualref ~loc bnd.full_qn acc.refs }
      with
      | Not_found ->
          acc  (* ignored for now, as we don't process all the DATA DIV. yet. *)
      | Qualmap.Ambiguous (lazy matching_qualnames) ->
          error acc @@ Ambiguous_data_name { given_qualname = qn &@ loc;
                                             matching_qualnames }

    method! fold_procedure_section ({ section_paragraphs; _ } as s)
        ({ current_section; _ } as acc) =
      let acc =
        Visitor.fold_list v section_paragraphs.list
          ~fold:Cobol_unit.Visitor.fold_procedure_paragraph'
          { acc with current_section = Some s }
      in
      Visitor.skip { acc with current_section }

    method! fold_procedure_paragraph { paragraph; _ } acc =
      Visitor.skip @@
      Cobol_ptree.Proc_division_visitor.fold_paragraph' v paragraph acc


    method! fold_procedure_name' qn
        ({ current_section = in_section; _ } as acc) =
      let register ?in_section qn acc =
        let loc = baseloc_of_qualname ~&qn in
        match Cobol_unit.Procedure.find ~&qn ?in_section procedure with
        | block ->
          { acc with
            refs = Typeck_outputs.register_procedure_ref ~loc block acc.refs }
        | exception Not_found ->
          error acc @@ Unknown_proc_name qn
        | exception Qualmap.Ambiguous (lazy matching_qualnames) ->
          error acc @@ Ambiguous_proc_name { given_qualname = qn;
                                             matching_qualnames }
      in
      let acc = register ?in_section qn acc in
      let acc = match ~&qn with
        | Name _ -> acc
        | Qual (_, section_qn) ->
          let loc = baseloc_of_qualname section_qn in
          register (section_qn &@ loc) acc
      in
      Visitor.skip_children acc

    method! fold_exec_block' exec_block acc =
      let register_name name acc =
        let qn = Cobol_ptree.Name name in
        let loc = name.loc in
        begin try
            let bnd = Qualmap.find_binding qn data_definitions.data_items.named in
            { acc with
              refs = Typeck_outputs.register_data_qualref
                  ~loc bnd.full_qn acc.refs }
          with
          | Not_found ->
            acc  (* ignored for now, as we don't process all the DATA DIV. yet. *)
          | Qualmap.Ambiguous (lazy matching_qualnames) ->
            error acc @@ Ambiguous_data_name { given_qualname = qn &@ loc;
                                               matching_qualnames }
        end in
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
          List.fold_left begin fun acc cobol_var_id ->
              register_name cobol_var_id acc
          end acc cobol_vars
        | _ -> acc
      in
      Visitor.skip_children acc

  end in

  Cobol_unit.Visitor.fold_procedure visitor procedure init |> references

(* --- *)

let of_compilation_unit ~data_definitions cu' =
  let procedure = procedure_of_compilation_unit cu' in
  let references, diags = references ~data_definitions procedure in
  {
    procedure;
    references;
  },
  diags
