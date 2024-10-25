(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2021-2023 OCamlPro SAS                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the                       *)
(*  OCAMLPRO-NON-COMMERCIAL license.                                      *)
(*                                                                        *)
(**************************************************************************)

(** `parse` sql command *)

open Ezcmd.V2
open EZCMD.TYPES
open Common_args

let typeck_file { preproc_options; parser_options } filename =
  Cobol_preproc.Input.from ~filename
    ~f:
      begin
        fun input ->
          input
          |> Cobol_preproc.preprocessor ~options:preproc_options
          |> Cobol_parser.parse_simple ~options:parser_options
          |> Cobol_parser.Outputs.result_only
          |> (* ignoe diagnostics *)
          Cobol_typeck.compilation_group ~config:parser_options.config
          |> Cobol_typeck.Results.result_only
          |> fun checked_group ->
          (* in group: *)
          let cu' = Cobol_unit.Collections.SET.choose checked_group.group in
          let cu = cu'.payload in
          cu
        (* let x_info =
             (* May raise Not_found | Cobol_unit.Qualmap.Ambiguous _ *)
             Cobol_unit.Qualmap.find
               (Cobol_unit.Qual.name ( Cobol_common.Srcloc.flagit "VBFLD" Cobol_common.Srcloc.dummy))
               cu.unit_data.data_items.named
           in
           match x_info with
           | Data_field { def = { payload = { field_layout; field_size; _ } ; _ } ; _ } ->
               Pretty.out "Size of VBFLD is %u Bytes@."
                 Cobol_data.Memory.(as_bits field_size / 8);
               begin match field_layout with
                 | Elementary_field { usage = Display picture; _ } ->
                     Pretty.out "PIC is %a@."
                       Cobol_data.Picture.pp picture;
                     (match picture.category with
                      | FixedNum { digits = _; scale = _; with_sign = _; _ } -> ()
                      | _ -> ())
                 | Elementary_field _
                 | Struct_field _ ->
                     ()
               end
           | _ -> ()
        *)
      end

let parse ~sql_in_copybooks ~copy_exts ~test_extension common files =
  let source_format = common.preproc_options.source_format in
  let copy_path = common.preproc_options.copybook_lookup_config.lookup_path in
  let source_format = Cobol_indent.Config.source_format source_format in
  List.iter
    (fun filename ->
       let common, _ = Common_args.get () in
       let cobol_unit = typeck_file (common ()) filename in
       (*TODO appel a typeck ici*)
       let contents =
         Sql_preproc.Main.preproc ~sql_in_copybooks ~copy_path ~copy_exts
           ~filename ~source_format () ~cobol_unit
       in
       let output_file filename s =
         match filename with
         | "-" ->
           Printf.printf "%s\n%!" s
         | _ ->
           let oc = open_out filename in
           output_string oc s;
           close_out oc;
           Printf.eprintf "File %S generated\n%!" filename
       in
       let new_filename =
         let extension = Filename.extension filename in
         if test_extension
         then
           let base_name = Filename.chop_suffix filename extension in
           base_name ^ ".cbsql"
         else if String.equal extension ".cob" || String.equal extension ".cbl"
         then
           let base_name = Filename.chop_suffix filename extension in
           base_name ^ ".pp" ^ extension
         else
           filename
       in
       output_file new_filename contents)
    files

let preproc_cmd =
  let sql_in_copybooks = ref false in
  let test_extension = ref false in
  let copy_exts = ref [] in
  let files = ref [] in
  let common, common_args = Common_args.get () in
  EZCMD.sub "sql preproc"
    (fun () ->
      let common = common () in
      Printexc.record_backtrace true;
      parse ~sql_in_copybooks:!sql_in_copybooks ~copy_exts:!copy_exts common
      ~test_extension:!test_extension !files )
    ~args:
      ( common_args
      @ [ ( [],
            Arg.Anons (fun l -> files := l),
            EZCMD.info ~docv:"FILE" "COBOL files to preproc" );
          ( [ "copybooks" ],
            Arg.Set sql_in_copybooks,
            EZCMD.info "Preprocess copybooks also (without REPLACING)" );
          ( [ "test-ext" ],
            Arg.Set test_extension,
            EZCMD.info "Set file extension to .cbsql" );
          (* I (@NeoKaios) removed that as it conflicts with another option *)
          (* ( [ "ext" ], *)
          (*   Arg.String (fun s -> copy_exts := !copy_exts @ [ "." ^ s ]), *)
          (*   EZCMD.info ~docv:"EXT" *)
          (*     "Add .EXT as an extension to find copybooks (default is cpy)" ) *)
        ] )
    ~doc:"Preprocess SQL EXECs"
