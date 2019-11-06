open Common

let test_parse_generic xs =
  let xs = List.map Common.fullpath xs in
  let files = Common.files_of_dir_or_files_no_vcs_nofilter xs in
  files |> List.iter (fun file ->
    match Lang.lang_of_filename_opt file with
    | None -> pr2 (spf "skipping %s" file)
    | Some _ -> 
          let _ast = Parse_generic.parse_program file in
          ()
  )

let test_dump_generic file =
  let ast = Parse_generic.parse_program file in
  let v = Meta_ast.vof_any (Ast_generic.Pr ast) in
  let s = Ocaml.string_of_v v in
  pr2 s


let actions () = [
  "-parse_generic", " <dirs_or_files>",
  Common.mk_action_n_arg test_parse_generic;
  "-dump_generic", " <file>",
  Common.mk_action_1_arg test_dump_generic;
]
