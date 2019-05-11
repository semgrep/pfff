open Common

(*****************************************************************************)
(* Simple AST *)
(*****************************************************************************)
let test_parse_simple xs =
  let fullxs = Lib_parsing_js.find_source_files_of_dir_or_files xs in
  fullxs +> List.iter (fun file ->
    try 
      let cst = Parse_js.parse_program file in
      let _ast = Ast_js_build.program cst in
      ()
    with exn ->
      (match exn with
      | Ast_js_build.TodoConstruct (s, tok)
      | Ast_js_build.UnhandledConstruct (s, tok)
        -> 
        pr2 s;
        pr2 (Parse_info.error_message_info tok);

      | _ -> raise exn
      )
  )


(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () = [
  "-parse_js_simple", "   <files or dirs>",
  Common.mk_action_n_arg test_parse_simple;
]
