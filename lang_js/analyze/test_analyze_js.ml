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

let test_dump_ast file =
  let cst = Parse_js.parse_program file in
  let ast = Ast_js_build.program cst in
  let v = Meta_ast_js.vof_program ast in
  let s = Ocaml.string_of_v v in
  pr s

(*****************************************************************************)
(* Abstract interpretation *)
(*****************************************************************************)

let ai_js dir file =
  (* copy paste of main_codegraph_build.ml *)
  let root = Common.fullpath dir in
  let files = 
   Lib_parsing_js.find_source_files_of_dir_or_files 
      ~include_scripts:false [root]
  in
  let skip_file = Filename.concat root "skip_list.txt" in
  let skip_list =
    if Sys.file_exists skip_file
    then begin 
      pr2 (spf "Using skip file: %s (for lang = JS)" skip_file);
      Skip_code.load skip_file;
    end
    else []
  in
  let files = Skip_code.filter_files skip_list root files in
  let (db, annotated_asts) = 
     Graph_code_js.build_for_ai root files in
  (* end copy paste *)
  
  (* to debug: *)
  if false then begin
  annotated_asts |> List.iter (fun (_file, ast)  ->
    let v = Meta_ast_js.vof_program ast in
    let s = Ocaml.string_of_v v in
    pr s
  );
  end;


  let ast = 
    try List.assoc file annotated_asts
    with Not_found ->
      failwith (spf "'%s' is not in the list of annotated ASTs for root '%s'"
                      file dir)
  in

  let env = Abstract_interpreter_js_env.empty_env db file in
  let heap = Abstract_interpreter_js_env.empty_heap in
  let _heap = 
    (* let's go! *)
    Abstract_interpreter_js.program env heap ast 
  in
  ()


let test_ai_js file = 
  let cst = Parse_js.parse_program file in
  let ast = Ast_js_build.program cst in
  let db = Hashtbl.create 0 in
  let env = Abstract_interpreter_js_env.empty_env db file in
  let heap = Abstract_interpreter_js_env.empty_heap in
  let _heap = 
    (* let's go! *)
    Abstract_interpreter_js.program env heap ast 
  in
  ()

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () = [
  "-parse_js_simple", "   <files or dirs>",
  Common.mk_action_n_arg test_parse_simple;
  "-dump_ast_js", "   <file>",
  Common.mk_action_1_arg test_dump_ast;
  "-ai_js", "   <dir> <mainfile>",
  Common.mk_action_2_arg ai_js;
  "-test_ai_js", "   <file>",
  Common.mk_action_1_arg test_ai_js;
]
