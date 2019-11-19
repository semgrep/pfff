open Common

module Flag = Flag_parsing
module E = Error_code

(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

let test_tokens_python file = 
  if not (file =~ ".*\\.py") 
  then pr2 "warning: seems not a python file";

  Flag.verbose_lexing := true;
  Flag.verbose_parsing := true;
  Flag.exn_when_lexical_error := true;

  let toks = Parse_python.tokens file in
  toks |> List.iter (fun x -> pr2_gen x);
  ()

let test_parse_python xs =
  let xs = List.map Common.fullpath xs in

  let fullxs = 
    Lib_parsing_python.find_source_files_of_dir_or_files xs 
    |> Skip_code.filter_files_if_skip_list
  in
  fullxs |> Console.progress (fun k -> List.iter (fun file -> 
    k();
    Common.save_excursion Flag.error_recovery true (fun () ->
    E.try_analyze_file_with_exn_to_errors file (fun () ->
      Parse_python.parse file |> ignore
    );
    !E.g_errors |> List.iter (fun err -> pr (E.string_of_error err));
    E.g_errors := []
   )
  ))


let test_dump_python file =
  let ast = Parse_python.parse_program file in
  let v = Meta_ast_python.vof_program ast in
  let s = Ocaml.string_of_v v in
  pr s

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () = [
  "-tokens_python", "   <file>", 
  Common.mk_action_1_arg test_tokens_python;
  "-parse_python", "   <files or dirs>", 
  Common.mk_action_n_arg test_parse_python;
  "-dump_python", "   <file>", 
  Common.mk_action_1_arg test_dump_python;
]
