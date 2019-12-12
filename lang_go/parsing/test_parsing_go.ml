open Common

module Flag = Flag_parsing

(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

let test_tokens_go file = 
  if not (file =~ ".*\\.go") 
  then pr2 "warning: seems not a Go file";

  Flag.verbose_lexing := true;
  Flag.verbose_parsing := true;
  Flag.exn_when_lexical_error := true;

  let toks = Parse_go.tokens file in
  toks |> List.iter (fun x -> pr2_gen x);
  ()

let test_parse_go xs =
  let xs = List.map Common.fullpath xs in

  let fullxs = 
    Lib_parsing_go.find_source_files_of_dir_or_files xs 
    |> Skip_code.filter_files_if_skip_list
  in

  let stat_list = ref [] in

  fullxs |> Console.progress (fun k -> List.iter (fun file ->
    k();

    let (_xs, stat) =
     Common.save_excursion Flag.error_recovery true (fun () ->
     Common.save_excursion Flag.exn_when_lexical_error false (fun () ->
       Parse_go.parse file
    )) in
    Common.push stat stat_list;
  ));
  Parse_info.print_parsing_stat_list !stat_list;
  ()


let test_dump_go file =
  let _ast = Parse_go.parse_program file in
(*
  let v = Meta_ast_python.vof_program ast in
  let s = Ocaml.string_of_v v in
  pr s
*)
  raise Todo

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () = [
  "-tokens_go", "   <file>", 
  Common.mk_action_1_arg test_tokens_go;
  "-parse_go", "   <files or dirs>", 
  Common.mk_action_n_arg test_parse_go;
  "-dump_go", "   <file>", 
  Common.mk_action_1_arg test_dump_go;
]

