open Common

module Flag = Flag_parsing

(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

let test_tokens file = 
  if not (file =~ ".*\\.rb") 
  then pr2 "warning: seems not a ruby file";

  Flag.verbose_lexing := true;
  Flag.verbose_parsing := true;
  Flag.exn_when_lexical_error := true;

  raise Todo

let test_parse xs =
  let xs = List.map Common.fullpath xs in

  let fullxs = xs
(*
    Lib_parsing_python.find_source_files_of_dir_or_files xs 
    |> Skip_code.filter_files_if_skip_list ~root:xs
*)
  in

  let stat_list = ref [] in

  fullxs |> Console.progress (fun k -> List.iter (fun file ->
    k();

    let (_xs, stat) =
     Common.save_excursion Flag.error_recovery true (fun () ->
     Common.save_excursion Flag.exn_when_lexical_error false (fun () ->
       let ast =  Parse_ruby.parse_program file in
       ast, Parse_info.default_stat file
    )) in
    Common.push stat stat_list;
  ));
  Parse_info.print_parsing_stat_list !stat_list;
  ()


let test_dump file =
  let ast = Parse_ruby.parse_program file in
  let v = Meta_ast_ruby.vof_ast ast in
  let s = Ocaml.string_of_v v in
  pr s

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () = [
  "-tokens_ruby", "   <file>", 
  Common.mk_action_1_arg test_tokens;
  "-parse_ruby", "   <files or dirs>", 
  Common.mk_action_n_arg test_parse;
  "-dump_ruby", "   <file>", 
  Common.mk_action_1_arg test_dump;
]
