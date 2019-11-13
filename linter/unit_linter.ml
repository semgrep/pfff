open Common
open OUnit

module E = Error_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

let unittest ~ast_of_file =
 "linter" >::: [
  "basic checkers" >:: (fun () ->
  let p path = 
     Filename.concat Config_pfff.path 
            (Filename.concat "tests/GENERIC/scheck" path )
  in

  let test_files = [
    p "foo.py";

    p "cfg.py";

(*
    p "builtins.php";
    p "common.php";

    p "includes.php";

    p "variables.php";
    p "variables_fp.php";
    p "arrays.php";
    p "foreach.php";
    p "edit_distance.php";

    p "functions.php";
    p "static_methods.php";
    p "methods.php";

    p "classes.php";
    p "traits.php";
(*
    p "namespaces.php";
    p "namespaces_uses.php";
*)

    p "references.php";
    p "xhp.php";
    p "typing.php";

    p "dynamic_bailout.php";

    p "format_string.php";
    p "ternary_if.php";
    p "misc.php";

    p "lint.php";
    p "micro_clones.php";
*)
  ] 
  in

  (* expected *)
  let expected_error_lines = E.expected_error_lines_of_files test_files in

  (* actual *)
  E.g_errors := [];
  let verbose = false in
  (* really old:
   *  let db = Database_php_build.db_of_files_or_dirs files in
   *  let find_entity = Some (Database_php_build.build_entity_finder db) in
   *
   * old:
   *  let _files_for_codegraph = test_files in
   *  let (cg, _stat) = Graph_code_php.build ~verbose "/" files in
   *  let find_entity = 
   *   Some (Entity_php.entity_finder_of_graph_code ~check_dupes:true
             cg "/") in
   * let env = Env_php.mk_env ~php_root:"/" in
   *)
  let find_entity = None in
  (* run the bugs finders *)
  test_files |> List.iter (fun file ->
    let ast = ast_of_file file in
    Check_all_generic.check_file ~verbose ~find_entity ast;
  );

  (* compare *)
  let actual_errors = !E.g_errors in
  if verbose 
  then actual_errors |> List.iter (fun e -> pr (E.string_of_error e));
  E.compare_actual_to_expected actual_errors expected_error_lines
  )]
