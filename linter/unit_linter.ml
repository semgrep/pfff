open Common
open OUnit

module E = Error_code
module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let (expected_errors_of_files: 
  Common.filename list -> (Common.filename * int (* line *)) list) =
 fun test_files ->
  test_files |> List.map (fun file ->
    Common.cat file |> Common.index_list_1 |> Common.map_filter 
      (fun (s, idx) -> 
        (* Right now we don't care about the actual error messages. We
         * don't check if they match. We are just happy to check for 
         * correct lines error reporting.
         *)
        if s =~ ".*#ERROR:.*" 
        (* + 1 because the comment is one line before *)
        then Some (file, idx + 1) 
        else None
      )
  ) |> List.flatten

let compare_actual_to_expected actual_errors expected_errors =
  (* diff report *)
  let (_common, only_in_expected, only_in_actual) = 
    Common2.diff_set_eff expected_errors actual_errors in

  only_in_expected |> List.iter (fun (src, l) ->
    pr2 (spf "this one error is missing: %s:%d" src l);
  );
  only_in_actual |> List.iter (fun (src, l) ->
    pr2 (spf "this one error was not expected: %s:%d (%s)" src l
           (!E.g_errors |> List.find (fun err ->
             let loc = err.E.loc in
             src =$= loc.PI.file &&
             l   =|= loc.PI.line
            ) |> E.string_of_error));
  );
  assert_bool
    ~msg:(spf "it should find all reported errors and no more (%d errors)"
             (List.length (only_in_actual @ only_in_expected)))
    (null only_in_expected && null only_in_actual)
  

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

  let _files_for_codegraph = test_files in

  let expected_errors = expected_errors_of_files test_files in
  E.g_errors := [];

  let verbose = false in

  (* really old:
   *  let db = Database_php_build.db_of_files_or_dirs files in
   *  let find_entity = Some (Database_php_build.build_entity_finder db) in
   *
   * old:
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
  if verbose 
  then !E.g_errors |> List.iter (fun e -> pr (E.string_of_error e));
  
  let actual_errors = 
    !E.g_errors |> List.map (fun err ->
      let loc = err.E.loc in
      loc.PI.file, loc.PI.line
      )
  in
  compare_actual_to_expected actual_errors expected_errors
  )]
