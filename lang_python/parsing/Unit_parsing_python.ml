(*s: pfff/lang_python/parsing/Unit_parsing_python.ml *)
open Common

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

(*s: constant [[Unit_parsing_python.tests]] *)
let tests =
  Testutil.pack_tests "parsing_python" [

    "regression files", (fun () ->
      let dir = Config_pfff.tests_path "python/parsing" in
      let files = Common2.glob (spf "%s/*.py" dir)in
      files |> List.iter (fun file ->
        try
          let _ = Parse_python.parse_program file in
          ()
        with Parse_info.Parsing_error _ ->
          Alcotest.failf "it should correctly parse %s" file
      )
    );
  ]
(*e: constant [[Unit_parsing_python.tests]] *)
(*e: pfff/lang_python/parsing/Unit_parsing_python.ml *)
