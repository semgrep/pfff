open Common

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

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
