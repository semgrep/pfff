open Common


(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

let tests =
  Testutil.pack_tests "parsing_scala" [

    "regression files", (fun () ->
      let dir = Config_pfff.tests_path "scala/parsing" in
      let files = Common2.glob (spf "%s/*.scala" dir) in
      files |> List.iter (fun file ->
        try
          let _ = Parse_scala.parse file in
          ()
        with exn ->
          Alcotest.failf "it should correctly parse %s (exn = %s)"
            file (Common.exn_to_s exn)
      )
    );
  ]
