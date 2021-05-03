open Common

open OUnit

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

let unittest =
  "parsing_scala" >::: [

    "regression files" >:: (fun () ->
      let dir = Config_pfff.tests_path "scala/parsing" in
      let files = Common2.glob (spf "%s/*.scala" dir) in
      files |> List.iter (fun file ->
        try
          let _ = Parse_scala.parse file in
          ()
        with exn ->
          assert_failure (spf "it should correctly parse %s (exn = %s)"
                            file (Common.exn_to_s exn))
      )
    );
  ]
