(*
   Test regexp parsing on individual files in /tests
*)

open OUnit

open Common

let unittest =
  "regexp parsing" >::: [

    "regression files" >:: (fun () ->
      Sys.command "pwd" |> ignore;
      let dir = Config_pfff.tests_path "regexp/parsing" in
      let files = Common2.glob (spf "%s/*.regexp" dir) in
      files |> List.iter (fun file ->
        try
          let _ = Parse.parse file in
          ()
        with exn ->
          assert_failure (spf "it should correctly parse %s (exn = %s)"
                            file (Common.exn_to_s exn))
      )
    );

    "rejecting bad code" >:: (fun () ->
      let dir = Config_pfff.tests_path "regexp/parsing_errors" in
      let files = Common2.glob (spf "%s/*.regexp" dir) in
      files |> List.iter (fun file ->
        try
          let _ast = Parse.file file in
          assert_failure (spf "it should have thrown a Parse_error %s" file)
        with
        | Parse_info.Parsing_error _ -> ()
        | exn -> assert_failure (spf "throwing wrong exn %s on %s"
                                   (Common.exn_to_s exn) file)
      )
    );
  ]
