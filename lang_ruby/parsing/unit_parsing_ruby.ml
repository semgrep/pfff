open Common
open OUnit

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

let unittest =
  "parsing_ruby" >::: [

    "regression files" >:: (fun () ->
      let dir = Config_pfff.tests_path "ruby/parsing" in
      let files = Common2.glob (spf "%s/*.rb" dir)in
      files |> List.iter (fun file ->
        try
          let _ = Parse_ruby.parse_program file in
          ()
        with Parse_info.Parsing_error _ ->
          assert_failure (spf "it should correctly parse %s" file)
      )
    );
  ]
