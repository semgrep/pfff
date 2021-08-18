(*
   Test regexp parsing on individual files in /tests
*)

open OUnit

open Common

let test_valid_files dialect rel_path () =
  let dir = Config_pfff.tests_path rel_path in
  let files = Common2.glob (spf "%s/*.regexp" dir) in
  files |> List.iter (fun file ->
    try
      let _ = Parse.parse ~conf:dialect file in
      ()
    with exn ->
      assert_failure (spf "it should correctly parse %s (exn = %s)"
                        file (Common.exn_to_s exn))
  )

let test_invalid_files dialect rel_path () =
  let dir = Config_pfff.tests_path rel_path in
  let files = Common2.glob (spf "%s/*.regexp" dir) in
  files |> List.iter (fun file ->
    try
      let _ast = Parse.file ~conf:dialect file in
      assert_failure (spf "it should have thrown a Parse_error %s" file)
    with
    | Parse_info.Parsing_error _ -> ()
    | exn -> assert_failure (spf "throwing wrong exn %s on %s"
                               (Common.exn_to_s exn) file)
  )

let unittest =
  "regexp parsing" >::: [
    "pcre" >::: [
      "valid files" >::
      test_valid_files Dialect.pcre "regexp/pcre/parsing";
      "invalid files" >::
      test_invalid_files Dialect.pcre "regexp/pcre/parsing_errors";
    ];
    "pcre_extended" >::: [
      "valid files" >::
      test_valid_files Dialect.pcre_extended
        "regexp/pcre_extended/parsing";
      "invalid files" >::
      test_invalid_files Dialect.pcre_extended
        "regexp/pcre_extended/parsing_errors";
    ];
    "perl_xx" >::: [
      "valid files" >::
      test_valid_files Dialect.perl_xx "regexp/perl_xx/parsing";
      "invalid files" >::
      test_invalid_files Dialect.perl_xx "regexp/perl_xx/parsing_errors";
    ];
  ]
