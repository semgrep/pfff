

(* 

(*****************************************************************************)
(* Fuzzy parsing *)
(*****************************************************************************)

PHP

(* for generalized sgrep/spatch patterns *)
val parse_fuzzy:
  Common.filename -> Ast_fuzzy.tree list * Parser_php.token list


let parse_fuzzy file =
  let toks = tokens file in
  let trees = Parse_fuzzy.mk_trees { Parse_fuzzy.
     tokf = TH.info_of_tok;
     kind = TH.token_kind_of_tok;
  } toks 
  in
  trees, toks
     parse_fuzzy.ml \

ML
(*****************************************************************************)
(* Fuzzy parsing *)
(*****************************************************************************)

(* This is similar to what I did for OPA. This is also similar
 * to what I do for parsing hacks for C++, but this fuzzy AST can be useful
 * on its own, e.g. for a not too bad sgrep/spatch.
 *)
let parse_fuzzy file =
  let toks = tokens file in
  let trees = Parse_fuzzy.mk_trees { Parse_fuzzy.
     tokf = TH.info_of_tok;
     kind = TH.token_kind_of_tok;
  } toks 
  in
  trees, toks




let test_parse_ml_fuzzy dir_or_file =
  let fullxs = 
    Lib_parsing_ml.find_source_files_of_dir_or_files [dir_or_file] 
    +> Skip_code.filter_files_if_skip_list
  in
  fullxs +> Console.progress (fun k -> List.iter (fun file -> 
     k ();
      try 
        let _fuzzy = Parse_ml.parse_fuzzy file in
        ()
      with _exn ->
        (* pr2 (spf "PB with: %s, exn = %s" file (Common.exn_to_s exn)); *)
        pr2 file;
  ));
  ()

let test_dump_ml_fuzzy file =
  let fuzzy, _toks = Parse_ml.parse_fuzzy file in
  let v = Ast_fuzzy.vof_trees fuzzy in
  let s = Ocaml.string_of_v v in
  pr2 s

  "-parse_ml_fuzzy", "   <file or dir>", 
  Common.mk_action_1_arg test_parse_ml_fuzzy;
  "-dump_ml_fuzzy", "   <file>", 
  Common.mk_action_1_arg test_dump_ml_fuzzy;


SKIP
(*****************************************************************************)
(* Fuzzy parsing *)
(*****************************************************************************)

val parse_fuzzy:
  Common.filename -> Ast_fuzzy.tree list * Parser_skip.token list

let parse_fuzzy file =
  let toks = tokens file in
  let trees = Parse_fuzzy.mk_trees { Parse_fuzzy.
     tokf = TH.info_of_tok;
     kind = TH.token_kind_of_tok;
  } toks 
  in
  trees, toks



let test_parse_fuzzy dir_or_file =
  let fullxs = 
    Lib_parsing_skip.find_source_files_of_dir_or_files [dir_or_file] 
    +> Skip_code.filter_files_if_skip_list
  in
  fullxs +> Console.progress (fun k -> List.iter (fun file -> 
     k ();
      try 
        let _fuzzy = Parse_skip.parse_fuzzy file in
        ()
      with _exn ->
        (* pr2 (spf "PB with: %s, exn = %s" file (Common.exn_to_s exn)); *)
        pr2 file;
  ));
  ()

let test_dump_fuzzy file =
  let fuzzy, _toks = Parse_skip.parse_fuzzy file in
  let v = Ast_fuzzy.vof_trees fuzzy in
  let s = Ocaml.string_of_v v in
  pr2 s


  "-parse_sk_fuzzy", "   <file or dir>", 
  Common.mk_action_1_arg test_parse_fuzzy;
  "-dump_sk_fuzzy", "   <file>", 
  Common.mk_action_1_arg test_dump_fuzzy;

JAVA

(*****************************************************************************)
(* Fuzzy parsing *)
(*****************************************************************************)

(* for generalized sgrep/spatch patterns *)
val parse_fuzzy:
  Common.filename -> Ast_fuzzy.tree list * Parser_java.token list

let parse_fuzzy file =
  let toks = tokens file in
  let trees = Parse_fuzzy.mk_trees { Parse_fuzzy.
     tokf = TH.info_of_tok;
     kind = TH.token_kind_of_tok;
  } toks
  in
  trees, toks


*)

