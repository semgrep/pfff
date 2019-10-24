
(* the token list contains also the comment-tokens *)
type program_and_tokens = 
  Ast_python.program option * Parser_python.token list

exception Parse_error of Parse_info.info

(* This is the main function *)
val parse:
  Common.filename -> (program_and_tokens * Parse_info.parsing_stat)

val parse_program:
  Common.filename -> Ast_python.program

(* other parsers *)

(* for sgrep_js *)
val any_of_string:
  string -> Ast_python.any

(* for sgrep via fuzzy AST *)
(*
val parse_fuzzy:
  Common.filename -> Ast_fuzzy.trees * Parser_python.token list
*)

(* to help write test code *)
val program_of_string: string -> Ast_python.program


(* internal *)
val tokens: Common.filename -> Parser_python.token list

