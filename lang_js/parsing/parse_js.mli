
(* the token list contains also the comment-tokens *)
type program_and_tokens =
  Ast_js.program option * Parser_js.token list

(* This is the main function. It may raise
 *  - Parse_info.Parsing_error if Flag_parsing.error_recovery is false
 *  - Parse_info.Lexical_error if Flag_parsing.exn_when_lexical_error is true.
*)
val parse:
  ?timeout: int ->
  Common.filename -> (program_and_tokens * Parse_info.parsing_stat)

val parse_program:
  Common.filename -> Ast_js.program
val parse_string :
  string -> Ast_js.program

(* other parsers *)

(* for semgrep *)
val any_of_string:
  string -> Ast_js.any

(* to help write test code *)
val program_of_string: string -> Ast_js.program

(* internal *)
val tokens: Common.filename -> Parser_js.token list
