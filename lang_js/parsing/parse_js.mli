
(* the token list contains also the comment-tokens *)
type program_and_tokens = 
  Cst_js.program option * Parser_js.token list

exception Parse_error of Parse_info.info

(* This is the main function. It may raise 
 *  - Parse_error if Flag_parsing.error_recovery is false
 *  - Lexer_js.Lexical_error if Flag_parsing.exn_when_lexical_error is true.
 *)
val parse: 
  Common.filename -> (program_and_tokens * Parse_info.parsing_stat)

val parse_program:
  Common.filename -> Cst_js.program
val parse_string : 
  string -> Cst_js.program

(* other parsers *)

(* for sgrep_js *)
val any_of_string:
  string -> Cst_js.any

(* for sgrep via fuzzy AST *)
val parse_fuzzy:
  Common.filename -> Ast_fuzzy.trees * Parser_js.token list

(* to help write test code *)
val program_of_string: string -> Cst_js.program

(* internal *)
val tokens: Common.filename -> Parser_js.token list
