
(* the token list contains also the comment-tokens *)
type program_and_tokens = 
  Ast_python.program option * Parser_python.token list

type parsing_mode =
  | Python2
  | Python3
  (* will start with Python3 and fallback to Python2 in case of an error *)
  | Python

(* This is the main function.
 * can throw Parse_info.Lexical_error and Parse_info.Parsing_error *)
val parse:
  ?parsing_mode:parsing_mode (* default mode is Python *) ->
  Common.filename -> (program_and_tokens * Parse_info.parsing_stat)

val parse_program:
  ?parsing_mode:parsing_mode ->
  Common.filename -> Ast_python.program

(* other parsers *)

(* for sgrep *)
val any_of_string:
  ?parsing_mode:parsing_mode -> string -> Ast_python.any

(* for sgrep via fuzzy AST *)
(*
val parse_fuzzy:
  Common.filename -> Ast_fuzzy.trees * Parser_python.token list
*)

(* to help write test code *)
val program_of_string: string -> Ast_python.program


(* internal *)
val tokens: parsing_mode -> Common.filename -> Parser_python.token list
