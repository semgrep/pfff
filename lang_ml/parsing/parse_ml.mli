
(* the token list contains also the comment-tokens *)
type program_and_tokens =
  Ast_ml.program option * Parser_ml.token list

(* This is the main function. See flag_parsing_ml for settings. *)
val parse:
  Common.filename -> (program_and_tokens * Parse_info.parsing_stat)

val parse_program:
  Common.filename -> Ast_ml.program

(* for semgrep *)
val any_of_string:
  string -> Ast_ml.any
(* for semgrep and LSP *)
val type_of_string:
  string -> Ast_ml.type_


(* internal *)
val tokens: Common.filename -> Parser_ml.token list
