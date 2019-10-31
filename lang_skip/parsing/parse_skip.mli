
(* the token list contains also the comment-tokens *)
type program_and_tokens = 
  Ast_skip.program option * Parser_skip.token list

(* This is the main function. See flag_parsing for settings. *)
val parse:
  Common.filename -> (program_and_tokens * Parse_info.parsing_stat)

val parse_program:
  Common.filename -> Ast_skip.program

val parse_fuzzy:
  Common.filename -> Ast_fuzzy.tree list * Parser_skip.token list

(* internal *)
val tokens: Common.filename -> Parser_skip.token list
