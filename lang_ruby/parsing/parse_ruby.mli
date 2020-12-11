
val parse: Common.filename ->
  (Ast_ruby.program, Parser_ruby.token) Parse_info.parsing_result

val parse_program : Common.filename -> Ast_ruby.program

(* for semgrep *)
val any_of_string:
  string -> Ast_ruby.any
