
(* may raise Parse_info.Parsing_error exn *)
val parse: Token_scala.token list -> AST_scala.program

val semgrep_pattern: Token_scala.token list -> AST_scala.any
