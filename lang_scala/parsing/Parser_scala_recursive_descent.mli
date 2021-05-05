
(* may raise Parse_info.Parsing_error exn *)
val parse: Parser_scala.token list -> AST_scala.program

val debug_parser: bool ref
