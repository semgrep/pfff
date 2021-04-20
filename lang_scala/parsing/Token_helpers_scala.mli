
val is_eof          : Parser_scala.token -> bool
val is_comment      : Parser_scala.token -> bool

val token_kind_of_tok: Parser_scala.token -> Parse_info.token_kind

val info_of_tok :
  Parser_scala.token -> Parse_info.t
val visitor_info_of_tok :
  (Parse_info.t -> Parse_info.t) -> Parser_scala.token -> Parser_scala.token
