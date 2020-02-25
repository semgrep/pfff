
val parse_lexbuf : Lexing.lexbuf -> Ast.ast

val parse_string : ?env:Utils.StrSet.t -> ?filename:string -> ?lineno:int
  -> string -> Ast.ast

val parse_file : string -> Ast.ast

