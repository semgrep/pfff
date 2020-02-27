module Ast = Ast_ruby
module H = Ast_ruby_helpers
module HH = Parser_ruby_helpers
module Utils = Utils_ruby
val set_lexbuf_fname : Lexing.lexbuf -> string -> unit
val set_lexbuf_lineno : Lexing.lexbuf -> int -> unit
val uniq_list : (Ast_ruby.expr list * 'a) list -> Ast_ruby.expr list list
val parse_lexbuf_with_state :
  ?env:Utils.StrSet.t -> Lexer_ruby.S.t -> Lexing.lexbuf -> Ast_ruby.ast
val parse_string_with_state :
  Lexer_ruby.S.t ->
  ?env:Utils.StrSet.t ->
  ?filename:string -> ?lineno:int -> string -> Ast_ruby.ast
val parse_lexbuf : Lexing.lexbuf -> Ast_ruby.ast
val parse_string :
  ?env:Utils.StrSet.t ->
  ?filename:string -> ?lineno:int -> string -> Ast_ruby.ast
val parse_file : string -> Ast_ruby.ast
val parse_program : string -> Ast_ruby.ast
