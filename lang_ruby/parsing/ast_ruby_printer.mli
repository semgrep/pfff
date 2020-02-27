(** This module produces a verbose, debug-friendly description of a
    Ruby AST.  It is intended purely to debug the parser and not for
    general use.  Cfg_printer should be used for unparsing.  *)

val string_of_expr : Ast_ruby.expr -> string

val string_of_ast : Ast_ruby.expr list -> string

(* used in il_ruby_build *)
val format_lit_kind : Format.formatter -> Ast_ruby.lit_kind -> unit
val format_expr : Format.formatter -> Ast_ruby.expr -> unit
val format_formals : Format.formatter -> Ast_ruby.formal_param list -> unit
