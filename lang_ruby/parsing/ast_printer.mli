(** This module produces a verbose, debug-friendly description of a
    Ruby AST.  It is intended purely to debug the parser and not for
    general use.  Cfg_printer should be used for unparsing.  *)

val format_uop : Format.formatter -> Ast_ruby.unary_op -> unit

val format_binop : Format.formatter -> Ast_ruby.binary_op -> unit

val format_string_contents : Format.formatter -> Ast_ruby.string_contents -> unit

val format_interp_string : Format.formatter -> Ast_ruby.interp_string -> unit

val format_string_kind : Format.formatter -> Ast_ruby.string_kind -> unit

val format_lit_kind : Format.formatter -> Ast_ruby.lit_kind -> unit

val format_expr : Format.formatter -> Ast_ruby.expr -> unit

val format_expr_comma_list : Format.formatter -> Ast_ruby.expr list -> unit

val format_expr_break_list : Format.formatter -> Ast_ruby.expr list -> unit

val format_formals : Format.formatter -> Ast_ruby.formal_param list -> unit

val format_rescues : Format.formatter -> (Ast_ruby.expr * Ast_ruby.expr) list -> unit

val format_ensure : Format.formatter -> Ast_ruby.expr list -> unit

val format_else : Format.formatter -> Ast_ruby.expr list -> unit

val format_inheritance :
  Format.formatter -> Ast_ruby.inheritance_kind option -> unit

val format_string_list : Format.formatter -> string list -> unit

val format_formal : Format.formatter -> Ast_ruby.formal_param -> unit

val format_case : Format.formatter -> Ast_ruby.case_block -> unit

val format_whens :
  Format.formatter -> (Ast_ruby.expr list * Ast_ruby.expr list) list -> unit

val format_ast : Format.formatter -> Ast_ruby.expr list -> unit

val print_ast : Ast_ruby.expr list -> unit

val string_of_expr : Ast_ruby.expr -> string

val string_of_ast : Ast_ruby.expr list -> string

