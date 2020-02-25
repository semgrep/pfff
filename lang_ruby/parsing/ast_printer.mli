
(** This module produces a verbose, debug-friendly description of a
    Ruby AST.  It is intended purely to debug the parser and not for
    general use.  Cfg_printer should be used for unparsing.  *)

val format_uop : Format.formatter -> Ast.unary_op -> unit

val format_binop : Format.formatter -> Ast.binary_op -> unit

val format_string_contents : Format.formatter -> Ast.string_contents -> unit

val format_interp_string : Format.formatter -> Ast.interp_string -> unit

val format_string_kind : Format.formatter -> Ast.string_kind -> unit

val format_lit_kind : Format.formatter -> Ast.lit_kind -> unit

val format_expr : Format.formatter -> Ast.expr -> unit

val format_expr_comma_list : Format.formatter -> Ast.expr list -> unit

val format_expr_break_list : Format.formatter -> Ast.expr list -> unit

val format_formals : Format.formatter -> Ast.formal_param list -> unit

val format_rescues : Format.formatter -> (Ast.expr * Ast.expr) list -> unit

val format_ensure : Format.formatter -> Ast.expr list -> unit

val format_else : Format.formatter -> Ast.expr list -> unit

val format_inheritance :
  Format.formatter -> Ast.inheritance_kind option -> unit

val format_string_list : Format.formatter -> string list -> unit

val format_formal : Format.formatter -> Ast.formal_param -> unit

val format_case : Format.formatter -> Ast.case_block -> unit

val format_whens :
  Format.formatter -> (Ast.expr list * Ast.expr list) list -> unit

val format_ast : Format.formatter -> Ast.expr list -> unit

val print_ast : Ast.expr list -> unit

val string_of_expr : Ast.expr -> string

val string_of_ast : Ast.expr list -> string

