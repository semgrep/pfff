open Ast_ruby

val compare_expr : expr -> expr -> int
val equal_expr : expr -> expr -> bool

val compare_ast : expr list -> expr list -> int
val equal_ast : expr list -> expr list -> bool

val pos_of : expr -> pos
val set_pos : pos -> expr -> expr

val binary_op_of_string : string -> binary_op

val mod_ast : (expr -> expr) -> expr list -> expr list

(* converts the method name or operator in the given string and
   returns an expression suitable for use within a MethodDef *)
val msg_of_str : string -> pos -> expr

val str_uop : unary_op -> string
val str_binop : binary_op -> string
