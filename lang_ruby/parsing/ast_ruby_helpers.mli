open Ast_ruby

val binary_op_of_string : string -> binary_op

val mod_ast : (expr -> expr) -> expr list -> expr list

(* converts the method name or operator in the given string and
   returns an expression suitable for use within a MethodDef *)
val msg_of_str : string -> tok -> expr
