
(* this is just used for the reaching/liveness analysis for now *)

val lvalues_of_expr: 
 Ast.expr -> (Ast.ident * Ast.id_info) list

val rvalues_of_expr: 
 Ast.expr -> (Ast.ident * Ast.id_info) list
