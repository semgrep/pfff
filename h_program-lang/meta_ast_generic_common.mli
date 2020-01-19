
(* reused in other dumpers *)
val vof_arithmetic_operator: Ast_generic.arithmetic_operator -> Ocaml.v
val vof_incr_decr: Ast_generic.incr_decr -> Ocaml.v
val vof_inc_dec: Ast_generic.incr_decr * Ast_generic.prefix_postfix -> Ocaml.v
val vof_prepost: Ast_generic.prefix_postfix -> Ocaml.v

