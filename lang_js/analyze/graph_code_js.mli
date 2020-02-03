val build:
  ?verbose:bool -> Common.dirname -> Common.filename list ->
  Graph_code.graph

(* helpers *)
val kind_of_expr: 
  Ast_js.var_kind Ast_js.wrap -> Ast_js.expr -> Entity_code.entity_kind

val build_for_ai: Common.dirname -> Common.filename list ->
  (Ast_js.qualified_name, Ast_js.var) Hashtbl.t *
  (Common.filename (* readable *) * Ast_js.program) list
