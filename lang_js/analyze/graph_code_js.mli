val build:
  ?verbose:bool -> Common.dirname -> Common.filename list ->
  Graph_code.graph

(* helpers *)
val kind_of_expr: Ast_js.var_kind -> Ast_js.expr -> Entity_code.entity_kind
