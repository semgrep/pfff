
val exprs_of_node: Controlflow.node -> Ast.expr list
val fold_on_node_and_expr: 
  ((Controlflow.nodei * Controlflow.node) -> Ast.expr -> 'a -> 'a) -> 
  Controlflow.flow -> 'a -> 'a

type visitor_in = Visitor_ast.visitor_in
type visitor_out = Controlflow.node -> unit

val mk_visitor: visitor_in -> visitor_out
