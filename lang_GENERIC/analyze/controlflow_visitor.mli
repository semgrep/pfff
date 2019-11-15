
type visitor_in = Visitor_ast.visitor_in
type visitor_out = Controlflow.node -> unit

val mk_visitor: visitor_in -> visitor_out
