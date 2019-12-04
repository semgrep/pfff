open Ast_generic

type visitor_in = {
  kexpr: (expr -> expr) * visitor_out -> expr -> expr;
  kstmt: (stmt -> stmt) * visitor_out -> stmt -> stmt;

  kinfo: (tok -> tok) * visitor_out -> tok -> tok;
}

and visitor_out = {
  vitem: item -> item;
  vprogram: program -> program;
  vexpr: expr -> expr;
  vany: any -> any;
}

val default_visitor: visitor_in

val mk_visitor: visitor_in -> visitor_out
