open Ast_generic

type visitor_in = {
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
