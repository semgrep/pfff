(*s: pfff/lang_GENERIC/parsing/map_ast.mli *)
open Ast_generic

(*s: type [[Map_ast.visitor_in]] *)
type visitor_in = {
  kexpr: (expr -> expr) * visitor_out -> expr -> expr;
  kstmt: (stmt -> stmt) * visitor_out -> stmt -> stmt;

  kinfo: (tok -> tok) * visitor_out -> tok -> tok;
}
(*e: type [[Map_ast.visitor_in]] *)

(*s: type [[Map_ast.visitor_out]] *)
and visitor_out = {
  vitem: item -> item;
  vprogram: program -> program;
  vexpr: expr -> expr;
  vany: any -> any;
}
(*e: type [[Map_ast.visitor_out]] *)

(*s: signature [[Map_ast.default_visitor]] *)
val default_visitor: visitor_in
(*e: signature [[Map_ast.default_visitor]] *)

(*s: signature [[Map_ast.mk_visitor]] *)
val mk_visitor: visitor_in -> visitor_out
(*e: signature [[Map_ast.mk_visitor]] *)
(*e: pfff/lang_GENERIC/parsing/map_ast.mli *)
