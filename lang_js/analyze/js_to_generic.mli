
exception Error of string * Parse_info.info

(* may raise Error *)
val program: Ast_js.program -> Ast_generic.program

