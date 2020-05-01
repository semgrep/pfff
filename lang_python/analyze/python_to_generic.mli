(*s: pfff/lang_python/analyze/python_to_generic.mli *)

(*s: signature [[Python_to_generic.program]] *)
val program: Ast_python.program -> Ast_generic.program
(*e: signature [[Python_to_generic.program]] *)

(*s: signature [[Python_to_generic.any]] *)
val any: Ast_python.any -> Ast_generic.any
(*e: signature [[Python_to_generic.any]] *)

(* exception Error of string * Parse_info.info *)
(* may raise Error *)
(*e: pfff/lang_python/analyze/python_to_generic.mli *)
