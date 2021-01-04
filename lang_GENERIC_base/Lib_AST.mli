(*s: pfff/lang_GENERIC/parsing/Lib_AST.mli *)

(* Note that ii_of_any relies on Visitor_AST which itself
 * uses OCaml.v_ref_do_not_visit, so no need to worry about
 * tokens inside id_type or id_info.
*)
(*s: signature [[Lib_AST.ii_of_any]] *)
val ii_of_any: AST_generic.any -> Parse_info.t list
(*e: signature [[Lib_AST.ii_of_any]] *)

(** Abstract away position and constness for comparison
 * with polymorphic operators.
*)
val abstract_for_comparison_any: AST_generic.any -> AST_generic.any
(*e: pfff/lang_GENERIC/parsing/Lib_AST.mli *)
