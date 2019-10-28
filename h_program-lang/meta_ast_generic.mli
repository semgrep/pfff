
val vof_any: Ast_generic.any -> Ocaml.v

(* obsolete? not working with vof_any anyway *)
type precision = {
  full_info: bool;
  token_info: bool;
  type_info: bool;
}
val default_precision: precision


(* reused in other dumpers *)
val vof_arithmetic_operator: Ast_generic.arithmetic_operator -> Ocaml.v

