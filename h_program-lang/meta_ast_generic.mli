
val vof_any: Ast_generic.any -> Ocaml.v

(* obsolete? not working with vof_any anyway *)
type precision = {
  full_info: bool;
  token_info: bool;
  type_info: bool;
}
val default_precision: precision

