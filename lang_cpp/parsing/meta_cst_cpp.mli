
val vof_program: 
  ?precision:Parse_info.dumper_precision ->
  Cst_cpp.program -> Ocaml.v

val vof_any: 
  ?precision:Parse_info.dumper_precision -> 
  Cst_cpp.any -> Ocaml.v
