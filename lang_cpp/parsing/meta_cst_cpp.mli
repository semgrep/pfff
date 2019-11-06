
val vof_program: 
  ?precision:Meta_parse_info.dumper_precision ->
  Cst_cpp.program -> Ocaml.v

val vof_any: 
  ?precision:Meta_parse_info.dumper_precision -> 
  Cst_cpp.any -> Ocaml.v
