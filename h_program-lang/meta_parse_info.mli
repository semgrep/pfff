
val vof_info: Parse_info.t -> Ocaml.v

type dumper_precision = {
  full_info: bool;
  token_info: bool;
  type_info: bool;
}
val default_dumper_precision : dumper_precision
