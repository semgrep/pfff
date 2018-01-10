
val build:
  ?verbose:bool -> 
  root:Common.path ->
  cmt_files:Common.filename list -> 
  ml_files:Common.filename list ->
  Graph_code.graph

(* for syncweb's indexer *)
val hook_def_node:
  (Graph_code.node -> Graph_code.graph -> unit) ref

val hook_use_edge: 
  ((Graph_code.node * Graph_code.node) -> 
  Graph_code.graph -> Parse_info.token_location ->
   unit)
  ref
