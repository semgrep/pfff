open Ast_generic

val control_flow_graph_of_stmts: 
  parameter list -> stmt list -> Controlflow_generic.flow

(* alias *)
val cfg_of_stmts: parameter list -> stmt list -> Controlflow_generic.flow

val cfg_of_func:   function_definition -> Controlflow_generic.flow

val deadcode_detection : Controlflow_generic.flow -> unit

type error = error_kind * Parse_info.info option
 and error_kind = 
  | DeadCode of Controlflow_generic.node_kind
  | NoEnclosingLoop
  | DynamicBreak


val string_of_error: error -> string
val string_of_error_kind: error_kind -> string

exception Error of error

val report_error : error -> unit
