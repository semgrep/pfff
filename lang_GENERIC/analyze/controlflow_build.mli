open Ast_generic

val control_flow_graph_of_stmts: 
  parameter list -> stmt list -> Controlflow.flow

(* alias *)
val cfg_of_stmts: parameter list -> stmt list -> Controlflow.flow

val cfg_of_func:   function_definition -> Controlflow.flow

val deadcode_detection : Controlflow.flow -> unit

type error = error_kind * Parse_info.t option
 and error_kind = 
  | DeadCode of Controlflow.node_kind
  | NoEnclosingLoop
  | DynamicBreak


val string_of_error: error -> string
val string_of_error_kind: error_kind -> string

exception Error of error

val report_error : error -> unit
