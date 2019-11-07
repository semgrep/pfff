open Ast_generic

val cfg_of_func:   function_definition -> Controlflow.flow

val control_flow_graph_of_stmts: 
  parameter list -> stmt list -> Controlflow.flow
(* alias *)
val cfg_of_stmts: parameter list -> stmt list -> Controlflow.flow

val deadcode_detection : Controlflow.flow -> unit

type error = error_kind * Parse_info.t option
 and error_kind = 
  (* raised during the building of the CFG *)
  | NoEnclosingLoop
  | DynamicBreak
  (* raised by deadcode_detection() *)
  | UnreachableStatement of Controlflow.node_kind


val string_of_error: error -> string
val string_of_error_kind: error_kind -> string

exception Error of error

val report_error : error -> unit
