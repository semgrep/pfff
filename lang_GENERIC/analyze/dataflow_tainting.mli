(*s: pfff/lang_GENERIC/analyze/dataflow_tainting.mli *)

(*s: type [[Dataflow_tainting.mapping]] *)
(* map for each node/var whether a variable is "tainted" *)
type mapping = unit Dataflow.mapping
(*e: type [[Dataflow_tainting.mapping]] *)

(*s: type [[Dataflow_tainting.config]] *)
(* this can use semgrep patterns under the hood *)
type config = {
  is_source: Il.instr -> bool;
  is_sink: Il.instr -> bool;
  is_sanitizer: Il.instr -> bool;

  found_tainted_sink: Il.instr -> unit Dataflow.env -> unit;
}
(*e: type [[Dataflow_tainting.config]] *)

(*s: signature [[Dataflow_tainting.fixpoint]] *)
(* main entry point *)
val fixpoint : config -> Il.cfg -> mapping
(*e: signature [[Dataflow_tainting.fixpoint]] *)
(*e: pfff/lang_GENERIC/analyze/dataflow_tainting.mli *)
