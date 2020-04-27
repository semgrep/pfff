
(* map for each node/var whether a variable is "tainted" *)
type mapping = unit Dataflow.mapping

(* this can use semgrep patterns under the hood *)
type config = {
  is_source: Il.instr -> bool;
  is_sink: Il.instr -> bool;
  is_sanitizer: Il.instr -> bool;

  found_tainted_sink: Il.instr -> unit Dataflow.env -> unit;
}

(* main entry point *)
val fixpoint : config ->Il.cfg -> mapping
