
(* map for each node/var whether a variable is "tainted" *)
type mapping = unit Dataflow.mapping

(* main entry point *)
val fixpoint : Il.cfg -> mapping
