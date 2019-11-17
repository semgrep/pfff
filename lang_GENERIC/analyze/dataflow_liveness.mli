
(* map for each node/var whether a variable is "live" *)
type mapping = unit Dataflow.mapping

(* main entry point *)
val fixpoint : Dataflow.F.flow -> mapping

val display :
  Dataflow.F.flow -> mapping -> unit
