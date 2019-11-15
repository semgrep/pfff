type reaching_mapping = Dataflow.NodeiSet.t Dataflow.mapping

(* main entry point *)
val reaching_fixpoint : Dataflow.F.flow -> reaching_mapping

val display_reaching_dflow :
  Dataflow.F.flow -> reaching_mapping -> unit
