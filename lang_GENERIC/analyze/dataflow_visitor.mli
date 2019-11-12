module F = Controlflow

type lhs_or_rhs = Lhs | Rhs

(* 'a is usually  a set, e.g., a Dataflow.NodeiSet.t *)
type 'a fold_fn = 
  Dataflow.nodei -> Dataflow.var -> lhs_or_rhs -> 'a -> 'a


val flow_fold : 'a fold_fn -> 'a -> F.flow -> 'a

val flow_fold_lv :
  (Dataflow.nodei -> Dataflow.var -> 'a -> 'a) -> 'a -> F.flow -> 'a
val flow_fold_rv :
  (Dataflow.nodei -> Dataflow.var -> 'a -> 'a) -> 'a -> F.flow -> 'a
