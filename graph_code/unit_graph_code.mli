
(* Test suite for this directory. *)

(* The function passed as a parameter makes it possible to have
 * a limited form of circular dependency, see unit_matcher.mli comment.
*)

val tests:
  graph_of_string:(string -> Graph_code.graph) ->
  Testutil.test list
