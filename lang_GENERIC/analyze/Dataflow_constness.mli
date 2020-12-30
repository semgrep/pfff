
type mapping = AST_generic.constness Dataflow.mapping

val string_of_constness : AST_generic.constness -> string

(** Flow-sensitive constant-propagation.
 *
 * Returns a mapping, but it also updates the [IL.lval.constness] refs
 * accordingly. The constness refs in IL are shared with the Generic AST,
 * so running this analysis also updates constness info in the Generic AST.
 * When updating the AST, it respects previous constant propagation passes,
 * updating constness info when it can deduce more specific facts, but
 * leaves it untouched otherwise.
 *
 * !Note that this assumes Naming_AST.resolve has been called before!
*)
val fixpoint : IL.cfg -> mapping

val update_constness : IL.cfg -> mapping -> unit
