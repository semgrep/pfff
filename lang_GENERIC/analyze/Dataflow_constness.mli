
type ctype =
  | Cbool
  | Cint
  | Cstr
  | Cany

type constness =
  | Lit of AST_generic.literal
  | Cst of ctype
  | Dyn

type mapping = constness Dataflow.mapping

val string_of_constness : constness -> string

val fixpoint : IL.cfg -> mapping

(** Flow-sensitive constant-propagation.
 * Refines (does not simply overwrite) constness info, that is, it
 * respects previous constant propagation analyses.
 * Works by side effect on the generic AST by modifying its refs.
 *
 * !Note that this assumes Naming_AST.resolve has been called before!
*)
val propagate_constants : AST_generic.program -> unit
