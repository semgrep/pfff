(* Works by side effect on the generic AST by modifying its refs.
 * We pass the lang because some constant propagation algorithm may be
 * specific to a language.
 *)
val propagate: Lang.t -> AST.program -> unit
