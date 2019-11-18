(* Intermediate language!
 *
 * As for CST -> AST, the goal of an AST -> IL is to simplify things
 * even more for program analysis purpose.
 *
 * Here are the simplifications done to the AST:
 *  - intermediate instr type, for instruction, for statements without
 *    any control flow
 *  - intermediate lvalue type, expressions are splitted in 
 *    lvalue vs regular expressions
 *  - Assign is now an instruction, not an expression
 *  - no AssignOp, or Decr/Incr, just Assign
 *  - Calls are now instructions (not nested inside complex expressions)
 *  - Lambdas are now instructions (not nested again)
 *  - Seq are instructions
 *
 * history:
 *  - cst_php.ml (was actually called ast_php.ml)
 *  - ast_php.ml (was called ast_php_simple.ml)
 *  - pil.ml, still for PHP
 *  - il.ml for AST generic
 * 
 * related work:
 *  - CIL, C Intermediate Language, Necula et al, CC'00
 *  - RIL, The Ruby Intermediate Language, Furr et al, DSL'09
 *  - C-- in OCaml?
 *)
