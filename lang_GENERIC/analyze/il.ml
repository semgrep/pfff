(* Intermediate language for static analysis.
 *
 * Just like for the CST -> AST, the goal of an AST -> IL transformation
 * is to simplify things even more for program analysis purpose.
 *
 * TODO Here are simplifications that could be done to the AST:
 *  - intermediate instr type, for instruction, for statements without
 *    any control flow
 *  - intermediate lvalue type, expressions are splitted in 
 *    lvalue vs regular expressions
 *  - Assign is now an instruction, not an expression
 *  - no AssignOp, or Decr/Incr, just Assign
 *  - Calls are now instructions (not nested inside complex expressions)
 *    and all its arguments are variables?
 *  - Lambdas are now instructions (not nested again)
 *  - Seq are instructions
 * 
 * Note that we still want to be close to the original code so that
 * error reported on the IL can be mapped back to error on the original code
 * (source "maps").
 *
 * history:
 *  - cst_php.ml (was actually called ast_php.ml)
 *  - ast_php.ml (was called ast_php_simple.ml)
 *  - pil.ml, still for PHP
 *  - il.ml for AST generic
 * 
 * related work:
 *  - CIL, C Intermediate Language, Necula et al, CC'00
 *  - RIL, The Ruby Intermediate Language, Furr et al, DLS'09
 *  - C-- in OCaml?
 *  - Rust IL?
 *  - LLVM IR (but too far away from original code? complicated 
 *    source maps)
 *)
