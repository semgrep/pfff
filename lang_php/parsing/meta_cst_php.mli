open Cst_php

(*s: meta_ast_php.mli *)
val vof_program: program -> Ocaml.v
val vof_toplevel: toplevel -> Ocaml.v
val vof_expr: expr -> Ocaml.v
val vof_any: any -> Ocaml.v

(* used by pil.ml or ast_php_simple.ml *)

val vof_info: tok -> Ocaml.v
val vof_tok: tok -> Ocaml.v

val vof_dname: dname -> Ocaml.v
val vof_name: name -> Ocaml.v

val vof_binaryOp: binaryOp -> Ocaml.v
val vof_unaryOp: unaryOp -> Ocaml.v
val vof_assignOp: assignOp -> Ocaml.v
val vof_castOp: castOp -> Ocaml.v
val vof_fixOp: fixOp -> Ocaml.v
val vof_ptype: ptype -> Ocaml.v

val vof_hint_type: hint_type -> Ocaml.v
val vof_constant: constant -> Ocaml.v
val vof_class_name_reference: class_name_reference -> Ocaml.v
val vof_modifier: modifier -> Ocaml.v

(*e: meta_ast_php.mli *)
