(* Yoann Padioleau
 *
 * Copyright (C) 2012, 2017 Facebook
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * This is mostly a wrapper around the OCaml compiler libs and its
 * typed OCaml tree. See the ast_cmt_stdlib.ml file in this directory for
 * a copy paste of what is in the OCaml compiler source (and what
 * was used to generate via ocamltarzan meta_ast_cmt.ml).
 * 
 * todo: starting from ocaml 4.02, you can use 
 * https://github.com/ocaml-ppx/ocaml-migrate-parsetree
 * to migrate automatically .cmt from one version to another.
 * That way you can write code (e.g., in graph_code_cmt.ml) handling
 * the AST of OCaml 4.02 and the same code could work on 4.06
 * (by converting down the AST from 4.06 to 4.02).
 *)

(*****************************************************************************)
(* The AST *)
(*****************************************************************************)

type ast = Cmt_format.cmt_infos
