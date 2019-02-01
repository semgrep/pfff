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
 * a copy paste of what was in the OCaml 4.02 compiler source (and what
 * was used to generate via ocamltarzan meta_ast_cmt.ml)
 * 
 * Starting from ocaml 4.02, the library
 * https://github.com/ocaml-ppx/ocaml-migrate-parsetree
 * allows to migrate automatically AST from one version to another.
 * Unfortunately, the library only migrate Parsetree, not Typedtree, which
 * is the tree we are using here. Thus, I regularly need to update
 * the code under lang_cmt/ to handle the changes in the Typedtree and
 * thus .cmt format.
 * alt: using #ifdef to handle multiple versions (4.02, 4.03, ...) but
 * this is tedious.
 *)

(*****************************************************************************)
(* The AST *)
(*****************************************************************************)

type ast = Cmt_format.cmt_infos
