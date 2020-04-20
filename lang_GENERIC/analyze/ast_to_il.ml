(* Yoann Padioleau
 *
 * Copyright (C) 2020 r2c
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
open Common

open Il
module G = Ast_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* AST generic to IL translation.
 *
 * todo:
 *  - a lot ...
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*****************************************************************************)
(* Error management *)
(*****************************************************************************)
let _error _tok _s =
  let _ = G.Block [] in
  raise Todo

let _error_any _any_generic _s =
  raise Todo

let _sgrep_construct _any_generic =
  raise Todo

let _todo _any_generic =
  raise Todo

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let _fresh_var () = 
  let i = G.gensym () in
  "_tmp", i

let _fresh_label () = 
  let i = G.gensym () in
  "_label", i

let _mk_e e eorig = 
  { e; eorig}

let _mk_i i iorig =
  { i; iorig }

let _mk_s s =
  { s }

(*****************************************************************************)
(* lvalue *)
(*****************************************************************************)

(*****************************************************************************)
(* Assign *)
(*****************************************************************************)

(*****************************************************************************)
(* Expression *)
(*****************************************************************************)

(*****************************************************************************)
(* Instruction *)
(*****************************************************************************)

(*****************************************************************************)
(* Statement *)
(*****************************************************************************)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let stmt _st =
  raise Todo
