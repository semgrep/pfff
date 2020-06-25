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
(*
open Common

open Ast_ruby
*)
module G = AST_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Ast_ruby to AST_generic.
 *
 * See ast_generic.ml for more information.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
(*
let id = fun x -> x
let option = Common.map_opt
let list = List.map

let bool = id
let string = id

let error = AST_generic.error
*)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(*
let info x = x
let tok v = info v

let wrap = fun _of_a (v1, v2) ->
  let v1 = _of_a v1 and v2 = info v2 in 
  (v1, v2)

let bracket of_a (t1, x, t2) = (info t1, of_a x, info t2)
*)

let program _ast = 
  (* TODO *)
  []

let any _any =
  G.Ss []