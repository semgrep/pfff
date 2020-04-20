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
module I = Il

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
type env = {
  instrs: instr list ref;
}

let empty_env () = {
  instrs = ref [];
}


(*****************************************************************************)
(* Error management *)
(*****************************************************************************)
let error tok s =
  raise (Parse_info.Ast_builder_error (s, tok))

let error_any any_generic msg =
  let toks = Lib_ast.ii_of_any any_generic in
  let s = Meta_ast.vof_any any_generic |> Ocaml.string_of_v in
  error (List.hd toks) (spf "%s: %s" msg s)

let sgrep_construct any_generic =
  error_any any_generic "Sgrep Construct"

let todo any_generic =
  error_any any_generic "TODO Construct"

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

let mk_s s =
  { s }

let _add_instr env instr = 
  Common.push env.instrs instr

(*****************************************************************************)
(* lvalue *)
(*****************************************************************************)
let _lval _env _x =
  raise Todo

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
and stmt _env st =
  match st with
  | G.DefStmt def -> [mk_s (I.DefStmt def)]
  | G.DirectiveStmt dir -> [mk_s (I.DirectiveStmt dir)]

  | G.DisjStmt _ -> sgrep_construct (G.S st)
  | _ -> todo (G.S st)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let stmt st =
  let env = empty_env () in
  stmt env st

