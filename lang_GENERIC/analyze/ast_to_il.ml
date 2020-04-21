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
let fresh_var _env tok = 
  let i = G.gensym () in
  ("_tmp", tok), i

let _fresh_label _env tok = 
  let i = G.gensym () in
  ("_label", tok), i

let fresh_lval env tok =
  let var = fresh_var env tok in
  { base = Var var; offset = NoOffset }

let mk_e e eorig = 
  { e; eorig}

let mk_i i iorig =
  { i; iorig }

let mk_s s =
  { s }

let add_instr env instr = 
  Common.push instr env.instrs

let prepend_and_reset_instrs env after = 
  let xs = !(env.instrs) in
  env.instrs := [];
  (xs |> List.map (fun instr -> mk_s (I.Instr instr))) @ after
  

(*****************************************************************************)
(* lvalue *)
(*****************************************************************************)
let rec _lval _env _x =
  raise Todo

(*****************************************************************************)
(* Assign *)
(*****************************************************************************)

(*****************************************************************************)
(* Expression *)
(*****************************************************************************)
and expr env e =
  match e with
  | G.Call (G.IdSpecial spec, args) ->
      let tok = snd spec in
      let lval = fresh_lval env tok in
      let special = call_special env spec in
      let args = arguments env args in
      add_instr env (mk_i (I.CallSpecial (Some lval, special, args)) e);
      mk_e (I.Lvalue lval) e
  | G.L lit -> mk_e (I.Literal lit) e
      
  | _ -> todo (G.E e)
  

and expr_opt env = function
  | None -> 
      let void = G.Unit (G.fake "void") in
      mk_e (I.Literal void) (G.L void)
  | Some e -> expr env e

and call_special _env (x, tok) = 
  (match x with
  | G.ArithOp op -> I.Operator op
  | _ -> todo (G.E (G.IdSpecial (x, tok)))
  ), tok

(* TODO: dependency of order between arguments for instr? *)
and arguments env xs = 
  xs |> List.map (argument env)

and argument env arg =
  match arg with
  | G.Arg e -> expr env e
  | _ -> todo (G.Ar arg)

(*****************************************************************************)
(* Statement *)
(*****************************************************************************)
and stmt env st =
  match st with
  | G.DefStmt def -> [mk_s (I.DefStmt def)]
  | G.DirectiveStmt dir -> [mk_s (I.DirectiveStmt dir)]

  | G.Block xs -> List.map (stmt env) xs |> List.flatten

  | G.Return (tok, eopt) ->
      let e = expr_opt env eopt in
      prepend_and_reset_instrs env
      [mk_s (I.Return (tok, e))]

  | G.ExprStmt e ->
      let _e' = expr env e in
      prepend_and_reset_instrs env []
      

  | G.DisjStmt _ -> sgrep_construct (G.S st)
  | _ -> todo (G.S st)
 

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let stmt st =
  let env = empty_env () in
  stmt env st

