(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
 * Copyright (C) 2019 r2c
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

open Ast_generic
module V = Visitor_ast
module E = Error_code
module F = Controlflow
module D = Dataflow

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Most of the hard work is done by Controlflow_build.ml.
 * 
 * TODO: check dead statements for toplevel blocks ?
 *)

(*****************************************************************************)
(* Checks *)
(*****************************************************************************)

(* less: could be in controlflow_build.ml (was there before) *)
let (unreachable_statement_detection : F.flow -> unit) = fun flow ->
  flow#nodes#iter (fun (k, node) ->
    let pred = flow#predecessors k in
    if pred#null then
      (match node.F.n with
      | F.Enter -> ()
      | _ ->
          (match node.F.i with
          | None ->
              pr2 (spf "CFG: PB, found dead node but no loc: %s"
                   (Controlflow.short_string_of_node node))
          | Some info ->
              E.error info E.UnusedStatement
          )
      )
  )


let is_ok_unused_varname s =
  s =~ "_.*"

let (dead_assign_detection: F.flow -> Dataflow_liveness.mapping -> unit) =
 fun flow mapping ->
  Controlflow_visitor.fold_on_node_and_expr (fun (ni, _nd) e () ->
    let lvals = Lrvalue.lvalues_of_expr e in
    lvals |> List.iter (fun ((var, tok), _idinfo) ->
      (* TODO: filter just Locals here! *)
      let out_env = mapping.(ni).D.out_env in
      try 
        let () = D.VarMap.find var out_env in
        ()
      with Not_found -> 
        if not (is_ok_unused_varname var)
        then E.error tok (E.UnusedAssign var)
    )
  ) flow ()

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let check_func_def fdef =
  try 
    let flow = Controlflow_build.cfg_of_func fdef in
    unreachable_statement_detection flow;

    let mapping = Dataflow_liveness.fixpoint flow in
    dead_assign_detection flow mapping;

  with Controlflow_build.Error (err, loc) ->
      let s = Controlflow_build.string_of_error_kind err in
      (match loc with
      | Some tok -> E.error tok (E.CFGError s)
      | None -> pr2 (spf "TODO: CFG error detected but no location: %s" s)
      )

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let check_program2 prog =
  let visitor = V.mk_visitor { V.default_visitor with
    (* also valid for methods *)
    V.kdef = (fun (k, _) x ->
      let (_entity, def) = x in
      (match def with
      | FuncDef fdef -> check_func_def fdef
      | _ -> ()
      );
      (* process nested definitions *)
      k x
    );
  }
  in
  visitor (Pr prog)

let check_program a = 
  Common.profile_code "Checker.cfg" (fun () -> check_program2 a)
