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

let is_global idinfo =
  match !(idinfo.id_resolved) with
  | Some (Global _) -> true
  | _ -> false

let is_local idinfo =
  match !(idinfo.id_resolved) with
  | Some (Local _ | Param _) -> true
  | _ -> false

let (dead_assign_detection: F.flow -> Dataflow_liveness.mapping -> unit) =
 fun flow mapping ->
  Controlflow_visitor.fold_on_node_and_expr (fun (ni, _nd) e () ->
    let lvals = Lrvalue.lvalues_of_expr e in
    lvals |> List.iter (fun ((var, tok), idinfo) ->
      (* TODO: filter just Locals here! *)
      let out_env = mapping.(ni).D.out_env in
      try 
        let () = D.VarMap.find var out_env in
        ()
      with Not_found -> 
        if is_ok_unused_varname var || is_global idinfo
        then ()
        else E.error tok (E.UnusedAssign var)
    )
  ) flow ()

let (_use_of_uninitialized: F.flow -> Dataflow_liveness.mapping -> unit) =
  fun flow mapping ->
    let enteri = F.find_enter flow in
    let in_enteri = mapping.(enteri).D.in_env in
    Controlflow_visitor.fold_on_node_and_expr (fun (_ni, _nd) e () ->
      let rvals = Lrvalue.rvalues_of_expr e in
      rvals |> List.iter (fun ((var, tok), idinfo) ->
        if D.VarMap.mem var in_enteri && (is_local idinfo)
        then E.error tok (E.UseOfUninitialized var)
      )
    ) flow ()



(* alternative dead assign, using reaching_def instead (but a bit awkward) *)
(*
let string_of_ni flow ni =
  let node = flow#nodes#assoc ni in
  match node.F.i with
  | None -> "Unknown location"
  | Some(info) ->
    let info = Parse_info.token_location_of_info info in
    spf "%s:%d:%d: "
      info.Parse_info.file info.Parse_info.line info.Parse_info.column

let display flow mapping =
  let arr = Dataflow.new_node_array flow true in

  (* Set the flag to false if the node has defined anything *)
  V.fold_on_node_and_expr (fun (ni, _nd) e () ->
    let lvals = Lrvalue.lvalues_of_expr e in
    (* TODO: filter just Locals here! *)
    if lvals <> [] (* less: and ExprStmt node? why? *)
    then arr.(ni) <- false
  ) flow ();

  (* Now flag the def if it is ever used on rhs *)
  V.fold_on_node_and_expr (fun (ni, _nd) e () ->
     let rvals = Lrvalue.rvalues_of_expr e in
     (* TODO: filter just local here! *)
     let vars = rvals |> List.map (fun ((s,_tok), _idinfo) -> s) in
     vars |> List.iter (fun var ->
       let in_env = mapping.(ni).D.in_env in
       (try
         let ns = VarMap.find var in_env in
         NodeiSet.iter (fun n -> arr.(n) <- true) ns
       with Not_found -> 
         pr (spf "%s: Undefined variable %s" (string_of_ni flow ni) var)
       );
     );
  ) flow ();

  arr |> Array.iteri (fun i x ->
    if (not x)
    then pr (spf "%s: Dead Assignment" (string_of_ni flow i));
  )
*)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let check_func_def fdef =
  try 
    let flow = Controlflow_build.cfg_of_func fdef in
    unreachable_statement_detection flow;

    let mapping = Dataflow_liveness.fixpoint flow in
    dead_assign_detection flow mapping;
(* TODO    use_of_uninitialized flow mapping; *)

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
