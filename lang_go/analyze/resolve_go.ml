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
open Ast_go
module Ast = Ast_go
module V = Visitor_go
module G = Ast_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Identifiers tagger (so we can colorize them differently in codemap/efuns).
 *
 * mostly copy paste of resolve_python.ml
 * TODO: generalize for ast_generic at some point? hard? better to do on
 *  lang-specific AST?
 *)

(*****************************************************************************)
(* Type *)
(*****************************************************************************)
type resolved_name = Ast_generic.resolved_name

type env = {
  (* ctx: context ref; *)
  names: (string * resolved_name) list ref;
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* because we use a Visitor instead of a clean recursive 
 * function passing down an environment, we need to emulate a scoped
 * environment by using save_excursion.
 *)
let _with_added_env xs env f = 
  let newnames = xs @ !(env.names) in
  Common.save_excursion env.names newnames f

let add_name_env name kind env =
  env.names := (Ast.str_of_id name, kind)::!(env.names)

let default_env () = {
(*  ctx = ref AtToplevel; *)
  names = ref [];
}

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let resolve prog =
  let env = default_env () in

  (* would be better to use a classic recursive with environment visit *)
  let visitor = V.mk_visitor { V.default_visitor with
    (* No need to resolve at the definition sites (for parameters, locals).
     * This will be patterned-match specially anyway in the highlighter. What
     * you want is to tag the use sites, and to maintain the right environment.
     *)

    (* defs *)
    V.kprogram = (fun (k, _) x ->

      let file = Parse_info.file_of_info (snd x.package), snd x.package in
      add_name_env x.package (G.ImportedModule (G.FileName file)) env;
      x.imports |> List.iter (fun { i_path = (path, ii); i_kind = kind } ->
          match kind with
          | ImportOrig -> 
            add_name_env (Filename.basename path, ii) 
              (G.ImportedModule (G.FileName (path,ii))) env
          | ImportNamed id -> 
            add_name_env id
              (G.ImportedModule (G.FileName (path,ii))) env
          | ImportDot _ -> ()
      );
      k x

    );
    V.ktop_decl = (fun (k, _) x ->
      k x
    );

    (* uses *)
    V.kexpr = (fun (k, _) x ->
      (match x with
      | Id (id, resolved) ->
        let s = Ast.str_of_id id in
        (match List.assoc_opt s !(env.names) with
          | Some x -> resolved := Some x
          | None -> () (* will be tagged as Error by highlighter later *)
        )
      | _ -> ()
      );
      k x
    );

  } in
  visitor (P prog)
