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

open Ast_generic
module V = Visitor_ast
module E = Error_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Most of the hard work is done by Controlflow_build_generic.ml.
 * 
 * TODO: check dead statements for toplevel blocks ?
 *)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let check_program2 prog =
  let visitor = V.mk_visitor { V.default_visitor with
    (* also valid for methods *)
    V.kdef = (fun (k, _) x ->
      let (_entity, def) = x in
      (match def with
      | FuncDef fdef ->
        (try 
          let flow = Controlflow_build.cfg_of_func fdef in
          Controlflow_build.deadcode_detection flow;
        with Controlflow_build.Error (err, loc) ->
          loc |> Common.do_option (fun loc ->
            let s = Controlflow_build.string_of_error_kind err in
            E.error loc (E.UnreachableStatement s);
          )
        )
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
