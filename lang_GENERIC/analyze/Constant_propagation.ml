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
open AST_generic
module Ast = AST_generic
module V = Visitor_AST

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Very basic constant propagation (no dataflow analysis involved).
 *
 * This is mainly to provide advanced features to semgrep such as the 
 * constant propagation of literals.
 *
 * Right now we just propagate constants when we're sure it's a constant
 * because:
 *  - the variable declaration use the 'const' keyword in Javascript/Go/...
 *  - TODO the field declaration use the 'final' keyword in Java
 *  - TODO we do a very basic const analysis where we check the variable
 *    is used only in an rvalue context (never assigned).
 *
 * history: this used to be in Naming_AST.ml but better to split, even though
 * things will be slightly slower because we will visit the same file
 * twice.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type env = {

  (* basic constant propagation of literals for sgrep *)
  constants: (string * sid, literal) assoc ref;
}

let default_env () = {
  constants = ref [];
}

(*****************************************************************************)
(* Environment Helpers *)
(*****************************************************************************)

let add_constant_env ident (sid, literal) env =
  env.constants := 
  ((Ast.str_of_ident ident, sid), literal)::!(env.constants)



(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let propagate _lang prog =
  let env = default_env () in

  let visitor = V.mk_visitor { V.default_visitor with
    (* the defs *)

    V.kdef = (fun (k, _v) x ->
      match x with
      | { name = id; 
          info = { id_resolved = {contents = Some (_kind, sid)}; _} as id_info;
          attrs = attrs; _}, 
        (* note that some languages such as Python do not have VarDef.
         * todo? should add those somewhere instead of in_lvalue detection? *)
        VarDef ({ vinit = Some (L literal); _ }) ->
          if Ast.has_keyword_attr Const attrs
          then begin
              id_info.id_const_literal := Some literal;
              add_constant_env id (sid, literal) env;
          end;
          k x

      | _ -> k x
    );

    (* the uses *)

    V.kexpr = (fun (k, _) x ->

       (match x with
       | Id (id, ({ id_resolved = {contents = Some (_kind, sid)}; _ }
                    as id_info))->
             let s = Ast.str_of_ident id in
             (match List.assoc_opt (s, sid) !(env.constants) with
             | Some (literal) ->
                 id_info.id_const_literal := Some literal
             | _ -> ()
             );
       | _ -> ()
       );
       k x
   );
  }
  in
  visitor (Pr prog);
  ()
