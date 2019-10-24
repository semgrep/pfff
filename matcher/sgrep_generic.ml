(* Yoann Padioleau
 *
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
module Ast = Ast_generic
module V = Visitor_ast_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* See https://github.com/facebook/pfff/wiki/Sgrep 
 *
 *)

(*****************************************************************************)
(* Type *)
(*****************************************************************************)

(* right now only Expr are actually supported *)
type pattern = Ast.any

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let sgrep_ast ~hook pattern ast =
  let hook =
    match pattern with

    | E pattern_expr ->
        { V.default_visitor with
          V.kexpr = (fun (k, _) x ->
            let matches_with_env =
              Matching_generic.match_e_e pattern_expr  x
            in
            if matches_with_env = []
            then k x
            else begin
              (* could also recurse to find nested matching inside
               * the matched code itself.
               *)
              let matched_tokens = Lib_ast_generic.ii_of_any (E x) in
              matches_with_env +> List.iter (fun env ->
                hook env matched_tokens
              )
            end
          );
        }

    | S pattern ->
        { V.default_visitor with
          V.kstmt = (fun (k, _) x ->
            let matches_with_env =
              Matching_generic.match_st_st pattern x
            in
            if matches_with_env = []
            then k x
            else begin
              (* could also recurse to find nested matching inside
               * the matched code itself.
               *)
              let matched_tokens = Lib_ast_generic.ii_of_any (S x) in
              matches_with_env +> List.iter (fun env ->
                hook env matched_tokens
              )
            end
          );
        }

    | _ -> failwith (spf "pattern not yet supported:" )
  in
  (* opti ? dont analyze func if no constant in it ?*)
  (V.mk_visitor hook) (Pr ast)

(*
let sgrep ~hook pattern file =
  let ast = 
    try 
      let cst = Parse_js.parse_program file in
      Ast_js_build.program cst
    with Parse_js.Parse_error _ | Lexer_js.Lexical_error _ | Common.Todo ->
      (* we usually do sgrep on a set of files or directories,
       * so we don't want on error in one file to stop the
       * whole process.
       *)
      Common.pr2 (spf "warning: parsing problem in %s" file);
      []
  in
  sgrep_ast ~hook pattern ast
*)