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

open Highlight_code
module T = Parser_ruby

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Syntax highlighting for Ruby code for codemap (and now also efuns)
 *)

(*****************************************************************************)
(* Helpers when have global-analysis information *)
(*****************************************************************************)

(* we generate fake value here because the real one are computed in a
 * later phase in rewrite_categ_using_entities in pfff_visual.
 *)
let _def2 = Def2 NoUse
let _use2 = Use2 (NoInfoPlace, UniqueDef, MultiUse)

(*****************************************************************************)
(* Code highlighter *)
(*****************************************************************************)

let visit_program ~tag_hook _prefs (_program, toks) =

  (* tagger wrappers *)
  let already_tagged = Hashtbl.create 101 in
  let tag = (fun ii categ ->
    tag_hook ii categ;
    Hashtbl.replace already_tagged ii true
  )
  in
  let _tag_name (_s, ii) categ = 
    (* so treat the most specific in the enclosing code and then
     * do not fear to write very general case patterns later because
     * the specific will have priority over the general
     * (e.g., a Method use vs a Field use)
     *)
    if not (Hashtbl.mem already_tagged ii)
    then tag ii categ 
  in
  let _tag_if_not_tagged ii categ =
   if not (Hashtbl.mem already_tagged ii)    
   then tag ii categ
  in

  (* -------------------------------------------------------------------- *)
  (* AST phase 1 *) 
  (* -------------------------------------------------------------------- *)

  (* -------------------------------------------------------------------- *)
  (* tokens phase 1 (list of tokens) *)
  (* -------------------------------------------------------------------- *)

  (* -------------------------------------------------------------------- *)
  (* Tokens phase 2 (individual tokens) *)
  (* -------------------------------------------------------------------- *)

  toks |> List.iter (fun tok -> 
    match tok with

    (* specials *)
    | T.T_EOF _ii -> 
       ()

    (* comments *)

    (* values  *)

    (* ident  *)

    (* keywords  *)

    (* symbols *)

   | _ -> failwith "TODO"
  );
  ()
