(* Yoann Padioleau
 *
 * Copyright (C) 2019 r2c
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *)
open Common
open Parser_go

module Flag = Flag_parsing
module T = Parser_go
module TH = Token_helpers_go
module F = Ast_fuzzy

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The goal for this module is to retag tokens (e.g, a LBRACE in LBODY),
 * or insert tokens (e.g., implicit semicolons) to help the grammar 
 * remains simple and unambiguous. 
 * See lang_cpp/parsing/parsing_hacks.ml for more information about
 * this technique.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type env_lbody = 
  | InIfHeader
  | Normal

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* ASI *)
(*****************************************************************************)

let fix_tokens_asi xs =
  let env = () in
  let rec aux env xs = 
    match xs with
    | [] -> []

    (* ASI: automatic semicolon insertion, similar in Javascript *)
    | ((LNAME _ 
      | LINT _ | LFLOAT _ | LIMAG _ | LRUNE _ | LSTR _
      | LBREAK _ | LCONTINUE _ | LFALL _ | LRETURN _
      | LINC _ | LDEC _
      | RPAREN _ 
      | RBRACE _ 
      | RBRACKET _
      ) as x) ::((TCommentNewline ii | EOF ii) as y)::xs ->
          let iifake = Parse_info.rewrap_str "FAKE ';'" ii in
          (* implicit semicolon insertion *)
          x::LSEMICOLON iifake::y::aux env xs

    | x::xs -> x::aux env xs
  in
  aux env xs

(*****************************************************************************)
(* LBODY *)
(*****************************************************************************)
let fix_tokens_lbody toks =
 try 
  let trees = Lib_ast_fuzzy.mk_trees { Lib_ast_fuzzy.
     tokf = TH.info_of_tok;
     kind = TH.token_kind_of_tok;
  } toks 
  in
  let retag_lbrace = Hashtbl.create 101 in

  let rec aux env trees =
      match trees with
      | [] -> ()
      | (F.Braces (lb, xs, _rb))::ys ->
          if env = InIfHeader
          then Hashtbl.add retag_lbrace lb true;
          aux Normal xs;
          aux Normal ys;
      | F.Tok (("if" | "for" | "switch" | "select"), _)::xs ->
          aux InIfHeader xs

      | x::xs -> 
          (match x with
          | F.Parens (_, xs, _) ->
                xs |> List.iter (function
                  | Left trees -> aux Normal trees
                  | Right _comma -> ()
                )
           | _ -> ()
          );
          aux env xs
  in
  aux Normal trees;

  (* use the tagged information and transform tokens *)
  toks |> List.map (function
    | T.LBRACE info when Hashtbl.mem retag_lbrace info ->
      T.LBODY (info)
    | x -> x
  )

  with Lib_ast_fuzzy.Unclosed (msg, info) ->
   if !Flag.error_recovery
   then toks
   else raise (Parse_info.Lexical_error (msg, info))


(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let fix_tokens xs =
  let xs = fix_tokens_asi xs in
  fix_tokens_lbody xs