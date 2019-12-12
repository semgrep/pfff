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

open Parser_go

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Insert implicit semicolons.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type env = {
 loophack: bool list;
}

(*****************************************************************************)
(* Fix tokens *)
(*****************************************************************************)

let fix_tokens xs =
  let rec aux env xs = 
    match xs with
    | [] -> []
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
          let env = 
            match x with
            | RPAREN _ | RBRACKET _ -> { loophack = List.tl env.loophack }
            | _ -> env
          in
          x::LSEMICOLON iifake::y::aux env xs

    | ((LPAREN _ | LBRACKET _) as x)::xs ->
        x::aux { loophack = false::env.loophack } xs
    | ((RPAREN _ | RBRACKET _) as x)::xs ->
        x::aux { loophack = List.tl env.loophack } xs

    | LBRACE ii::xs  ->
       (match env.loophack with 
       | true::rest ->
          LBODY ii::aux { loophack = false::rest } xs
       | _ -> 
          LBRACE ii::aux env xs
       )

    | ((LFOR _ | LIF _ | LSWITCH _ | LSELECT _) as x)::xs ->
       (match env.loophack with 
       | _::rest ->
          x::aux { loophack = true::rest } xs
       | [] -> 
         raise Common.Impossible
       )
        

    | x::xs -> x::aux env xs
  in
  aux { loophack = [false] } xs

