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
(* Fix tokens *)
(*****************************************************************************)

let fix_tokens xs =
  let rec aux env xs = 
    match xs with
    | [] -> []
    | x::y::xs ->
        (match x, y with
        | (LNAME _ 
          | LINT _ | LFLOAT _ | LIMAG _ | LRUNE _ | LSTR _
          | LBREAK _ | LCONTINUE _ | LFALL _ | LRETURN _
          | LINC _ | LDEC _
          | RPAREN _ | RBRACE _ | RBRACKET _
          ), (TCommentNewline ii | EOF ii) ->
          let iifake = ii in (* TODO generate fake from ii? *)
          (* implicit semicolon insertion *)
          x::LSEMICOLON iifake::y::aux env xs
        | _ -> x::aux env (y::xs)
        )
    | [x] -> [x]


  in
  aux () xs

