(* Yoann Padioleau
 * 
 * Copyright (C) 2011 Facebook
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

module Ast = Ast_css
module Flag = Flag_parsing
module T = Parser_css
module TH   = Token_helpers_css
module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Lots of copy paste with my other parsers (e.g. C++, PHP, sql) but
 * copy paste is sometimes ok.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type program2 = Ast.stylesheet * T.token list

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let error_msg_tok tok = 
  Parse_info.error_message_info (TH.info_of_tok tok)

(*****************************************************************************)
(* Lexing only *)
(*****************************************************************************)

let tokens2 file = 
  let table     = Parse_info.full_charpos_to_pos_large file in

  Common.with_open_infile file (fun chan -> 
    let lexbuf = Lexing.from_channel chan in

      let mltoken lexbuf = 
        Lexer_css.token lexbuf
      in
      let rec tokens_aux acc = 
        let tok = mltoken lexbuf in
        if !Flag.debug_lexer then Common.pr2_gen tok;

        let tok = tok |> TH.visitor_info_of_tok (fun ii -> 
        { ii with PI.token=
          (* could assert pinfo.filename = file ? *)
           match ii.PI.token with
           | PI.OriginTok pi ->
               PI.OriginTok 
                 (PI.complete_token_location_large file table pi)
           | _ -> raise Todo
        })
        in
        if (match tok with T.EOF _ -> true | _ -> false)
        then List.rev (tok::acc)
        else tokens_aux (tok::acc)
      in
      tokens_aux []
  )

let tokens a = 
  Common.profile_code "Parse_css.tokens" (fun () -> tokens2 a)

(*****************************************************************************)
(* Helper for main entry point *)
(*****************************************************************************)

(* Hacked lex. This function use refs passed by parse.
 * 'tr' means 'token refs'.
 *)
let rec lexer_function tr = fun lexbuf ->
  match tr.PI.rest with
  | [] -> (pr2 "LEXER: ALREADY AT END"; tr.PI.current)
  | v::xs -> 
      tr.PI.rest <- xs;
      tr.PI.current <- v;
      tr.PI.passed <- v::tr.PI.passed;
      if (match v with T.TComment _ -> true | _ -> false)
      then lexer_function (*~pass*) tr lexbuf
      else v


(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let parse2 filename = 

  let toks = tokens filename in

  let tr = Parse_info.mk_tokens_state toks in
  let lexbuf_fake = Lexing.from_function (fun _buf _n -> raise Impossible) in

  (* -------------------------------------------------- *)
  (* Call parser *)
  (* -------------------------------------------------- *)
  try 
    (Common.profile_code "Parser_css.main" (fun () ->
      Parser_css.stylesheet (lexer_function tr) lexbuf_fake, toks
    ))
  with Parsing.Parse_error ->
    let current = tr.PI.current in

    if not !Flag.error_recovery 
    then raise (Parse_info.Parsing_error (TH.info_of_tok current));

    if !Flag.show_parsing_error
    then pr2 ("parse error \n = " ^ error_msg_tok current);
    [], toks

let parse a = 
  Common.profile_code "Parse_css.parse" (fun () -> parse2 a)
