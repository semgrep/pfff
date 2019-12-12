(* Yoann Padioleau
 * 
 * Copyright (C) 2010 Facebook
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

module Flag = Flag_parsing
module TH   = Token_helpers_go
module PI = Parse_info
module Lexer = Lexer_go

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type program_and_tokens = 
  Ast_go.program option * Parser_go.token list

(*****************************************************************************)
(* Error diagnostic  *)
(*****************************************************************************)
let error_msg_tok tok = 
  Parse_info.error_message_info (TH.info_of_tok tok)

(*****************************************************************************)
(* Lexing only *)
(*****************************************************************************)

let tokens2 file = 
  let token lexbuf = 
      Lexer.token lexbuf
  in
  Parse_info.tokenize_all_and_adjust_pos 
    file token TH.visitor_info_of_tok TH.is_eof

let tokens a = 
  Common.profile_code "Parse_go.tokens" (fun () -> tokens2 a)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)
let parse2 filename = 
  let stat = Parse_info.default_stat filename in

  (* this can throw Parse_info.Lexical_error *)
  let toks = tokens filename in
  let tr, lexer, lexbuf_fake = 
    Parse_info.mk_lexer_for_yacc toks TH.is_comment in

  try 
    (* -------------------------------------------------- *)
    (* Call parser *)
    (* -------------------------------------------------- *)
    let xs =
      Common.profile_code "Parser_go.file" (fun () ->
        Parser_go.file  lexer lexbuf_fake
      )
    in
    stat.PI.correct <- (Common.cat filename |> List.length);
    (Some xs, toks), stat

  with Parsing.Parse_error ->

    let cur = tr.PI.current in
    if not !Flag.error_recovery
    then raise (PI.Parsing_error (TH.info_of_tok cur));

    if !Flag.show_parsing_error
    then begin
      pr2 ("parse error \n = " ^ error_msg_tok cur);

      let filelines = Common2.cat_array filename in
      let checkpoint2 = Common.cat filename |> List.length in
      let line_error = PI.line_of_info (TH.info_of_tok cur) in
      Parse_info.print_bad line_error (0, checkpoint2) filelines;
    end;

    stat.PI.bad     <- Common.cat filename |> List.length;
    (None, toks), stat

let parse a = 
  Common.profile_code "Parse_go.parse" (fun () -> parse2 a)

let parse_program file = 
  let ((astopt, _toks), _stat) = parse file in
  Common2.some astopt

(*****************************************************************************)
(* Sub parsers *)
(*****************************************************************************)

let (program_of_string: string -> Ast_go.program) = fun s -> 
  Common2.with_tmp_file ~str:s ~ext:"go" (fun file ->
    parse_program file
  )

(* for sgrep/spatch *)
let any_of_string s = 
  Common2.with_tmp_file ~str:s ~ext:"go" (fun file ->
    let toks = tokens file in
    let _tr, lexer, lexbuf_fake = PI.mk_lexer_for_yacc toks TH.is_comment in
    (* -------------------------------------------------- *)
    (* Call parser *)
    (* -------------------------------------------------- *)
    Parser_go.sgrep_spatch_pattern lexer lexbuf_fake
  )


(*****************************************************************************)
(* Fuzzy parsing *)
(*****************************************************************************)

(*
let parse_fuzzy file =
  let toks = tokens file in
  let trees = Parse_fuzzy.mk_trees { Parse_fuzzy.
     tokf = TH.info_of_tok;
     kind = TH.token_kind_of_tok;
  } toks 
  in
  trees, toks
*)

