(* Yoann Padioleau
 *
 * Copyright (C) 2010, 2013 Facebook
 * Copyright (C) 2019 Yoann Padioleau
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

module Flag = Flag_parsing
module Flag_js = Flag_parsing_js
module Ast = Ast_js
module TH   = Token_helpers_js
module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Lots of copy paste with my other parsers (e.g. PHP, C, ML) but
 * copy paste is sometimes ok.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* the token list contains also the comment-tokens *)
type program_and_tokens = 
  Ast_js.program option * Parser_js.token list

(*****************************************************************************)
(* Error diagnostic  *)
(*****************************************************************************)
let error_msg_tok tok = 
  Parse_info.error_message_info (TH.info_of_tok tok)

(*****************************************************************************)
(* Helpers  *)
(*****************************************************************************)
let rec first_non_comment_line xs =
  match xs with
  | [] -> None
  | x::xs ->
    if TH.is_comment x
    then first_non_comment_line xs
    else Some (TH.line_of_tok x)

(*****************************************************************************)
(* Lexing only *)
(*****************************************************************************)

let tokens2 file = 
  let table     = Parse_info.full_charpos_to_pos_large file in

  Common.with_open_infile file (fun chan -> 
    let lexbuf = Lexing.from_channel chan in

    Lexer_js.reset();
    try 
      let jstoken lexbuf = 
        match Lexer_js.current_mode() with
        | Lexer_js.ST_IN_CODE ->
            Lexer_js.initial lexbuf
        | Lexer_js.ST_IN_XHP_TAG current_tag ->
            Lexer_js.st_in_xhp_tag current_tag lexbuf
        | Lexer_js.ST_IN_XHP_TEXT current_tag ->
            Lexer_js.st_in_xhp_text current_tag lexbuf
        | Lexer_js.ST_IN_BACKQUOTE ->
            Lexer_js.backquote lexbuf
      in
      let rec tokens_aux acc = 
        let tok = jstoken lexbuf in
        if !Flag.debug_lexer 
        then Common.pr2_gen tok;

        if not (TH.is_comment tok)
        then Lexer_js._last_non_whitespace_like_token := Some tok;

        let tok = tok +> TH.visitor_info_of_tok (fun ii -> 
        { ii with PI.token =
          (* could assert pinfo.filename = file ? *)
            match ii.PI.token with
            | PI.OriginTok pi ->
              PI.OriginTok (PI.complete_token_location_large file table pi)
            | PI.FakeTokStr _ | PI.Ab  | PI.ExpandedTok _ ->
              raise Impossible
        })
        in

        if TH.is_eof tok
        then List.rev (tok::acc)
        else tokens_aux (tok::acc)
    in
    tokens_aux []
  with
  | Lexer_js.Lexical s -> 
    failwith ("lexical error " ^ s ^ "\n =" ^ 
                 (PI.error_message file (PI.lexbuf_to_strpos lexbuf)))
  | e -> raise e
 )

let tokens a = 
  Common.profile_code "Parse_js.tokens" (fun () -> tokens2 a)

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

      if TH.is_comment v (* || other condition to pass tokens ? *)
      then lexer_function (*~pass*) tr lexbuf
      else v

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

exception Parse_error of Parse_info.info

let parse2 filename =
  let stat = PI.default_stat filename in

  let toks = tokens filename in
  (*  let toks = Parsing_hacks_js.fix_tokens toks in *)

  let tr = PI.mk_tokens_state toks in
  let last_charpos_error = ref 0 in
  let lexbuf_fake = Lexing.from_function (fun _buf _n -> raise Impossible) in

   Sys.set_signal Sys.sigalrm (Sys.Signal_handle (fun _ -> raise Timeout ));
   ignore(Unix.alarm 5);

  let rec parse_module_item_or_eof tr =
    try 
     let item = 
       (* -------------------------------------------------- *)
       (* Call parser *)
       (* -------------------------------------------------- *)
       Common.profile_code "Parser_js.module_item" (fun () ->
         Parser_js.module_item_or_eof (lexer_function tr) lexbuf_fake
       )
     in
     Left item
   with 
   | Lexer_js.Lexical s ->
      let cur = tr.PI.current in
      if !Flag.show_parsing_error
      then pr2 ("lexical error " ^s^ "\n =" ^ error_msg_tok cur);
      Right ()

   | Parsing.Parse_error ->
      let cur = tr.PI.current in
      let info = TH.info_of_tok cur in
      let charpos = Parse_info.pos_of_info info in

      (* try Automatic Semicolon Insertion *)
      if charpos > !last_charpos_error && 
         tr.PI.passed <> [] &&
         (match first_non_comment_line (List.tl tr.PI.passed) with
         | None -> false
         | Some line -> 
            TH.line_of_tok cur > line ||
            (match cur with 
             | Parser_js.T_RCURLY _ -> true 
             | Parser_js.EOF _ -> true
             | _ -> false
             )
         )
      then begin
        match tr.PI.passed with
        | [] -> raise Impossible
        | x::xs ->
          assert (x = cur);
          let virtual_semi = 
            Parser_js.T_VIRTUAL_SEMICOLON (Ast.fakeInfoAttach info) in
          if !Flag_js.debug_asi
          then pr2 (spf "insertion fake ';' at %s" (PI.string_of_info info));
  
          let toks = List.rev xs @ [virtual_semi; x] @ tr.PI.rest in
          (* like in Parse_info.mk_tokens_state *)
          tr.PI.rest <- toks;
          tr.PI.current <- List.hd toks;
          tr.PI.passed <- [];
          (* try again! 
           * This significantly slow-down parsing, but at least it's parsing
           *)
          last_charpos_error := charpos;
          parse_module_item_or_eof tr
      end else begin
        if !Flag.show_parsing_error 
        then pr2 ("parse error \n = " ^ error_msg_tok cur);
        Right ()
      end
  in
  let rec aux tr =
    let line_start = TH.line_of_tok tr.PI.current in
    let res = parse_module_item_or_eof tr in
    let passed = tr.PI.passed in
    tr.PI.passed <- [];
    let lines =
      try 
        let (head, _middle, last) = Common2.head_middle_tail passed in
        let line1 = TH.line_of_tok last in
        let line2 = TH.line_of_tok head in
        line2 - line1 (* +1? *)
      with _ -> 1
    in
    match res with
    (* EOF *)
    | Left None -> 
        stat.PI.correct <- stat.PI.correct + lines;
        []
    | Left (Some x) -> 
        stat.PI.correct <- stat.PI.correct + lines;
        x::aux tr 
    | Right () ->
       let max_line = Common.cat filename +> List.length in
       if !Flag.show_parsing_error
       then begin
         let filelines = Common2.cat_array filename in
         let cur = tr.PI.current in
         let line_error = TH.line_of_tok cur in
         PI.print_bad line_error (line_start, min max_line (line_error + 10))
              filelines;
       end;
       (* todo? try to recover? call 'aux tr'? but then can be really slow *)
       stat.PI.bad <- stat.PI.bad + (max_line - line_start);
       []
  in
  let items = 
   try 
     aux tr 
   with Timeout ->
      ignore(Unix.alarm 0);
      if !Flag.show_parsing_error
      then pr2 (spf "TIMEOUT on %s" filename);
      stat.PI.bad <- Common.cat filename |> List.length;
      stat.PI.have_timeout <- true;
      stat.PI.correct <- 0;
      []
  in
  ignore(Unix.alarm 0);
  (Some items, []), stat

let parse a = 
  Common.profile_code "Parse_js.parse" (fun () -> parse2 a)

let parse_program file = 
  let ((astopt, _toks), _stat) = parse file in
  Common2.some astopt


let parse_string (w : string) : Ast.program =
  Common2.with_tmp_file ~str:w ~ext:"js" parse_program

(*****************************************************************************)
(* Sub parsers *)
(*****************************************************************************)

let (program_of_string: string -> Ast_js.program) = fun s -> 
  Common2.with_tmp_file ~str:s ~ext:"js" (fun file ->
    parse_program file
  )

(*****************************************************************************)
(* Fuzzy parsing *)
(*****************************************************************************)

let parse_fuzzy file =
  let toks = tokens file in
  let trees = Parse_fuzzy.mk_trees { Parse_fuzzy.
     tokf = TH.info_of_tok;
     kind = TH.token_kind_of_tok;
  } toks 
  in
  trees, toks
