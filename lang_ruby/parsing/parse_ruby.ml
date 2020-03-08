(* Mike Furr
 *
 * Copyright (C) 2010 Mike Furr
 * Copyright (C) 2020 r2c
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of the <organization> nor the
 *       names of its contributors may be used to endorse or promote products
 *       derived from this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)
open Common
module H = Ast_ruby_helpers
module HH = Parser_ruby_helpers
module Utils = Utils_ruby
module PI = Parse_info
module TH = Token_helpers_ruby

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let uniq_list lst =
  let rec u = function
    | [] -> []
    | [x] -> [x]
    | x1::x2::tl ->
      if H.equal_ast x1 x2
      then u (x1::tl) 
      else x1 :: (u (x2::tl))
  in
  let l = List.map fst lst in
  u (List.sort H.compare_ast l)


(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let parse_program file = 
  (* todo: reuse Parse_info.tokenize_all_and_adjust_pos *)
  Common.with_open_infile file (fun chan -> 
    let lexbuf = Lexing.from_channel chan in
    let state = Lexer_parser_ruby.create Lexer_ruby.top_lexer in 
    let table     = Parse_info.full_charpos_to_pos_large file in

    let adjust_info ii = 
      { ii with PI.token =
        (* could assert pinfo.filename = file ? *)
         match ii.PI.token with
         | PI.OriginTok pi -> PI.OriginTok
           (PI.complete_token_location_large file table pi)
         | _ -> raise Todo
      }      
    in

    try 
      HH.clear_env ();
      let env = Utils.default_opt Utils.StrSet.empty None in
      HH.set_env env;
    
      let lexer lexbuf = 
        let tok = 
         try
           Lexer_ruby.token state lexbuf
         with PI.Lexical_error (s, info) ->
           raise (PI.Lexical_error (s, adjust_info info))
        in 
        if !Flag_parsing.debug_lexer 
        then Common.pr2_gen tok;

       let tok = tok |> TH.visitor_info_of_tok adjust_info in
       tok
      in
    
      let lst = Parser_ruby.main lexer lexbuf in
      let lst = uniq_list lst in
      (match lst with
      | [ast] -> (*Ast.mod_ast (replace_heredoc state) ast*) 
              ast
      | _l -> failwith "ambiguous parse"
      )
    with Dyp.Syntax_error ->
      let msg = Printf.sprintf "parse error in file %s, line: %d, token: '%s'\n"
        lexbuf.Lexing.lex_curr_p.Lexing.pos_fname
        lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum
        (Lexing.lexeme lexbuf)
      in
      failwith msg
  )
