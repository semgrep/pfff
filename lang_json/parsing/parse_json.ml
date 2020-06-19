
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
open Parse_js
module PI = Parse_info
module TH = Token_helpers_js
module Flag = Flag_parsing

let error_msg_tok tok = 
  Parse_info.error_message_info (TH.info_of_tok tok)

let parse_program filename =
  let toks = tokens filename in
  (* need need parsing hacks fix I think *)
  let tr, lexer, lexbuf_fake =
    PI.mk_lexer_for_yacc toks TH.is_comment in

  try 
    let e_cst = Parser_js.json lexer lexbuf_fake in
    Ast_js_build.expr e_cst
  with Parsing.Parse_error ->
      let cur = tr.PI.current in
      if !Flag.show_parsing_error 
      then pr2 ("parse error \n = " ^ error_msg_tok cur);
      raise (PI.Parsing_error (TH.info_of_tok cur))

let any_of_string str =
  match Parse_js.any_of_string str with
  | Cst_js.Expr e_cst ->
      Ast_json.E (Ast_js_build.expr e_cst)
  | _ -> failwith (spf "not a valid JSON expression: %s" str)
