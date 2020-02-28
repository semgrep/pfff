module H = Ast_ruby_helpers
module HH = Parser_ruby_helpers
module Utils = Utils_ruby


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


let parse_lexbuf_with_state ?env state lexbuf = 
  try 
    HH.clear_env ();
    let env = Utils.default_opt Utils.StrSet.empty env in
    let () = HH.set_env env in
    let lst = Parser_ruby.main (Lexer_ruby.token state) lexbuf in
    let lst = uniq_list lst in
      begin match lst with
        | [ast] -> (*Ast.mod_ast (replace_heredoc state) ast*) 
            ast
        | _l -> failwith "ambiguous parse"
      end
  with Dyp.Syntax_error ->
    let msg = Printf.sprintf "parse error in file %s, line: %d, token: '%s'\n"
      lexbuf.Lexing.lex_curr_p.Lexing.pos_fname
      lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum
      (Lexing.lexeme lexbuf)
    in
      failwith msg

let parse_lexbuf lexbuf = 
  let state = Lexer_parser_ruby.create Lexer_ruby.top_lexer in 
  parse_lexbuf_with_state state lexbuf

let parse_file fname = 
  let ic = open_in fname in
  let lexbuf = Lexing.from_channel ic in
  try
      let res = parse_lexbuf lexbuf in
        close_in ic;
        res
  with e -> close_in ic; raise e

(* entry point *)
let parse_program file = 
  parse_file file
