
open Utils

let set_lexbuf_fname lexbuf name = 
  lexbuf.Lexing.lex_curr_p <-
    {lexbuf.Lexing.lex_curr_p with
       Lexing.pos_fname = name
    }

let set_lexbuf_lineno lexbuf line = 
  lexbuf.Lexing.lex_curr_p <-
    {lexbuf.Lexing.lex_curr_p with
       Lexing.pos_lnum = line
    }

let uniq_list lst =
  let rec u = function
    | [] -> []
    | [x] -> [x]
    | x1::x2::tl ->
	if Ast.equal_ast x1 x2
	then u (x1::tl) else x1 :: (u (x2::tl))
  in
  let l = List.map fst lst in
    u (List.sort Ast.compare_ast l)


(* determines if the given Ast.expr is a special case *)
let single_delims = (* copied from newLexer *)
  ['!'; '@'; '#'; '$'; '%'; '^'; '&'; '*';
   ';'; '.'; '?'; '`'; '~'; '|'; '+'; '_';
   '-'; '\\'; '/'; ':'; '"'; '\'']

(* return a string delimiter (%|..|) that is not used in the string *)
let free_delimiter str pos = 
  try List.find (fun d -> not (String.contains str d)) single_delims
  with Not_found -> 
    Printf.eprintf "%s:%d, need to try harder to wrap string in delimiter\n"
      pos.Lexing.pos_fname pos.Lexing.pos_lnum;
    exit(1)

let mk_block ast pos = match ast with
  | [e] -> e
  | lst -> Ast.E_Block(lst,pos)

(*
let rec replace_heredoc state ast = match ast with
  | Ast.E_Literal(Ast.Lit_String(Ast.String_Heredoc(i,interp)),pos) ->
      let s = Hashtbl.find state.RubyLexerState.heredoc_tbl i in
      let d = free_delimiter s pos in
      let full_s = 
        if interp then Printf.sprintf "%%%c%s%c" d s d
        else Printf.sprintf "%%q%c%s%c" d s d
      in
      let ast = parse_string_with_state state full_s in 
        mk_block ast pos
  | e -> e
*)

let rec parse_lexbuf_with_state ?env state lexbuf = 
  try 
    NewParser.clear_env ();
    let env = default_opt StrSet.empty env in
    let () = NewParser.set_env env in
    let lst = NewParser.main (NewLexer.token state) lexbuf in
    let lst = uniq_list lst in
      begin match lst with
        | [ast] -> (*Ast.mod_ast (replace_heredoc state) ast*) 
            ast
        | l -> failwith "ambiguous parse"
      end
  with Dyp.Syntax_error ->
    let msg = Printf.sprintf "parse error in file %s, line: %d, token: '%s'\n"
      lexbuf.Lexing.lex_curr_p.Lexing.pos_fname
      lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum
      (Lexing.lexeme lexbuf)
    in
      failwith msg

and parse_string_with_state state ?env ?filename ?lineno str = 
  let lexbuf = Lexing.from_string str in
  let fname = match filename with None -> str | Some x -> x in
  let line = match lineno with None -> 1 | Some x -> x in 
    set_lexbuf_fname lexbuf fname;
    set_lexbuf_lineno lexbuf line;
    parse_lexbuf_with_state ?env state lexbuf

let parse_lexbuf lexbuf = 
  let state = RubyLexerState.create NewLexer.top_lexer in 
    parse_lexbuf_with_state state lexbuf

let parse_string ?env ?filename ?lineno str = 
  let state = RubyLexerState.create NewLexer.top_lexer in 
    parse_string_with_state state ?env ?filename ?lineno str

let parse_file fname = 
  let ic = open_in fname in
  let lexbuf = Lexing.from_channel ic in
  let () = set_lexbuf_fname lexbuf fname in
    try
      let res = parse_lexbuf lexbuf in
        close_in ic;
        res
    with e -> close_in ic; raise e
