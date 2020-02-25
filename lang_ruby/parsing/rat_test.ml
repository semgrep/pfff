
open Printf
open Ast

let process t = 
  eprintf "got %s\n" (NewParser.Dyp_symbols.str_token t)

let uniq_list lst =
  let rec u = function
    | [] -> []
    | [x] -> [x]
    | x1::x2::tl ->
	if (compare_ast x1 x2) = 0 
	then u (x1::tl) else x1 :: (u (x2::tl))
  in
  let l = List.map fst lst in
    u (List.sort compare_ast l)


let lextest lexbuf =
  let state = RubyLexerState.create NewLexer.top_lexer in
  while not lexbuf.Lexing.lex_eof_reached; do
    let t = NewLexer.token state lexbuf in
      if t = NewParser.T_EOF 
      then lexbuf.Lexing.lex_eof_reached <- true;
      process t
  done
      
let parsetest lexbuf = 
      let state = RubyLexerState.create NewLexer.top_lexer in
    try
      let asts = NewParser.main (NewLexer.token state) lexbuf in
        (*Printf.eprintf "lexer stack: %d\n" 
          (Stack.length state.RubyLexerState.lexer_stack);*)
      let asts = uniq_list asts in
	eprintf "got %d uniq parse trees\n" (List.length asts);
(*
	List.iter (fun (ast) -> 
		     Printf.printf "%s\n\n"
		       (Ast_printer.string_of_ast ast)
		  ) asts;*)

	(*()*)
	List.iter Ast_printer.print_ast asts
    with
      | Failure _ (*"lexing: empty token"*)
      | Dyp.Syntax_error ->
        Printf.eprintf "lexer stack: %d\n" 
          (Stack.length state.RubyLexerState.lexer_stack);

	  eprintf "parse error at %s : %d '%s', abs pos: %d\n"
	    lexbuf.Lexing.lex_curr_p.Lexing.pos_fname
	    lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum
	    (Lexing.lexeme lexbuf)
	    lexbuf.Lexing.lex_abs_pos

let _ = 
  assert (Array.length Sys.argv > 1);
  (*let file = "setup2.rb" in*)
  let get_lexbuf () = 
    let file = Sys.argv.(1) in
    let ic = open_in file in 
    let lexbuf = Lexing.from_channel ic in
      lexbuf.Lexing.lex_curr_p <-
	{lexbuf.Lexing.lex_curr_p with
	   Lexing.pos_fname = file
	};
      lexbuf
  in
    begin try
      let _ = Sys.getenv "LEXTEST" in
	lextest (get_lexbuf());
    with Not_found -> ()
    end;
    parsetest (get_lexbuf())
(*    with
      | Failure _ ->
	  eprintf "parse error at %s: %d: '%s'\n"
	    lexbuf.Lexing.lex_curr_p.Lexing.pos_fname
	    lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum
	    (Lexing.lexeme lexbuf)
*)
