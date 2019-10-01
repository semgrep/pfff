{
(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
 * Copyright (C) 2011-2015 Tomohiro Matsuyama
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

open Lexing

module Flag = Flag_parsing
open Parser_python

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The Python lexer.
 *
 * original src:
 *  https://github.com/m2ym/ocaml-pythonlib/blob/master/src/lexer.mll
 * old src: 
 *  - http://inst.eecs.berkeley.edu/~cs164/sp10/python-grammar.html
 *    which was itself from the python reference manual at:
 *    http://docs.python.org/release/2.5.4/ref/ref.html
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
exception Lexical_error of string * Parse_info.info

let tok     lexbuf  = 
  Lexing.lexeme lexbuf
let tokinfo lexbuf  = 
  Parse_info.tokinfo_str_pos (Lexing.lexeme lexbuf) (Lexing.lexeme_start lexbuf)

let error s lexbuf =
  if !Flag.exn_when_lexical_error
  then raise (Lexical_error (s, tokinfo lexbuf))
  else
    if !Flag.verbose_lexing
    then pr2_once ("LEXER: " ^ s)
    else ()


let unescaped s =
  let buf = Buffer.create (String.length s) in
  let escape = ref false in
  let unescapechar c =
    if !escape then begin
      match c with
      | '\r' -> ()
      | '\n' -> escape := false
      | _ -> begin
          escape := false;
          (* TODO http://docs.python.org/reference/lexical_analysis.html#string-literals *)
          Buffer.add_char
            buf
            (match c with
             | '\\' -> '\\'
             | '\'' -> '\''
             | '"' -> '"'
             | 'a' -> Char.chr 7
             | 'b' -> '\b'
             | 'f' -> Char.chr 12
             | 'n' -> '\n'
             | 'r' -> '\r'
             | 't' -> '\t'
             | 'v' -> Char.chr 11
             | _ -> (Buffer.add_char buf '\\'; c))
        end
    end else if c = '\\' then
      escape := true
    else
      Buffer.add_char buf c
  in
    String.iter unescapechar s;
    Buffer.contents buf

let count_lines s =
  let n = ref 0 in
    String.iter
      (fun c -> if c = '\n' then incr n)
      s;
    !n

(* ---------------------------------------------------------------------- *)
(* Keywords *)
(* ---------------------------------------------------------------------- *)

(* ---------------------------------------------------------------------- *)
(* Lexer state *)
(* ---------------------------------------------------------------------- *)

type lexer_state = {
  mutable curr_offset : int;
  offset_stack : int Stack.t;
  mutable nl_ignore : int;
}

let create () =
  let stack = Stack.create () in
  Stack.push 0 stack;
  { curr_offset = 0;
    offset_stack = stack;
    nl_ignore = 0 
  }

let ignore_nl t =
  t.nl_ignore <- succ t.nl_ignore

and aware_nl t =
  t.nl_ignore <- pred t.nl_ignore

}

(*****************************************************************************)
(* Regexp aliases *)
(*****************************************************************************)

(* epsilon *)
let e = ""

let newline = ('\n' | "\r\n")
let whitespace = [' ' '\t']
let comment = '#' [^ '\n' '\r']*

let digit = ['0'-'9']
let octdigit = ['0'-'7']
let hexdigit = ['0'-'9' 'a'-'f' 'A'-'F']
let nonzerodigit = ['1'-'9']

let longintpostfix = ['l' 'L']
let decimalinteger = nonzerodigit digit*
let octinteger = '0' octdigit+
let hexinteger = '0' ['x' 'X'] hexdigit+

let intpart = digit+
let fraction = '.' digit+
let pointfloat = intpart? fraction | intpart '.'
let exponent = ['e' 'E'] ['+' '-']? digit+
let exponentfloat = (intpart | pointfloat) exponent
let floatnumber = pointfloat | exponentfloat
let imagnumber = (floatnumber | intpart) ['j' 'J']

let stringprefix = ('u' | 'U')? ('r' | 'R')?
let escapeseq = '\\' _

let identifier = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let nonidchar = [^ 'a'-'z' 'A'-'Z' '0'-'9' '_']

(*****************************************************************************)
(* Rule initial *)
(*****************************************************************************)

rule token state = parse
  | e { 
        let curr_offset = state.curr_offset in
        let last_offset = Stack.top state.offset_stack in
          if curr_offset < last_offset
          then (ignore (Stack.pop state.offset_stack); DEDENT)
          else if curr_offset > last_offset
          then (Stack.push curr_offset state.offset_stack; INDENT)
          else _token state lexbuf 
      }

and offset state = parse
  | e { }
  | ' '  { state.curr_offset <- state.curr_offset + 1; offset state lexbuf }
  | '\t' { state.curr_offset <- state.curr_offset + 8; offset state lexbuf }

and _token state = parse

  (* ----------------------------------------------------------------------- *)
  (* spacing/comments *)
  (* ----------------------------------------------------------------------- *)
  | ((whitespace* comment? newline)* whitespace* comment?) newline
      { 
        let lines = count_lines (Lexing.lexeme lexbuf) in
        let pos = lexbuf.lex_curr_p in
          lexbuf.lex_curr_p <-
            { pos with
                pos_bol = pos.pos_cnum;
                pos_lnum = pos.pos_lnum + lines };
        if state.nl_ignore <= 0 then begin
          state.curr_offset <- 0;
          offset state lexbuf;
          NEWLINE
        end else
          _token state lexbuf 
       }
  | '\\' newline whitespace*
      { 
          let pos = lexbuf.lex_curr_p in
          lexbuf.lex_curr_p <-
            { pos with
                pos_bol = pos.pos_cnum;
                pos_lnum = pos.pos_lnum + 1 };
          _token state lexbuf 
      }

  | whitespace+
      { _token state lexbuf }

  (* ----------------------------------------------------------------------- *)
  (* symbols *)
  (* ----------------------------------------------------------------------- *)
  (* symbols *)
  | "+="    { ADDEQ (tokinfo lexbuf) }
  | "-="    { SUBEQ (tokinfo lexbuf) }
  | "*="    { MULTEQ (tokinfo lexbuf) }
  | "/="    { DIVEQ (tokinfo lexbuf) }
  | "%="    { MODEQ (tokinfo lexbuf) }
  | "**="   { POWEQ (tokinfo lexbuf) }
  | "//="   { FDIVEQ (tokinfo lexbuf) }
  | "&="    { ANDEQ (tokinfo lexbuf) }
  | "|="    { OREQ (tokinfo lexbuf) }
  | "^="    { XOREQ (tokinfo lexbuf) }
  | "<<="   { LSHEQ (tokinfo lexbuf) }
  | ">>="   { RSHEQ (tokinfo lexbuf) }

  | "=="    { EQUAL (tokinfo lexbuf) }
  | "!="    { NOTEQ (tokinfo lexbuf) }
  | "<>"    { NOTEQ (tokinfo lexbuf) }
  | "<="    { LEQ (tokinfo lexbuf) }
  | ">="    { GEQ (tokinfo lexbuf) }
  | '<'     { LT (tokinfo lexbuf) }
  | '>'     { GT (tokinfo lexbuf) }

  | '='     { EQ (tokinfo lexbuf) }

  | "**"    { POW (tokinfo lexbuf) }
  | "//"    { FDIV (tokinfo lexbuf) }

  | '+'     { ADD (tokinfo lexbuf) }
  | '-'     { SUB (tokinfo lexbuf) }
  | '*'     { MULT (tokinfo lexbuf) }
  | '/'     { DIV (tokinfo lexbuf) }
  | '%'     { MOD (tokinfo lexbuf) }

  | '|'     { BITOR (tokinfo lexbuf) }
  | '&'     { BITAND (tokinfo lexbuf) }
  | '^'     { BITXOR (tokinfo lexbuf) }
  | '~'     { BITNOT (tokinfo lexbuf) }

  | "<<"    { LSHIFT (tokinfo lexbuf) }
  | ">>"    { RSHIFT (tokinfo lexbuf) }

  | '('     { ignore_nl state; LPAREN (tokinfo lexbuf) }
  | ')'     { aware_nl state; RPAREN (tokinfo lexbuf) }
  | '['     { ignore_nl state; LBRACK (tokinfo lexbuf) }
  | ']'     { aware_nl state; RBRACK (tokinfo lexbuf) }
  | '{'     { ignore_nl state; LBRACE (tokinfo lexbuf) }
  | '}'     { aware_nl state; RBRACE (tokinfo lexbuf) }

  | ':'     { COLON (tokinfo lexbuf) }
  | ';'     { SEMICOL (tokinfo lexbuf) }
  | '.'     { DOT (tokinfo lexbuf) }
  | ','     { COMMA (tokinfo lexbuf) }
  | '`'     { BACKQUOTE (tokinfo lexbuf) }
  | '@'     { AT (tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* Keywords and ident *)
  (* ----------------------------------------------------------------------- *)

  (* keywords *)
  | identifier as id
      { match id with
        | "and"      -> AND (tokinfo lexbuf)
        | "as"       -> AS (tokinfo lexbuf)
        | "assert"   -> ASSERT (tokinfo lexbuf)
        | "break"    -> BREAK (tokinfo lexbuf)
        | "class"    -> CLASS (tokinfo lexbuf)
        | "continue" -> CONTINUE (tokinfo lexbuf)
        | "def"      -> DEF (tokinfo lexbuf)
        | "del"      -> DEL (tokinfo lexbuf)
        | "elif"     -> ELIF (tokinfo lexbuf)
        | "else"     -> ELSE (tokinfo lexbuf)
        | "except"   -> EXCEPT (tokinfo lexbuf)
        | "exec"     -> EXEC (tokinfo lexbuf)
        | "finally"  -> FINALLY (tokinfo lexbuf)
        | "for"      -> FOR (tokinfo lexbuf)
        | "from"     -> FROM (tokinfo lexbuf)
        | "global"   -> GLOBAL (tokinfo lexbuf)
        | "if"       -> IF (tokinfo lexbuf)
        | "import"   -> IMPORT (tokinfo lexbuf)
        | "in"       -> IN (tokinfo lexbuf)
        | "is"       -> IS (tokinfo lexbuf)
        | "lambda"   -> LAMBDA (tokinfo lexbuf)
        | "not"      -> NOT (tokinfo lexbuf)
        | "or"       -> OR (tokinfo lexbuf)
        | "pass"     -> PASS (tokinfo lexbuf)
        | "print"    -> PRINT (tokinfo lexbuf)
        | "raise"    -> RAISE (tokinfo lexbuf)
        | "return"   -> RETURN (tokinfo lexbuf)
        | "try"      -> TRY (tokinfo lexbuf)
        | "while"    -> WHILE (tokinfo lexbuf)
        | "with"     -> WITH (tokinfo lexbuf)
        | "yield"    -> YIELD (tokinfo lexbuf)
        | _          -> NAME (id, (tokinfo lexbuf)) }

  (* ----------------------------------------------------------------------- *)
  (* Constant *)
  (* ----------------------------------------------------------------------- *)

  (* literals *)
  | decimalinteger as n longintpostfix
      { LONGINT (int_of_string n, tokinfo lexbuf) }
  | decimalinteger as n
      { INT (int_of_string n, tokinfo lexbuf) }
  | octinteger as n longintpostfix
      { LONGINT (int_of_string ("0o" ^ n), tokinfo lexbuf) }
  | octinteger as n
      { INT (int_of_string ("0o" ^ n), tokinfo lexbuf) }
  | hexinteger as n longintpostfix
      { LONGINT (int_of_string n, tokinfo lexbuf) }
  | hexinteger as n
      { INT (int_of_string n, tokinfo lexbuf) }
  | floatnumber as n
      { FLOAT (float_of_string n, tokinfo lexbuf) }
  | imagnumber as n
      { IMAG (n, tokinfo lexbuf) }
  | '0' longintpostfix
      { LONGINT (0, tokinfo lexbuf) }
  | '0'
      { INT (0, tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* Strings *)
  (* ----------------------------------------------------------------------- *)

  | stringprefix '\''
      { sq_shortstrlit state (tokinfo lexbuf) lexbuf }
  | stringprefix '"'
      { dq_shortstrlit state (tokinfo lexbuf) lexbuf }
  | stringprefix "'''"
      { sq_longstrlit state (tokinfo lexbuf) lexbuf }
  | stringprefix "\"\"\""
      { dq_longstrlit state (tokinfo lexbuf) lexbuf }

  (* ----------------------------------------------------------------------- *)
  (* eof *)
  (* ----------------------------------------------------------------------- *)
  (* eof *)
  | eof { EOF (tokinfo lexbuf) }

  | _ { 
      if !Flag.verbose_lexing 
      then pr2_once ("LEXER:unrecognised symbol, in token rule:"^tok lexbuf);
      TUnknown (tokinfo lexbuf)
    }

(*****************************************************************************)
(* Rules on strings *)
(*****************************************************************************)

and sq_shortstrlit state pos = parse
  | (([^ '\\' '\r' '\n' '\''] | escapeseq)* as s) '\'' { STR (unescaped s, pos) }

and sq_longstrlit state pos = shortest
| (([^ '\\'] | escapeseq)* as s) "'''"
    { let lines = count_lines s in
      let curpos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <- { curpos with pos_lnum = curpos.pos_lnum + lines };
        STR (unescaped s, pos) }

and dq_shortstrlit state pos = parse
  | (([^ '\\' '\r' '\n' '\"'] | escapeseq)* as s) '"' { STR (unescaped s, pos) }

and dq_longstrlit state pos = shortest
  | (([^ '\\'] | escapeseq)* as s) "\"\"\""
      { let lines = count_lines s in
        let curpos = lexbuf.lex_curr_p in
          lexbuf.lex_curr_p <- { curpos with pos_lnum = curpos.pos_lnum + lines };
          STR (unescaped s, pos) }
