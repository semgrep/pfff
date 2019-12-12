{
(* Yoann Padioleau
 *
 * Copyright (C) 2019 r2c
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

open Parser_go
module PI = Parse_info
module Flag = Flag_parsing

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The Go lexer.
 *
 * reference:
 *    - https://golang.org/ref/spec#Lexical_elements
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* shortcuts *)
let tok = Lexing.lexeme
let tokinfo = Parse_info.tokinfo
let error = Parse_info.lexical_error

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
}

(*****************************************************************************)
(* Regexp aliases *)
(*****************************************************************************)

let newline = ('\n' | "\r\n")
let whitespace = [' ' '\t']

let digit = ['0'-'9']
let octdigit = ['0'-'7']
let hexdigit = ['0'-'9' 'a'-'f' 'A'-'F']

let digipart = digit (('_'? digit)* )

let integer =
  ('0' ['b' 'B'] ('_'? ['0'-'1'])+) | (* Binary. *)
  ('0' ['o' 'O'] ('_'? ['0'-'7'])+) | (* Octal. *)
  ('0' ['x' 'X'] ('_'? hexdigit)+) | (* Hexadecimal. *)
  (['1' - '9'] ('_'? digit)* | '0' ('_'? '0')* ) (* Decimal. *)


let intpart = digipart
let fraction = '.' digipart
let pointfloat = intpart? fraction | intpart '.'
let exponent = ['e' 'E'] ['+' '-']? digipart
let exponentfloat = (intpart | pointfloat) exponent
let floatnumber = pointfloat | exponentfloat

let imagnumber = (floatnumber | intpart) ['i']

let escapeseq = '\\' _

let identifier = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

(*****************************************************************************)
(* Rule initial *)
(*****************************************************************************)

rule token = parse

  (* ----------------------------------------------------------------------- *)
  (* spacing/comments *)
  (* ----------------------------------------------------------------------- *)
  | "/*" {
      let info = tokinfo lexbuf in
      let buf = Buffer.create 127 in
      Buffer.add_string buf "/*";
      comment buf lexbuf;
      TComment(info |> PI.rewrap_str (Buffer.contents buf))
    }

  (* don't keep the trailing \n; it will be in another token *)
  | "//" [^ '\r' '\n']+ { TComment (tokinfo lexbuf) }
  | newline     { TCommentNewline (tokinfo lexbuf) }
  | whitespace+ { TCommentSpace (tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* symbols *)
  (* ----------------------------------------------------------------------- *)

  | '+'     { LPLUS (tokinfo lexbuf) }
  | '-'     { LMINUS (tokinfo lexbuf) }
  | '*'     { LMULT (tokinfo lexbuf) }
  | '/'     { LDIV (tokinfo lexbuf) }
  | '%'     { LPERCENT (tokinfo lexbuf) }

  | "+=" as s   { LASOP (s, tokinfo lexbuf) }
  | "-=" as s   { LASOP (s, tokinfo lexbuf) }
  | "*=" as s   { LASOP (s, tokinfo lexbuf) }
  | "/=" as s   { LASOP (s, tokinfo lexbuf) }
  | "%=" as s   { LASOP (s, tokinfo lexbuf) }
  | "&=" as s   { LASOP (s, tokinfo lexbuf) }
  | "|=" as s   { LASOP (s, tokinfo lexbuf) }
  | "^=" as s   { LASOP (s, tokinfo lexbuf) }
  | "<<=" as s  { LASOP (s, tokinfo lexbuf) }
  | ">>=" as s  { LASOP (s, tokinfo lexbuf) }
  (* Go specific *)
  | "&^=" as s   { LASOP (s, tokinfo lexbuf) }

  | "=="    { LEQEQ (tokinfo lexbuf) }
  | "!="    { LNE (tokinfo lexbuf) }

  | "<="    { LLE (tokinfo lexbuf) }
  | ">="    { LGE (tokinfo lexbuf) }
  | '<'     { LLT (tokinfo lexbuf) }
  | '>'     { LGT (tokinfo lexbuf) }

  | '='     { LEQ (tokinfo lexbuf) }

  | '|'     { LPIPE (tokinfo lexbuf) }
  | '&'     { LAND (tokinfo lexbuf) }
  | '^'     { LHAT (tokinfo lexbuf) }
  | '~'     { LTILDE (tokinfo lexbuf) }
  | '!'     { LBANG (tokinfo lexbuf) }

  | "<<"    { LLSH (tokinfo lexbuf) }
  | ">>"    { LRSH (tokinfo lexbuf) }

  | "++"    { LINC (tokinfo lexbuf) }
  | "--"    { LDEC (tokinfo lexbuf) }

  | "<-"    { LCOMM (tokinfo lexbuf) }

  | '(' { LPAREN (tokinfo lexbuf) }   | ')' { RPAREN (tokinfo lexbuf) }
  | '[' { LBRACKET (tokinfo lexbuf) } | ']' { RBRACKET (tokinfo lexbuf) }
  | '{' { LBRACE (tokinfo lexbuf) }   | '}' { RBRACE (tokinfo lexbuf) }

  | ':'     { LCOLON (tokinfo lexbuf) }
  | ';'     { LSEMICOLON (tokinfo lexbuf) }
  | '.'     { LDOT (tokinfo lexbuf) }
  | ','     { LCOMMA (tokinfo lexbuf) }

  (* part of go and also sgrep-ext: *)
  | "..."   { LDDD (tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* Keywords and ident *)
  (* ----------------------------------------------------------------------- *)

  (* keywords *)
  | identifier as id
      { match id with

        | "if"       -> LIF (tokinfo lexbuf)
        | "else"     -> LELSE (tokinfo lexbuf)

        | "for"      -> LFOR (tokinfo lexbuf)

        | "switch"      -> LSWITCH (tokinfo lexbuf)
        | "case"      -> LCASE (tokinfo lexbuf)
        | "default"      -> LDEFAULT (tokinfo lexbuf)

        | "return"   -> LRETURN (tokinfo lexbuf)
        | "break"    -> LBREAK (tokinfo lexbuf)
        | "continue" -> LCONTINUE (tokinfo lexbuf)
        | "fallthrough"      -> LFALL (tokinfo lexbuf)
        | "goto"      -> LGOTO (tokinfo lexbuf)

        | "func"      -> LFUNC (tokinfo lexbuf)
        | "const"    -> LCONST (tokinfo lexbuf)
        | "var"    -> LVAR (tokinfo lexbuf)
        | "type"    -> LTYPE (tokinfo lexbuf)
        | "struct"    -> LSTRUCT (tokinfo lexbuf)
        | "interface"   -> LINTERFACE (tokinfo lexbuf)


        | "package"     -> LPACKAGE (tokinfo lexbuf)
        | "import"   -> LIMPORT (tokinfo lexbuf)

        | "go"       -> LGO (tokinfo lexbuf)
        | "chan"       -> LCHAN (tokinfo lexbuf)
        | "select"       -> LSELECT (tokinfo lexbuf)
        | "defer"   -> LDEFER (tokinfo lexbuf)
        | "map"      -> LMAP (tokinfo lexbuf)
        | "range"   -> LRANGE (tokinfo lexbuf)
      
        | _          -> LNAME (id, (tokinfo lexbuf)) 
    }

  (* sgrep-ext: *)
  | '$' identifier 
    { let s = tok lexbuf in
      if not !Flag_parsing.sgrep_mode
      then error ("identifier with dollar: "  ^ s) lexbuf;
      LNAME (s, tokinfo lexbuf)
    }

  (* ----------------------------------------------------------------------- *)
  (* Constant *)
  (* ----------------------------------------------------------------------- *)

  (* literals *)
  | integer as n
      { LINT (n, tokinfo lexbuf) }

  | floatnumber as n
      { LFLOAT (n, tokinfo lexbuf) }

  | imagnumber as n
      { LIMAG (n, tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* Chars/Strings *)
  (* ----------------------------------------------------------------------- *)
  | '\''
      { rune (tokinfo lexbuf) lexbuf }
  | '"'
      { string (tokinfo lexbuf) lexbuf }

  (* ----------------------------------------------------------------------- *)
  (* eof *)
  (* ----------------------------------------------------------------------- *)
  | eof { EOF (tokinfo lexbuf) }

  | _ { error (spf "unrecognized symbol: %s" (tok lexbuf)) lexbuf;
        TUnknown (tokinfo lexbuf)
      }

(*****************************************************************************)
(* Rules on strings *)
(*****************************************************************************)

and rune pos = parse
  | (([^ '\\' '\r' '\n' '\''] | escapeseq) as s) '\'' 
     { 
       let full_str = Lexing.lexeme lexbuf in
       LRUNE (unescaped s, PI.tok_add_s full_str pos) }
 | eof { error "EOF in rune" lexbuf; EOF (tokinfo lexbuf) }
 | _  { error "unrecognized symbol in rune" lexbuf; TUnknown(tokinfo lexbuf)}

and string pos = parse
  | (([^ '\\' '\r' '\n' '\"'] | escapeseq)* as s) '"' 
     { 
       let full_str = Lexing.lexeme lexbuf in
       LSTR (unescaped s, PI.tok_add_s full_str pos) }
 | eof { error "EOF in string" lexbuf; EOF (tokinfo lexbuf) }
 | _  { error "unrecognized symbol in string" lexbuf; TUnknown(tokinfo lexbuf)}


(*****************************************************************************)
(* Rule comment *)
(*****************************************************************************)

and comment buf = parse
  | "*/"    { Buffer.add_string buf (tok lexbuf) }
  (* noteopti: *)
  | [^'*']+ { Buffer.add_string buf (tok lexbuf); comment buf lexbuf }
  | "*"     { Buffer.add_string buf (tok lexbuf); comment buf lexbuf }
  | eof     { error "end of file in comment" lexbuf }
  | _  {
      let s = tok lexbuf in
      error ("unrecognised symbol in comment:"^s) lexbuf;
      Buffer.add_string buf s;
      comment buf lexbuf
    }
