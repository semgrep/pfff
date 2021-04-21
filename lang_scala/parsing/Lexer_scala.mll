{
(* Yoann Padioleau
 *
 * Copyright (C) 2021 r2c
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

open Parser_scala
module PI = Parse_info
module Flag = Flag_parsing

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The Scala lexer.
 *
 * reference:
 *    - https://scala-lang.org/files/archive/spec/2.13/13-syntax-summary.html
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* shortcuts *)
let tok = Lexing.lexeme
let tokinfo = Parse_info.tokinfo
let error = Parse_info.lexical_error

(* ---------------------------------------------------------------------- *)
(* Lexer State *)
(* ---------------------------------------------------------------------- *)
(* note: mostly a copy paste of the trick used in the lexer for PHP *)

type state_mode =
  (* Regular Scala mode *)
  | ST_IN_CODE

  (* started with x", finished with " *)
  | ST_IN_INTERPOLATED_DOUBLE
  (* started with x""", finished with """ *)
  | ST_IN_INTERPOLATED_TRIPLE

let default_state = ST_IN_CODE

let _mode_stack =
  ref [default_state]

let reset () =
  _mode_stack := [default_state];
  ()

let rec current_mode () =
  try
    Common2.top !_mode_stack
  with Failure("hd") ->
    pr2("mode_stack is empty, defaulting to INITIAL");
    reset();
    current_mode ()

let push_mode mode = Common.push mode _mode_stack
let pop_mode () = ignore(Common2.pop2 _mode_stack)
let set_mode mode = begin pop_mode(); push_mode mode; end

}

(*****************************************************************************)
(* Regexp aliases *)
(*****************************************************************************)

let whiteSpace = [' ' '\t'] (* todo: also OA, OD? *)

let upper = ['A'-'Z''$'] (* todo: and unicode category *)
let lower = ['a'-'z''_'] (* todo: and unicode category L1 *)
let letter = upper | lower (* todo: and unicode category Lo, Lt, Nl *)

let digit = ['0'-'9']
let hexDigit = digit | ['a'-'f''A'-'F']

(* no paren, and no delim, no quotes, no $ or _, no / for /* ambiguity *)
let opchar1 = ['+''-''*''%' ':''=''!''#''~''?''\\''@' '|''&''^' '<''>' ]
let opchar2 = opchar1 | '/'
let op = '/' | opchar1 opchar2*

let idrest = (letter | digit)* ('_' | op)?

let varid = lower idrest
let boundvarid = varid | '`' varid '`'
let plainid = upper idrest | varid | op

let charNoBackQuoteOrNewline = [^'\n''`']
let charNoQuoteOrNewline = [^'\n''\'']
let charNoDoubleQuoteOrNewline = [^'\n''"']
let charNoDoubleQuote = [^'"']

let newline = ('\n' | "\r\n")

let _charEscapeSeq = '\\' ['b''t''n''f''r''"''\'''\\']
(* semgrep: we can use regexp in semgrep in strings and we want to
 * support any escape characters there, e.g. eval("=~/.*dev\.corp/")
 *)
let semgrepEscapeSeq = '\\' _

let unicodeEscape = '\\' 'u'+ hexDigit hexDigit hexDigit hexDigit
let escapeSeq = unicodeEscape | semgrepEscapeSeq

let stringElement = charNoDoubleQuoteOrNewline | escapeSeq
let multiLineChars = ('"'? '"'? charNoDoubleQuote)* '"'*

let decimalNumeral = digit digit*
let hexNumeral = '0' ['X''x'] hexDigit hexDigit*

let integerLiteral = (decimalNumeral | hexNumeral) ['L''l']?

(* note: old Scale spec was wrong and was using stringLiteral *)
let id = plainid | '`' (charNoBackQuoteOrNewline | escapeSeq)+ '`'

(* for interpolated strings *)
let alphaid = upper idrest | varid

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
      Comment(info |> PI.rewrap_str (Buffer.contents buf))
    }

  (* don't keep the trailing \n; it will be in another token *)
  | "//" [^ '\r' '\n']* { Comment (tokinfo lexbuf) }
  | whiteSpace+ { Space (tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* Newline (treated specially in Scala, like in Python) *)
  (* ----------------------------------------------------------------------- *)

  | newline     { Nl (tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* symbols *)
  (* ----------------------------------------------------------------------- *)

  (* paren *)
  | '(' { LPAREN (tokinfo lexbuf) }   | ')' { RPAREN (tokinfo lexbuf) }
  | '[' { LBRACKET (tokinfo lexbuf) } | ']' { RBRACKET (tokinfo lexbuf) }
  | '{' { push_mode ST_IN_CODE; LBRACE (tokinfo lexbuf) }
  | '}' { pop_mode ();          RBRACE (tokinfo lexbuf) }

  (* delim *)
  | ';'     { SEMI (tokinfo lexbuf) }
  | ','     { COMMA (tokinfo lexbuf) }
  | '.'     { DOT (tokinfo lexbuf) }

  | ':'     { COLON (tokinfo lexbuf) }
  | '='     { EQ (tokinfo lexbuf) }

  (* conflict with op? *)
  | '+'     { PLUS (tokinfo lexbuf) }
  | '-'     { MINUS (tokinfo lexbuf) }
  | '*'     { STAR (tokinfo lexbuf) }

  | '!'     { BANG (tokinfo lexbuf) }
  | '#'     { SHARP (tokinfo lexbuf) }
  | '~'     { TILDE (tokinfo lexbuf) }
  | '|'     { PIPE (tokinfo lexbuf) }
  | '_'     { UNDERSCORE (tokinfo lexbuf) }

  | "<%"    { LESSPERCENT (tokinfo lexbuf) }
  | "<-"    { LESSMINUS (tokinfo lexbuf) }
  | "<:"    { LESSCOLON (tokinfo lexbuf) }
  | "=>"    { EQMORE (tokinfo lexbuf) }
  | ">:"    { MORECOLON (tokinfo lexbuf) }

  | "@"    { AT (tokinfo lexbuf) }

  (* semgrep-ext: *)
  | "..."   { Ellipsis (tokinfo lexbuf) }
  (* semgrep-ext: *)
  | "<..."  { Flag_parsing.sgrep_guard (LDots (tokinfo lexbuf)) }
  | "...>"  { Flag_parsing.sgrep_guard (RDots (tokinfo lexbuf)) }

  (* ----------------------------------------------------------------------- *)
  (* Keywords and ident *)
  (* ----------------------------------------------------------------------- *)

  (* keywords *)
  | id as s
      { let t = tokinfo lexbuf in
        match s with
        | "null" -> Knull t

        | "true" -> BooleanLiteral (true, t)
        | "false" -> BooleanLiteral (false, t)

        | "if"       -> Kif t
        | "else"     -> Kelse t

        | "for"      -> Kfor t
        | "forSome"      -> KforSome t
        | "while"      -> Kwhile t
        | "do"      -> Kdo t

        | "match"      -> Kmatch t
        | "case"      -> Kcase t

        | "try"      -> Ktry t
        | "catch"      -> Kcatch t
        | "finally"      -> Kfinally t
        | "throw"      -> Kthrow t

        | "return"   -> Kreturn t

        | "class"      -> Kclass t
        | "trait"      -> Ktrait t
        | "object"      -> Kobject t
        | "new"      -> Knew t
        | "super" -> Ksuper t
        | "this" -> Kthis t
        | "with" -> Kwith t

        | "def"    -> Kdef t
        | "type"    -> Ktype t
        | "var"    -> Kvar t
        | "val"    -> Kval t

        | "package"     -> Kpackage t
        | "import"   -> Kimport t

        | "abstract"       -> Kabstract t
        | "final"       -> Kfinal t
        | "private"       -> Kprivate t
        | "protected"       -> Kprotected t

        | "sealed"       -> Ksealed t
        | "override"       -> Koverride t
        | "implicit"    -> Kimplicit t
        | "lazy"    -> Klazy t
        | "yield"    -> Kyield t

        | _          -> Id (s, t)
    }

  (* semgrep-ext: *)
(*
  | '$' id
    { let s = tok lexbuf in
      if not !Flag_parsing.sgrep_mode
      then error ("identifier with dollar: "  ^ s) lexbuf;
      Id (tokinfo lexbuf)
    }
*)

  (* ----------------------------------------------------------------------- *)
  (* Constant *)
  (* ----------------------------------------------------------------------- *)
  (* literals *)
  | integerLiteral as n
      { IntegerLiteral (int_of_string_opt n, tokinfo lexbuf) }

(*
  | float_lit as n
      { FloatingPointLiteral (float_of_string_opt n, tokinfo lexbuf) }
*)

  (* ----------------------------------------------------------------------- *)
  (* Chars/Strings *)
  (* ----------------------------------------------------------------------- *)

  | "'" ((charNoBackQuoteOrNewline | escapeSeq) as s)  "'"
      { CharacterLiteral (s, tokinfo lexbuf) }
  | '"' (stringElement* as s) '"'
      { StringLiteral (s, tokinfo lexbuf) }
  | "\"\"\"" (multiLineChars as s) "\"\"\""
      { StringLiteral (s, tokinfo lexbuf) }

  (* interpolated strings *)
  | alphaid as s '"'
   { push_mode ST_IN_INTERPOLATED_DOUBLE;
     T_INTERPOLATED_START(s, tokinfo lexbuf) }
  | alphaid as s "\"\"\""
   { push_mode ST_IN_INTERPOLATED_TRIPLE;
     T_INTERPOLATED_START(s, tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* eof *)
  (* ----------------------------------------------------------------------- *)
  | eof { EOF (tokinfo lexbuf) }

  | _ { error (spf "unrecognized symbol: %s" (tok lexbuf)) lexbuf;
        Unknown (tokinfo lexbuf)
      }

(*****************************************************************************)
(* String interpolation *)
(*****************************************************************************)
and in_interpolated_double = parse
  | '"'  { pop_mode(); T_INTERPOLATED_END (tokinfo lexbuf) }

  (* not in original spec *)
  | escapeSeq as s { T_INTERPOLATED_STRING (s, tokinfo lexbuf) }
  | [^'"''$''\\']+ as s { T_INTERPOLATED_STRING (s, tokinfo lexbuf) }
  | "${" { push_mode ST_IN_CODE; T_DOLLAR_LBRACE (tokinfo lexbuf) }
  | "$" id as s { Id (s, tokinfo lexbuf) }

  | eof  { error "end of file in interpolated string" lexbuf;
           pop_mode();
           Unknown(tokinfo lexbuf) }
  | _  {
       error ("unrecognised symbol in interpolated string:"^tok lexbuf) lexbuf;
       Unknown(tokinfo lexbuf)
       }

and in_interpolated_triple = parse
  | "\"\"\""  { pop_mode(); T_INTERPOLATED_END (tokinfo lexbuf) }
  | "\"" { T_INTERPOLATED_STRING (tok lexbuf, tokinfo lexbuf) }
  (* not in original spec *)
  | escapeSeq as s { T_INTERPOLATED_STRING (s, tokinfo lexbuf) }
  | [^'"''$''\\']+ as s { T_INTERPOLATED_STRING (s, tokinfo lexbuf) }
  | "${" { push_mode ST_IN_CODE; T_DOLLAR_LBRACE (tokinfo lexbuf) }
  | "$" id as s { Id (s, tokinfo lexbuf) }

  | eof  { error "end of file in interpolated2 string" lexbuf;
           pop_mode();
           Unknown(tokinfo lexbuf) }
  | _  {
       error("unrecognised symbol in interpolated2 string:"^tok lexbuf) lexbuf;
       Unknown(tokinfo lexbuf)
       }

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
