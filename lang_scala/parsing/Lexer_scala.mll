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

}

(*****************************************************************************)
(* Regexp aliases *)
(*****************************************************************************)

let newline = ('\n' | "\r\n")
let whitespace = [' ' '\t']

(* todo: *)
let unicode_digit = ['0'-'9']
let unicode_letter = ['a'-'z' 'A'-'Z']
let unicode_char = [^ '\n' '\r']
let unicode_char_no_quote = [^ '\n' '\r' '\'' '\\']
let unicode_char_no_double_quote = [^ '\n' '\r' '"' '\\']
let unicode_char_no_backquote = [^ '\n' '\r' '`' ]

let letter = unicode_letter | '_'

let identifier = letter (letter | unicode_digit)*


let decimal_digit = ['0'-'9']
let binary_digit = ['0'-'1']
let octal_digit = ['0'-'7']
let hex_digit = ['0'-'9' 'a'-'f' 'A'-'F']

let decimal_digits = decimal_digit ('_'? decimal_digit)*
let binary_digits = binary_digit ('_'? binary_digit)*
let octal_digits = octal_digit ('_'? octal_digit)*
let hex_digits = hex_digit ('_'? hex_digit)*

let decimal_lit = "0" | ['1'-'9'] ( '_'? decimal_digits)?
let binary_lit = "0" ['b' 'B'] '_'? binary_digits
let octal_lit = "0" ['o' 'O']? '_'? octal_digits
let hex_lit = "0" ['x' 'X'] '_'? hex_digits

let int_lit =
   decimal_lit
 | binary_lit
 | octal_lit
 | hex_lit

let decimal_exponent = ['e' 'E'] ['+' '-']? decimal_digits
let decimal_float_lit =
   decimal_digits '.' decimal_digits? decimal_exponent?
 | decimal_digits decimal_exponent
 | '.' decimal_digits decimal_exponent?

let hex_mantissa =
   '_'? hex_digits '.' hex_digits?
 | '_'? hex_digits
 | '.' hex_digits
let hex_exponent = ['p' 'P'] ['+' '-']? decimal_digits
let hex_float_lit = '0' ['x' 'X'] hex_mantissa hex_exponent

let float_lit = decimal_float_lit | hex_float_lit

let imaginary_lit = (decimal_digits | int_lit | float_lit) 'i'

let escaped_char = '\\' ['a' 'b' 'f' 'n' 'r' 't' 't' 'v' '\\' '\'' '"']

let little_u_value = '\\' 'u' hex_digit hex_digit hex_digit hex_digit
let big_u_value =    '\\' 'U' hex_digit hex_digit hex_digit hex_digit
                              hex_digit hex_digit hex_digit hex_digit

(* the Go ref says just unicode_char, but this can not work, hence the
 * use of various xxx_no_yyy below
 *)
let unicode_value_no_quote =
  unicode_char_no_quote
| little_u_value
| big_u_value
| escaped_char

let unicode_value_no_double_quote =
  unicode_char_no_double_quote
| little_u_value
| big_u_value
| escaped_char

let octal_byte_value = '\\' octal_digit octal_digit octal_digit
let hex_byte_value = '\\' 'x' hex_digit hex_digit
let byte_value = octal_byte_value | hex_byte_value

(* semgrep: we can use regexp in semgrep in strings and we want to
 * support any escape characters there, e.g. eval("=~/.*dev\.corp/")
 *)
let semgrep_escapeseq = '\\' _

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
  | newline     { Nl (tokinfo lexbuf) }
  | whitespace+ { Space (tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* symbols *)
  (* ----------------------------------------------------------------------- *)

  | '(' { LPAREN (tokinfo lexbuf) }   | ')' { RPAREN (tokinfo lexbuf) }
  | '[' { LBRACKET (tokinfo lexbuf) } | ']' { RBRACKET (tokinfo lexbuf) }
  | '{' { LBRACE (tokinfo lexbuf) }   | '}' { RBRACE (tokinfo lexbuf) }

  | ';'     { SEMI (tokinfo lexbuf) }
  | ','     { COMMA (tokinfo lexbuf) }
  | '.'     { DOT (tokinfo lexbuf) }
  | ':'     { COLON (tokinfo lexbuf) }
  | '='     { EQ (tokinfo lexbuf) }

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
  | identifier as id
      { let t = tokinfo lexbuf in
        match id with
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

        | _          -> Id t
    }

  (* semgrep-ext: *)
  | '$' identifier
    { let s = tok lexbuf in
      if not !Flag_parsing.sgrep_mode
      then error ("identifier with dollar: "  ^ s) lexbuf;
      Id (tokinfo lexbuf)
    }

  (* ----------------------------------------------------------------------- *)
  (* Constant *)
  (* ----------------------------------------------------------------------- *)

  (* literals *)
  (* this is also part of int_lit, but we specialize it here to use the
   * right int_of_string *)
  | "0" (octal_digits as n) { IntegerLiteral (int_of_string_opt( "0o" ^ n), tokinfo lexbuf) }

  | int_lit as n
      { IntegerLiteral (int_of_string_opt n, tokinfo lexbuf) }

  | float_lit as n
      { FloatingPointLiteral (float_of_string_opt n, tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* Chars/Strings *)
  (* ----------------------------------------------------------------------- *)
  | '\'' ((unicode_value_no_quote | byte_value) as s) '\''
      { CharacterLiteral (s, tokinfo lexbuf) }
  | '"' ((unicode_value_no_double_quote | byte_value)* as s) '"'
      { StringLiteral (s, tokinfo lexbuf) }
  | '"' ((unicode_value_no_double_quote | byte_value | semgrep_escapeseq)* as s) '"'
      { Flag.sgrep_guard (StringLiteral (s, tokinfo lexbuf)) }

  (* ----------------------------------------------------------------------- *)
  (* eof *)
  (* ----------------------------------------------------------------------- *)
  | eof { EOF (tokinfo lexbuf) }

  | _ { error (spf "unrecognized symbol: %s" (tok lexbuf)) lexbuf;
        Unknown (tokinfo lexbuf)
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
