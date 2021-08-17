(*
   Predefined character classes.

   Uses the definitions of PCRE unless otherwise noted.
   https://www.pcre.org/original/doc/html/pcrepattern.html

   Many different Unicode classes are defined and supported by PCRE via
   \p{Foo} and \P{Foo} (complement). These are recognized by our parser
   as being character classes but we don't expand them.
*)

open AST

(*
  \d     any decimal digit
  \D     any character that is not a decimal digit
  \h     any horizontal white space character
  \H     any character that is not a horizontal white space character
  \s     any white space character
  \S     any character that is not a white space character
  \v     any vertical white space character
  \V     any character that is not a vertical white space character
  \w     any "word" character
  \W     any "non-word" character

  There is also the single sequence \N, which matches a non-newline character.
*)

let rec list chars =
  match chars with
  | [] -> Empty
  | [last] -> Singleton last
  | x :: chars -> Union (Singleton x, list chars)

let decimal_digit = Range (Char.code '0', Char.code '9')

let horizontal_whitespace = list [
  0x0009; (* Horizontal tab (HT) *)
  0x0020; (* Space *)
  0x00A0; (* Non-break space *)
  0x1680; (* Ogham space mark *)
  0x180E; (* Mongolian vowel separator *)
  0x2000; (* En quad *)
  0x2001; (* Em quad *)
  0x2002; (* En space *)
  0x2003; (* Em space *)
  0x2004; (* Three-per-em space *)
  0x2005; (* Four-per-em space *)
  0x2006; (* Six-per-em space *)
  0x2007; (* Figure space *)
  0x2008; (* Punctuation space *)
  0x2009; (* Thin space *)
  0x200A; (* Hair space *)
  0x202F; (* Narrow no-break space *)
  0x205F; (* Medium mathematical space *)
  0x3000; (* Ideographic space *)
]

let vertical_whitespace = list [
  0x000A; (* Linefeed (LF) *)
  0x000B; (* Vertical tab (VT) *)
  0x000C; (* Form feed (FF) *)
  0x000D; (* Carriage return (CR) *)
  0x0085; (* Next line (NEL) *)
  0x2028; (* Line separator *)
  0x2029; (* Paragraph separator *)
]

let whitespace = Union (horizontal_whitespace, vertical_whitespace)
