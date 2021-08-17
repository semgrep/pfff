(*
   Regexp AST
*)

type loc = Lexing.position * Lexing.position

type abstract_char_class =
  | Word_character
  | Unicode_character_property of string
  | Extended_grapheme_cluster (* \X *)

type char_class =
  | Empty
  | Singleton of int
  | Range of int * int
  | Union of char_class * char_class
  | Inter of char_class * char_class (* exotic *)
  | Diff of char_class * char_class (* exotic *)
  | Complement of char_class
  | Abstract of abstract_char_class
  | Other of string (* anything we can't make sense of *)

type repeat_range = int * int option

type matching_pref =
  | Longest_first (* default e.g. 'a*' *)
  | Shortest_first (* lazy e.g. 'a*?' *)
  | Possessive (* disable backtracking e.g. 'a*+' *)

type group_kind =
  | Non_capturing
  | Capturing
  | Lookahead
  | Neg_lookahead
  | Lookbehind
  | Neg_lookbehind
  | Comment
  | Other of int (* some unrecognized character following '(?' *)

type t =
  | Char of loc * char_class
  | Shorthand of loc * char
  | Seq of loc * t * t
  | Alt of loc * t * t
  | Repeat of loc * t * repeat_range * matching_pref
  | Group of loc * group_kind * t

let location = function
  | Char (loc, _)
  | Shorthand (loc, _)
  | Seq (loc, _, _)
  | Alt (loc, _, _)
  | Repeat (loc, _, _, _)
  | Group (loc, _, _) -> loc

let location2 a b =
  let start, _ = location a in
  let _, end_ = location b in
  (start, end_)
