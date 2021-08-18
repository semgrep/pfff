(*
   Regexp AST
*)

type loc = Parse_info.t * Parse_info.t

type abstract_char_class =
  | Word_character
  (* \w a "word character", whose definition is locale-dependent. *)
  | Unicode_character_property of string
  (* Unicode character property introduced with \p{...}
     e.g. \p{Meroitic_Hieroglyphs} *)
  | Extended_grapheme_cluster (* \X *)

(*
   Character ranges or single characters as they occur within
   character classes e.g. '[a-z_]'.
   Characters are represented by their Unicode identifier.

   Pure ascii classes should not use 'Name' but instead should be expanded
   into the exact set of code points. Mixed ascii/unicode characters classes
   may be represented using 'Name' or using the other constructs.
*)
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
  | Other of int (* some unrecognized character following '(?' *)

type t =
  | Empty of loc
  | Char of loc * char_class
  (* match a single character from a set *)

  | Shorthand of loc * char
  (* all the backslash sequences like '\A' that don't consume
     a character. Some of them are assertions, others may modify the
     behavior of the matching engine. *)

  | Seq of loc * t * t
  | Alt of loc * t * t
  | Repeat of loc * t * repeat_range * matching_pref
  | Group of loc * group_kind * t

val location : t -> loc
val location2 : t -> t -> loc
val range : loc -> loc -> loc
val dummy_loc : loc

(* Show the general structure of the AST. *)
val print : t -> unit
