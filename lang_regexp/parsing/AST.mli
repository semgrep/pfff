(*
   Regexp AST
*)

type loc = Parse_info.t * Parse_info.t

(*
   Any atomic sequence that doesn't consume a character of input,
   such as '^', '\A', '\w', etc.
*)
type special =
  | Beginning_of_line
  | End_of_line
  | Beginning_of_input
  | End_of_input
  | Back_reference of int

type abstract_char_class =
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

  | Special of loc * special
  (* all the backslash sequences like '\A' that don't consume
     a character. Some of them are assertions, others may modify the
     behavior of the matching engine. *)

  | Seq of loc * t * t
  | Alt of loc * t * t
  | Repeat of loc * t * repeat_range * matching_pref
  | Group of loc * group_kind * t

(***************************************************************************)
(* Constructors, meant for the parser or for testing. *)
(***************************************************************************)

(* Eliminate one of the terms if it's'Empty' since we get that a lot due
   to how parsing is done *)
val union : char_class -> char_class -> char_class

(* Eliminate one of the terms if it's'Empty' since we get that a lot due
   to how parsing is done *)
val seq : loc -> t -> t -> t

val code_points_of_ascii_string : string -> int list

val location : t -> loc
val location2 : t -> t -> loc
val range : loc -> loc -> loc
val dummy_loc : loc

(***************************************************************************)
(* AST dump *)
(***************************************************************************)

(* Show the general structure of the AST. *)
val print : t -> unit
