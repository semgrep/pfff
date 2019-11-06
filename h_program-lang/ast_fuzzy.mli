
type tok = Parse_info.t
type 'a wrap = 'a * tok

type tree =
  | Parens of tok * (trees, tok (* comma*)) Common.either list * tok
  | Braces of tok * trees * tok
  | Angle  of tok * trees * tok
  | Bracket  of tok * trees * tok

  (* note that gcc allows $ in identifiers, so using $ for metavariables
   * means we will not be able to match such identifiers (but no big deal)
   *)
  | Metavar of string wrap
  (* note that "..." are allowed in many languages, so using "..."
   * to represent a list of anything means we will not be able to
   * match specifically "...".
   *)
  | Dots of tok

  | Tok of string wrap

and trees = tree list

(* see matcher/parse_fuzzy.mli for helpers to build such trees *)

val is_metavar: string -> bool
