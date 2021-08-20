(*
   Regexp AST
*)

open Printf

type loc = Parse_info.t * Parse_info.t

type special =
  | Beginning_of_line
  | End_of_line
  | Beginning_of_input
  | End_of_input

type abstract_char_class =
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
  | Other of int (* some unrecognized character following '(?' *)

type t =
  | Empty of loc
  | Char of loc * char_class
  | Special of loc * special
  | Seq of loc * t * t
  | Alt of loc * t * t
  | Repeat of loc * t * repeat_range * matching_pref
  | Group of loc * group_kind * t

let location = function
  | Empty loc
  | Char (loc, _)
  | Special (loc, _)
  | Seq (loc, _, _)
  | Alt (loc, _, _)
  | Repeat (loc, _, _, _)
  | Group (loc, _, _) -> loc

let union (a : char_class) (b : char_class) =
  match a, b with
  | a, Empty -> a
  | Empty, b -> b
  | a, b -> Union (a, b)

let seq loc (a : t) (b : t) =
  match a, b with
  | a, Empty _ -> a
  | Empty _, b -> b
  | a, b -> Seq (loc, a, b)

let code_points_of_ascii_string s : int list =
  let codes = ref [] in
  for i = String.length s - 1 downto 0 do
    codes := Char.code s.[i] :: !codes
  done;
  !codes

let location2 a b =
  let start, _ = location a in
  let _, end_ = location b in
  (start, end_)

let range (a, _) (_, b) =
  (a, b)

let dummy_loc =
  let tok = Parse_info.fake_info "" in
  (tok, tok)

type pp =
  | Line of string
  | Block of pp list
  | Inline of pp list

let to_buf buf l =
  let open Printf in
  let rec pp indent = function
    | Line s ->
        bprintf buf "%s%s\n" indent s
    | Block l ->
        List.iter (pp (indent ^ "  ")) l
    | Inline l ->
        List.iter (pp indent) l
  in
  pp "" (Inline l)

let pp_special (x : special) =
  match x with
  | Beginning_of_line -> "Beginning_of_line"
  | End_of_line -> "End_of_line"
  | Beginning_of_input -> "Beginning_of_input"
  | End_of_input -> "End_of_input"

let show_char code =
  if code < 128 then
    sprintf "%C" (Char.chr code)
  else
    sprintf "0x%X" code

(*
   We print a character class on a single line, using a format designed
   to be easy to understand. For example, '\w' is printed as 'word_char'.
*)
let pp_char_class (x : char_class) =
  let rec pp buf (x : char_class) =
    match x with
    | Empty -> bprintf buf "{}"
    | Singleton code -> bprintf buf "%s" (show_char code)
    | Range (a, b) -> bprintf buf "[%s-%s]" (show_char a) (show_char b)
    | Union (a, b) -> bprintf buf "(%a|%a)" pp a pp b
    | Inter (a, b) -> bprintf buf "(%a&%a)" pp a pp b
    | Diff (a, b) -> bprintf buf "(%a-%a)" pp a pp b
    | Complement a -> bprintf buf "^%a" pp a
    | Abstract (Unicode_character_property name) ->
        bprintf buf "(Unicode_property %s)" name
    | Abstract Extended_grapheme_cluster ->
        bprintf buf "(Extended_grapheme_cluster)"
    | Other data ->
        bprintf buf "(Other %S)" data
  in
  let buf = Buffer.create 64 in
  pp buf x;
  Buffer.contents buf

let rec pp (node : t) =
  match node with
  | Empty _ -> [Line "Empty"]
  | Char (_, x) -> [Line ("Char: " ^ pp_char_class x)]
  | Special (_, x) -> [Line ("Special: " ^ pp_special x)]
  | Seq (_, a, b) -> [Block (pp a); Line "."; Block (pp b)]
  | Alt (_, a, b) -> [Block (pp a); Line "|"; Block (pp b)]
  | Repeat (_, a, _, _) -> [Line "Repeat:"; Block (pp a)]
  | Group (_, _, a) -> [Line "Group:"; Block (pp a)]

let print node =
  let buf = Buffer.create 1000 in
  to_buf buf (pp node);
  print_string (Buffer.contents buf)
