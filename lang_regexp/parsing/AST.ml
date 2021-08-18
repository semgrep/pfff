(*
   Regexp AST
*)

type loc = Parse_info.t * Parse_info.t

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
  | Other of int (* some unrecognized character following '(?' *)

type t =
  | Empty of loc
  | Char of loc * char_class
  | Shorthand of loc * char
  | Seq of loc * t * t
  | Alt of loc * t * t
  | Repeat of loc * t * repeat_range * matching_pref
  | Group of loc * group_kind * t

let location = function
  | Empty loc
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

let rec pp (node : t) =
  match node with
  | Empty _ -> [Line "Empty"]
  | Char (_, _) -> [Line "Char"]
  | Shorthand (_, _) -> [Line "Shorthand"]
  | Seq (_, a, b) -> [Block (pp a); Line "."; Block (pp b)]
  | Alt (_, a, b) -> [Block (pp a); Line "|"; Block (pp b)]
  | Repeat (_, a, _, _) -> [Line "Repeat:"; Block (pp a)]
  | Group (_, _, a) -> [Line "Group:"; Block (pp a)]

let print node =
  let buf = Buffer.create 1000 in
  to_buf buf (pp node);
  print_string (Buffer.contents buf)
