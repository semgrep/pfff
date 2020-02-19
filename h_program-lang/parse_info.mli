(* ('token_location' < 'token_origin' < 'token_mutable') * token_kind *)

(* to report errors, regular position information *)
type token_location = {
    str: string; (* the content of the "token" *)
    charpos: int; (* byte position *)
    line: int; column: int;
    file: Common.filename;
} 
(* see also type filepos = { l: int; c: int; } in common.mli *)

(* to deal with expanded tokens, e.g. preprocessor like cpp for C *)
type token_origin =
  | OriginTok  of token_location
  | FakeTokStr of string  * (token_location * int) option (* next to *)
  | ExpandedTok of token_location * token_location * int 
  | Ab (* abstract token, see parse_info.ml comment *)

(* to allow source to source transformation via token "annotations", 
 * see the documentation for spatch.
 *)
type token_mutable = {
  token: token_origin; 
  (* for spatch *)
  mutable transfo: transformation;
}

 and transformation = 
  | NoTransfo
  | Remove 
  | AddBefore of add
  | AddAfter of add
  | Replace of add
  | AddArgsBefore of string list

  and add = 
    | AddStr of string
    | AddNewlineAndIdent

(* Shortcut.
 * Technically speaking this is not a token, because we do not have
 * the kind of the token (e.g., PLUS | IDENT | IF | ...).
 * It's just a lexeme, but the word lexeme is not as known as token.
 *)
type t = token_mutable
(* deprecated *)
type info_ = t

(* mostly for the fuzzy AST builder *)
type token_kind =
  | LPar | RPar
  | LBrace | RBrace
  | LBracket | RBracket
  | LAngle | RAngle
  | Esthet of esthet
  | Eof
  | Other
  and esthet =
   | Comment
   | Newline
   | Space

(* note that those exceptions can be converted in Error_code.error with
 * Error_code.try_with_exn_to_error()
 *)
(* see also Parsing.Parse_error and Failure "empty token" raised by Lexing *)
exception Lexical_error of string * t
(* better than Parsing.Parse_error, which does not have location information *)
exception Parsing_error of t
(* when convert from CST to AST *)
exception Ast_builder_error of string * t
(* other stuff *)
exception Other_error of string * t

exception NoTokenLocation of string

val lexical_error: string -> Lexing.lexbuf -> unit

val fake_token_location : token_location
val fake_info : string -> t
val first_loc_of_file: Common.filename -> token_location

val str_of_info   : t -> string
val line_of_info  : t -> int
val col_of_info   : t -> int
val pos_of_info   : t -> int
val file_of_info  : t -> Common.filename

(* small error reporting, for longer reports use error_message above *)
val string_of_info: t -> string

val is_origintok: t -> bool

val token_location_of_info: t -> token_location
val get_original_token_location: token_origin -> token_location

val compare_pos: t -> t -> int
val min_max_ii_by_pos: t list -> t * t


type parsing_stat = {
  filename: Common.filename;
  mutable correct: int;
  mutable bad: int;
  (* used only for cpp for now *)
  mutable have_timeout: bool;
  mutable commentized: int;
  mutable problematic_lines: (string list * int ) list;
}
val default_stat: Common.filename -> parsing_stat
val print_parsing_stat_list: ?verbose:bool -> parsing_stat list -> unit
val print_recurring_problematic_tokens: parsing_stat list -> unit


(* lexer helpers *)
type 'tok tokens_state = {
  mutable rest:         'tok list;
  mutable current:      'tok;
  (* it's passed since last "checkpoint", not passed from the beginning *)
  mutable passed:       'tok list;
}
val mk_tokens_state: 'tok list -> 'tok tokens_state

val tokinfo:
  Lexing.lexbuf -> t
val yyback: int -> Lexing.lexbuf -> unit

(* can deprecate? *)
val tokinfo_str_pos:  string -> int -> t

val rewrap_str: string -> t -> t
val tok_add_s: string -> t -> t

(* to be used by the lexer *)
val tokenize_all_and_adjust_pos: 
  Common.filename -> 
  (Lexing.lexbuf -> 'tok) (* tokenizer *) -> 
  ((t -> t) -> 'tok -> 'tok) (* token visitor *) -> 
  ('tok -> bool) (* is_eof *) -> 
  'tok list
val mk_lexer_for_yacc: 'tok list -> ('tok -> bool) (* is_comment *) ->
  'tok tokens_state * (* token stream for error recovery *)
   (Lexing.lexbuf -> 'tok) * (* the lexer to pass to the ocamlyacc parser *)
   Lexing.lexbuf (* fake lexbuf needed by ocamlyacc API *)

(* can deprecate? just use tokenize_all_and_adjust_pos *)
(* f(i) will contain the (line x col) of the i char position *)
val full_charpos_to_pos_large: 
  Common.filename -> (int -> (int * int))
(* fill in the line and column field of token_location that were not set
 * during lexing because of limitations of ocamllex. *)
val complete_token_location_large : 
  Common.filename -> (int -> (int * int))  -> token_location -> token_location


val error_message : Common.filename -> (string * int) -> string
val error_message_info :  t -> string
val print_bad: int -> int * int -> string array -> unit
