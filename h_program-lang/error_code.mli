
type error = {
  typ: error_kind;
  loc: Parse_info.token_location;
  sev: severity;
}
 and severity = Error | Warning

 and error_kind =
  (* parsing related *)
  | LexicalError of string
  | ParseError (* aka SyntaxError *)
  | AstbuilderError of string
  | OtherParsingError of string

  (* global analysis *)
  | Deadcode of entity

  (* use/def entities *)
  | UndefinedDefOfDecl of entity
  | UnusedExport of entity * Common.filename
  | UnusedVariable of string * Scope_code.t

  (* CFG *)
 | UnreachableStatement of string
 | CFGError of string

  (* sgrep lint rules *)
  | SgrepLint of (string (* title/code *) * string (* msg *))

  (* other *)
  | FatalError of string

 and entity = (string * Entity_code.entity_kind)


(* @xxx to acknowledge or explain false positives *)
type annotation =
  | AtScheck of string

(* to detect false positives (we use the Hashtbl.find_all property) *)
type identifier_index = (string, Parse_info.token_location) Hashtbl.t


val string_of_error: error -> string
val string_of_error_kind: error_kind -> string


val g_errors: error list ref
(* !modify g_errors! *)
val error: Parse_info.t -> error_kind -> unit
val warning: Parse_info.t -> error_kind -> unit

val error_loc: Parse_info.token_location -> error_kind -> unit
val warning_loc: Parse_info.token_location -> error_kind -> unit

type rank =
 | Never
 | OnlyStrict
 | Less
 | Ok
 | Important
 | ReallyImportant

val score_of_rank: 
  rank -> int
val rank_of_error:
  error -> rank
val score_of_error:
  error -> int

val annotation_at:
  Parse_info.token_location -> annotation option

val options: unit -> Common.cmdline_options
val report_parse_errors: bool ref
val report_fatal_errors: bool ref
(* use the flags above to filter certain errors *)
val filter_maybe_parse_and_fatal_errors: error list -> error list
(* convert parsing and other fatal exceptions in regular 'error'
 * added to g_errors
 *)
val try_analyze_file_with_exn_to_errors:
  Common.filename -> (unit -> unit) -> unit

(* have some approximations and Fps in graph_code_checker so filter them *)
val adjust_errors:
  error list -> error list
