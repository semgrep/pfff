type match_format =
  (* ex: tests/misc/foo4.php:3
   *  foo(
   *   1,
   *   2);
   *)
  | Normal
  (* ex: tests/misc/foo4.php:3: foo( *)
  | Emacs
  (* ex: tests/misc/foo4.php:3: foo(1,2) *)
  | OneLine
  (* ex: { check_id: ...; path: ...; start: ... end: ...; extra: ... *)
  | Json

val print_header: match_format -> unit
val print_match: ?format:match_format -> Parse_info.info list -> unit
val print_trailer: match_format -> unit

val join_with_space_if_needed: string list -> string
