(* you should set this flag when you run code compiled by js_of_ocaml *)
val jsoo: bool ref

val (=|=) : int    -> int    -> bool
val (=<=) : char   -> char   -> bool
val (=$=) : string -> string -> bool
val (=:=) : bool   -> bool   -> bool

val (=*=): 'a -> 'a -> bool

(*
   Same as print_endline: print the string and a newline, then flush stdout.
*)
val pr : string -> unit

(*
   Print a string and a newline to stderr, then flush stderr.
*)
val pr2 : string -> unit

(* forbid pr2_once to do the once "optimisation" *)
val _already_printed : (string, bool) Hashtbl.t
val disable_pr2_once : bool ref
val pr2_once : string -> unit

val pr2_gen: 'a -> unit
val dump: 'a -> string

(* to be used in pipes as in foo() |> before_return (fun v -> pr2_gen v)*)
val before_return: ('a -> unit) -> 'a -> 'a

exception Todo
exception Impossible

exception Multi_found

val exn_to_s : exn -> string

val i_to_s : int -> string
val s_to_i : string -> int

val null_string : string -> bool

val (=~) : string -> string -> bool
val matched1 : string -> string
val matched2 : string -> string * string
val matched3 : string -> string * string * string
val matched4 : string -> string * string * string * string
val matched5 : string -> string * string * string * string * string
val matched6 : string -> string * string * string * string * string * string
val matched7 : string -> string * string * string * string * string * string * string

val spf : ('a, unit, string) format -> 'a

val join : string (* sep *) -> string list -> string
val split : string (* sep regexp *) -> string -> string list

type filename = string
val pp_filename: Format.formatter -> filename -> unit
val equal_filename: filename -> filename -> bool
type dirname = string
type path = string

(*
   Return the lines of a file. Both Windows-style and Unix-style line endings
   are recognized and removed from the end of the line.
*)
val cat :      filename -> string list

val write_file : file:filename -> string -> unit

(* Read the contents of file.

   This implementation works even with Linux files like /dev/fd/63
   created by bash when using "process substitution"* e.g.

     my-ocaml-program <(echo contents)

   * https://www.gnu.org/software/bash/manual/html_node/Process-Substitution.html
*)
val read_file : filename -> string

val with_open_outfile :
  filename -> ((string -> unit) * out_channel -> 'a) -> 'a
val with_open_infile :
  filename -> (in_channel -> 'a) -> 'a

exception CmdError of Unix.process_status * string
val command2 : string -> unit
val cmd_to_list :  ?verbose:bool -> string -> string list (* alias *)
val cmd_to_list_and_status:
  ?verbose:bool -> string -> string list * Unix.process_status

val null : 'a list -> bool

(** Same as [List.map] but stack-safe and slightly faster on short lists.
    Additionally, we guarantee that the mapping function is applied from
    from left to right like for [List.iter].
*)
val map : ('a -> 'b) -> 'a list -> 'b list

val exclude : ('a -> bool) -> 'a list -> 'a list
val sort : 'a list -> 'a list

val uniq_by: ('a -> 'a -> bool) -> 'a list -> 'a list

(** Same as [List.filter_map] but tail recursive. *)
val map_filter : ('a -> 'b option) -> 'a list -> 'b list
val find_opt: ('a -> bool) -> 'a list -> 'a option
val find_some : ('a -> 'b option) -> 'a list -> 'b
val find_some_opt : ('a -> 'b option) -> 'a list -> 'b option
val filter_some: 'a option list -> 'a list

val take : int -> 'a list -> 'a list
val take_safe : int -> 'a list -> 'a list
val drop : int -> 'a list -> 'a list
val span : ('a -> bool) -> 'a list -> 'a list * 'a list

val index_list   : 'a list -> ('a * int) list
val index_list_0 : 'a list -> ('a * int) list
val index_list_1 : 'a list -> ('a * int) list

type ('a, 'b) assoc = ('a * 'b) list

val sort_by_val_lowfirst: ('a,'b) assoc -> ('a * 'b) list
val sort_by_val_highfirst: ('a,'b) assoc -> ('a * 'b) list

val sort_by_key_lowfirst: ('a,'b) assoc -> ('a * 'b) list
val sort_by_key_highfirst: ('a,'b) assoc -> ('a * 'b) list

val group_by: ('a -> 'b) -> 'a list -> ('b * 'a list) list
val group_assoc_bykey_eff : ('a * 'b) list -> ('a * 'b list) list
val group_by_mapped_key: ('a -> 'b) -> 'a list -> ('b * 'a list) list
val group_by_multi: ('a -> 'b list) -> 'a list -> ('b * 'a list) list

(** Same as [List.flatten] but tail recursive. *)
val flatten : 'a list list -> 'a list

type 'a stack = 'a list
val push : 'a -> 'a stack ref -> unit

val hash_of_list : ('a * 'b) list -> ('a, 'b) Hashtbl.t
val hash_to_list : ('a, 'b) Hashtbl.t -> ('a * 'b) list

type 'a hashset = ('a, bool) Hashtbl.t
val hashset_of_list : 'a list -> 'a hashset
val hashset_to_list : 'a hashset -> 'a list

val optlist_to_list: 'a list option -> 'a list
(* you should prefer let ( let* ) = Option.bind though *)
val (>>=): 'a option -> ('a -> 'b option) -> 'b option
val (|||): 'a option -> 'a -> 'a
val (<|>): 'a option -> 'a option -> 'a option


type ('a, 'b) either = Left of 'a | Right of 'b
val pp_either: (Format.formatter -> 'a -> 'b) ->
  (Format.formatter -> 'c -> 'd) ->
  Format.formatter -> ('a, 'c) either -> unit
val equal_either:
  ('a -> 'a -> bool) ->
  ('b -> 'b -> bool) ->
  ('a, 'b) either -> ('a, 'b) either -> bool
type ('a, 'b, 'c) either3 = Left3 of 'a | Middle3 of 'b | Right3 of 'c
val pp_either3: (Format.formatter -> 'a -> 'b) ->
  (Format.formatter -> 'c -> 'd) ->
  (Format.formatter -> 'e -> 'f) ->
  Format.formatter -> ('a, 'c, 'e) either3 -> unit
val partition_either :
  ('a -> ('b, 'c) either) -> 'a list -> 'b list * 'c list
val partition_either3 :
  ('a -> ('b, 'c, 'd) either3) -> 'a list -> 'b list * 'c list * 'd list

(*
   Same as 'partition_either' but operates on the standard type 'result'
   (Ok or Error).
*)
val partition_result :
  ('a -> ('ok, 'error) result) -> 'a list -> 'ok list * 'error list

type arg_spec_full = Arg.key * Arg.spec * Arg.doc
type cmdline_options = arg_spec_full list

type options_with_title = string * string * arg_spec_full list
type cmdline_sections = options_with_title list

(* A wrapper around Arg modules that have more logical argument order,
 * and returns the remaining args.
*)
val parse_options :
  cmdline_options -> Arg.usage_msg -> string array -> string list
(* Another wrapper that does Arg.align automatically *)
(* Another wrapper that does Arg.align automatically *)
val usage : Arg.usage_msg -> cmdline_options -> unit

(* Work with the options_with_title type way to organize a long
 * list of command line switches.
*)
val short_usage :
  Arg.usage_msg -> short_opt:cmdline_options -> unit
val long_usage :
  Arg.usage_msg -> short_opt:cmdline_options -> long_opt:cmdline_sections ->
  unit

(* With the options_with_title way, we don't want the default -help and --help
 * so need adapter of Arg module, not just wrapper.
*)
val arg_align2 : cmdline_options -> cmdline_options
val arg_parse2 :
  cmdline_options -> Arg.usage_msg -> (unit -> unit) (* short_usage func *) ->
  string list

(* The action lib. Useful to debug supart of your system. cf some of
 * my Main.ml for example of use. *)
type flag_spec   = Arg.key * Arg.spec * Arg.doc
type action_spec = Arg.key * Arg.doc * action_func
and action_func = (string list -> unit)

type cmdline_actions = action_spec list
exception WrongNumberOfArguments

val mk_action_0_arg : (unit -> unit)                       -> action_func
val mk_action_1_arg : (string -> unit)                     -> action_func
val mk_action_2_arg : (string -> string -> unit)           -> action_func
val mk_action_3_arg : (string -> string -> string -> unit) -> action_func
val mk_action_4_arg : (string -> string -> string -> string -> unit) ->
  action_func

val mk_action_n_arg : (string list -> unit) -> action_func

val options_of_actions:
  string ref (* the action ref *) -> cmdline_actions -> cmdline_options
val do_action:
  Arg.key -> string list (* args *) -> cmdline_actions -> unit
val action_list:
  cmdline_actions -> Arg.key list


(* if set then certain functions like unwind_protect will not
 * do a try and finalize and instead just call the function, which
 * helps in ocamldebug and also in getting better backtraces.
 * This is also useful to set in a js_of_ocaml (jsoo) context to
 * again get better backtraces.
*)
val debugger : bool ref

(* emacs spirit *)
val unwind_protect : (unit -> 'a) -> (Exception.t -> 'b) -> 'a
(* java spirit *)
val finalize :       (unit -> 'a) -> (unit -> unit) -> 'a

val save_excursion : 'a ref -> 'a -> (unit -> 'b) -> 'b

val memoized :
  ?use_cache:bool -> ('a, 'b) Hashtbl.t -> 'a -> (unit -> 'b) -> 'b

exception ExceededMemoryLimit of string

exception UnixExit of int

(* Contains the name given by the user to the timer and the time limit *)
type timeout_info

(*
   If ever caught, this exception must be re-raised immediately so as
   to not interfere with the timeout handler. See function 'set_timeout'.
*)
exception Timeout of timeout_info

(* Show name and time limit in a compact format for debugging purposes. *)
val string_of_timeout_info : timeout_info -> string

(*
   Launch the specified computation and abort if it takes longer than
   specified (in seconds).

   This uses a global timer. An Invalid_argument exception will be raised
   if the timer is already running.

   tl;dr nesting will fail
*)
val set_timeout:
  name:string -> float -> (unit -> 'a) -> 'a option

(*
   Only set a timer if a time limit is specified. Uses 'set_timeout'.
*)
val set_timeout_opt:
  name:string -> float option -> (unit -> 'a) -> 'a option

(*
   Measure how long it takes for a function to run, returning the result
   and the duration.
*)
val with_time: (unit -> 'a) -> 'a * float

(*
   Run a function and print how long it took to return or to raise an
   exception. pr_time prints to stdout, pr2_time prints to stderr.
*)
val pr_time: string -> (unit -> 'a) -> 'a
val pr2_time: string -> (unit -> 'a) -> 'a

type prof = ProfAll | ProfNone | ProfSome of string list
val profile : prof ref
val show_trace_profile : bool ref

val _profile_table : (string, (float ref * int ref)) Hashtbl.t ref
val profile_code : string -> (unit -> 'a) -> 'a
val profile_diagnostic : unit -> string
val profile_code_exclusif : string -> (unit -> 'a) -> 'a
val profile_code_inside_exclusif_ok : string -> (unit -> 'a) -> 'a
val report_if_take_time : int -> string -> (unit -> 'a) -> 'a
(* similar to profile_code but print some information during execution too *)
(* similar to profile_code but print some information during execution too *)
val profile_code2 : string -> (unit -> 'a) -> 'a

(* creation of /tmp files, a la gcc
 * ex: new_temp_file "cocci" ".c" will give "/tmp/cocci-3252-434465.c"
*)
val _temp_files_created : (string, unit) Hashtbl.t
val save_tmp_files : bool ref
val new_temp_file : string (* prefix *) -> string (* suffix *) -> filename
val erase_temp_files : unit -> unit
val erase_this_temp_file : filename -> unit

(* for realpath, see efuns_c library  *)
(*
   Check that the file exists and produce a valid absolute path for the file.
*)
val fullpath: filename -> filename

val cache_computation :
  ?use_cache:bool -> filename  -> string (* extension *) ->
  (unit -> 'a) -> 'a

val filename_without_leading_path : string -> filename -> filename
val readable: root:string -> filename -> filename
val is_directory : filename -> bool 

val follow_symlinks: bool ref
val files_of_dir_or_files_no_vcs_nofilter:
  string list -> filename list

(* do some finalize, signal handling, unix exit conversion, etc *)
val main_boilerplate : (unit -> unit) -> unit

(* type of maps from string to `a *)
module SMap : Map.S with type key = String.t
type 'a smap = 'a SMap.t
