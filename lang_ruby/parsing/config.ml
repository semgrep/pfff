open Format

type config = {
  mutable cache : bool;
  mutable check_annotations : bool;
  mutable clean_cached : bool;
  mutable debug_constraints : bool;
  mutable debug_level : int;
  mutable error_raises_exc : bool;
  mutable stub_dir : string;
  mutable mine_dyn_feats : bool; (* should be hidden *)
  mutable mining_output : string option; (* should be hidden; output filename *)
  mutable no_dup_errors : bool;
  mutable print_error_ctx : bool;
  mutable print_filenames : bool;
  mutable profile: bool;
  mutable profile_db: string;
  mutable transform: bool;
  mutable resolve_scopes : bool;
  mutable ruby_args : string list;
  mutable ruby_file : string;
  mutable ruby_options : string list;
  mutable run_ruby : bool;
  mutable show_time : bool;
  mutable single_entry : bool;
  mutable use_annotations : bool;
  mutable use_base_types : bool;
  mutable used_conf_file : bool; (* not an option *)
  mutable warn_empty_body : bool;
  mutable emit_runtime_casts : bool;
  mutable idirectory : string list; (* shared *)
  mutable rlibrary : string list; (* shared *)
  mutable save_temps : bool;
  mutable type_inference : bool;
}

(* DRuby configuration; the default values are initially assigned *)
let conf = {
  cache = false;
  check_annotations = true;
  clean_cached = false;
  debug_constraints = false;
  debug_level = 0;
  error_raises_exc = false;
  stub_dir = "../stubs/2.3";
  mine_dyn_feats = false;
  mining_output = None;
  no_dup_errors = true;
  print_error_ctx = false;
  print_filenames = false;
  profile = false;
  profile_db = "druby_profile.db";
  transform = false;
  resolve_scopes = true;
  ruby_args = [];
  ruby_file = "";
  ruby_options = [];
  run_ruby = false;
  show_time = false;
  single_entry = true;
  use_annotations = true;
  use_base_types = true;
  used_conf_file = false; (* not an option *)
  warn_empty_body = false;
  emit_runtime_casts = false;
  idirectory = []; (* shared *)
  rlibrary = []; (* shared *)
  save_temps = false;
  type_inference = true;
}

(* constructs the usage string *)
let usage = 
  sprintf ("Usage: %s [options] file [-- arguments]")
  Sys.argv.(0)
 
let s_of_b = string_of_bool
let s_of_i = string_of_int

(* to provide better usage message, used string for short arguments as well *)
let druby_excl_options = [
  ("", "dr-cache", "cache control flow graph", Some(s_of_b conf.cache));
  ("", "dr-check-annotations", "check user annotations", 
    Some(s_of_b conf.check_annotations));
  ("", "dr-clean-cached", "clean up cache files", 
    Some(s_of_b conf.clean_cached));
  ("", "dr-config", "configuration file", None);
  ("", "dr-debug-constraints", "debug info for the constraint solver",
    Some(s_of_b conf.debug_constraints));
  ("", "dr-debug-level", "debug level", Some(s_of_i conf.debug_level));
  ("", "dr-error-raises-exc", "raise exception on error", 
    Some(s_of_b conf.error_raises_exc));
  ("", "dr-stub-dir", 
   "The location of DRuby's base_types.rb file and druby_* directories.  Will be added to $:", None);
  ("", "dr-no-dup-errors", 
    "do not print the same error twice (uses more memory)", 
    Some(s_of_b conf.no_dup_errors));
  ("", "dr-print-error-ctx", "print verbose context in error messages", 
    Some(s_of_b conf.print_error_ctx));
  ("", "dr-print-filenames", "print out filenames as they are parsed",
    Some(s_of_b conf.print_filenames));
  ("", "dr-profile", 
    "handles some dynamic features by running a test script",
    Some(s_of_b conf.profile));
  ("", "dr-profile-db", 
    "the name of the profile database used by --dr-profile",
    Some conf.profile_db);
  ("", "dr-transform", 
    "profile code using previously profiled data",
    Some(s_of_b conf.transform));
  ("", "dr-resolve-scopes", "statically resolve all constant scopes", 
    Some(s_of_b conf.resolve_scopes));
  ("", "dr-run-ruby", "execute the given file afterwards", 
    Some(s_of_b conf.run_ruby));
  ("", "dr-show-time", "show how much time it took to analyze", 
    Some(s_of_b conf.show_time));
  ("", "dr-single-entry", "attempt to detect and skip main like functions",
    Some(s_of_b conf.single_entry));
  ("", "dr-use-annotations", "use user annotations", 
    Some(s_of_b conf.use_annotations));
  ("", "dr-use-base-types", "load the base_types file before parsing files",
    Some(s_of_b conf.use_base_types));
  ("", "dr-warn-empty-body", 
    "warn when a method has no body and no annotation",
    Some(s_of_b conf.warn_empty_body));
  ("", "dr-emit-runtime-casts", 
    "create casts.rb to implement runtime casts",
    Some(s_of_b conf.emit_runtime_casts));
  ("", "dr-save-temps", 
    "do not remove temporary files created in /tmp",
    Some(s_of_b conf.save_temps));
  ("", "dr-type-inference", 
    "run the type inference algorithm",
    Some(s_of_b conf.type_inference));
]

let shared_options = [
  ("Idirectory", "", 
    "specify $LOAD_PATH directory (make backup if extension supplied)", 
    None);
  ("rlibrary", "", "require the library, before executing your script", 
    None);
  ("h", "help", "display this help message", None);
]

let ruby_excl_options = [
  ("0[octal]", "", "specify record separator (\\0, if no argument)", None);
  ("a", "", "autosplit mode with -n or -p (splits $_ into $F)", None);
  ("c", "", "check syntax only", None);
  ("Cdirectory", "", "cd to directory, before executing your script", None);
  ("d", "", "set debugging flags (set $DEBUG to true)", None);
  ("e 'command'", "",
    "one line of script. Several -e's allowed. Omit [programfile]", None);
  ("Fpattern", "", "split() pattern for autosplit (-a)", None);
  ("i[extension]", "",
    "edit ARGV files in place (make backup if extension supplied)", None);
  ("Kkcode", "", "specifies KANJI (Japanese) code-set", None);
  ("l", "", "enable line ending processing", None);
  ("n", "", "assume 'while gets(); ... end' loop around your script", None);
  ("p", "", "assume loop like -n but print line also like sed", None);
  ("s", "", "enable some switch parsing for switches after script name", None);
  ("S", "", "look for the script using PATH environment variable", None);
  ("T[level]", "", "turn on tainting checks", None);
  ("v", "", "print version number, then turn on verbose mode", None);
  ("w", "", "turn warnings on for your script", None);
  ("W[level]", "",
      "set warning level; 0=silence, 1=medium, 2=verbose (default)", None);
  ("x[directory]", "",
      "strip off text before #!ruby line and perhaps cd to directory", None);
]

(* [("option category", ["-flag"; "description"]), ...] *)
let refined_options =
  let sort_f f t1 t2 = String.compare (f t1) (f t2) in
  let fst_of_four (x, _, _, _) = x in
  let snd_of_four(_,x,_,_) = x in
  let druby_options = List.sort (sort_f snd_of_four) druby_excl_options in
  let shared_options = List.sort (sort_f fst_of_four) shared_options in
  let ruby_options = List.sort (sort_f fst_of_four) ruby_excl_options in
    [
      ("DRuby options", druby_options);
      ("Shared options", shared_options);
      ("Ruby options", ruby_options);
    ]

(* pretty prints an option's message *)
let show_msg ppf m =
  String.iter 
    (fun(c) -> 
      if c = ' ' then fprintf ppf "@ " 
      else fprintf ppf "%c" c
    )
    m

let show_default ppf = function
  | None -> fprintf ppf "."
  | Some(d) -> fprintf ppf "; the default is %s." d

(* pretty prints an option *)
let show_option ppf = function
    ("", "", _, _) -> failwith "should not happen."
  | (s, "", m, d) -> 
      fprintf ppf "@[  -%-24s @[<hov>%a%a@]@]@,@?" s show_msg m show_default d
  | ("", l, m, d) -> 
      fprintf ppf "@[  --%-23s @[<hov>%a%a@]@]@,@?" l show_msg m show_default d
  | (s, l, m, d) -> 
      fprintf ppf "@[  -%-2s --%-19s @[<hov>%a%a@]@]@,@?" s l show_msg m
        show_default d

(* displays the usage information *)
let show_usage () = 
  print_endline usage;
  let rec show_options = function
    | [] -> ()
    | (title, ops)::t -> 
        print_endline (title ^ ":");
        List.iter (show_option std_formatter) ops;
        show_options t
  in
    show_options refined_options;
    exit(0)

(* man page generator; run this in command-line to see the output
   $ druby --dr-gen-manpage | groff -man -Tascii | less
 *)
let generate_manpage () =
  let name = "DRuby - Diamondback Ruby" in
  let description =
"DRuby is a static analysis tool that aims to integrate static typing into 
Ruby. It includes a type annotation language and type inference system that 
has important features that allow it to accurately type Ruby programs."
  in
  let sl_tag tag str = printf ".%s %s\n" tag str in
  let ml_tag tag head str = printf ".%s %s\n%s\n" tag head str in
  let no_tag str = printf "%s\n" str in
  let rec do_option = function
    | [] -> ()
    | (_, l, m, d)::t -> 
        sl_tag "TP" "";
        no_tag ("--" ^ l); (
        match d with 
          | None -> no_tag m
          | Some(d) ->
              no_tag (m ^ "; the default is");
              sl_tag "B" (d ^ ".")
        );
        do_option t
  in
    sl_tag "TH" "DRuby 1";
    ml_tag "SH" "NAME" name;
    sl_tag "SH" "SYNOPSIS";
    sl_tag "B" "druby";
    no_tag "[";
    sl_tag "B" "options";
    no_tag "] file [--";
    sl_tag "B" "args";
    no_tag "]";
    ml_tag "SH" "DESCRIPTION" description;
    sl_tag "SH" "OPTIONS (DRUBY-specific)";
    do_option druby_excl_options;
    sl_tag "SH" "AUTHOR";
    no_tag "Mike Furr, Jong-hoon (David) An";
    exit(0)

(* Setters for conf *)
let s_to_i fname s =
  try int_of_string s 
  with Failure _ ->
    raise (Getopt.Error (sprintf "Invalid %s argument %s" fname s))
    
let s_to_b fname s =
  try bool_of_string s 
  with Failure _ ->
    raise (Getopt.Error (sprintf "Invalid %s argument %s" fname s))

let set_cache s = conf.cache <- s_to_b "dr-cache" s
let set_clean_cached s = conf.clean_cached <- s_to_b "dr-clean-cached" s
let set_debug_level s = conf.debug_level <- s_to_i "dr-debug-level" s
let set_error_raises_exc s = 
  conf.error_raises_exc <- s_to_b "dr-error-raises-exc" s
let set_profile s = conf.profile <- s_to_b "dr-profile" s
let set_profile_db s = conf.profile_db <- s
let set_transform s = conf.transform <- s_to_b "dr-transform" s
let set_use_annotations s = 
  conf.use_annotations <- s_to_b "dr-use-annotations" s
let set_check_annotations s = 
  conf.check_annotations <- s_to_b "dr-check-annotations" s
let set_stub_dir str = conf.stub_dir <- str
let set_use_base_types s = conf.use_base_types <- s_to_b "dr-use-base-types" s
let set_mine_dyn_feats s = 
  conf.mine_dyn_feats <- s_to_b "dr-mine-dyn-feats" s
let set_mining_output s = 
  conf.mining_output <- Some s
let set_print_error_ctx s = 
  conf.print_error_ctx <- s_to_b "dr-print-error-ctx" s
let set_no_dup_errors s = conf.no_dup_errors <- s_to_b "dr-no-dup-errors" s
let set_print_filenames s = 
  conf.print_filenames <- s_to_b "dr-print-filenames" s
let set_debug_constraints s = 
  conf.debug_constraints <- s_to_b "dr-debug-constraints" s
let set_resolve_scopes s = conf.resolve_scopes <- s_to_b "dr-resolve-scopes" s
let set_run_ruby s = conf.run_ruby <- s_to_b "dr-run-ruby" s
let set_idirectory s = conf.idirectory <- conf.idirectory @ [s]
let set_rlibrary s = conf.rlibrary <- conf.rlibrary @ [s]
let set_ruby_options n s =
  conf.ruby_options <- conf.ruby_options @ [n ^ s]
let set_ruby_args s = conf.ruby_args <- conf.ruby_args @ [s]
let set_single_entry s = conf.single_entry <- s_to_b "dr-single-entry" s
let set_show_time b = conf.show_time <- s_to_b "dr-show-time" b
let set_warn_empty_body b = 
  conf.warn_empty_body <- s_to_b "dr-warn-empty-body" b
let set_emit_runtime_casts b = 
  conf.emit_runtime_casts <- s_to_b "dr-emit-runtime-casts" b
let set_save_temps b = conf.save_temps <- s_to_b "dr-save-temps" b
let set_type_inference b = conf.type_inference <- s_to_b "dr-type-inference" b

(* returns a list of parse options for parsing; note that some option may not
   require a value; also, the given value is *NOT* a default value but rather
   default assignment when given no argument value *)
let opts parse_config = [
  ('\000', "dr-cache", Some (fun() ->  set_cache "true"), Some set_cache);
  ('\000', "dr-config", None, 
      Some (fun(s) -> conf.used_conf_file <- true; parse_config s));
  ('\000', "dr-debug-level", Some (fun() ->  set_debug_level "0"), 
      Some set_debug_level);
  ('\000', "dr-error-raises-exc", Some (fun() ->  set_error_raises_exc "true"), 
      Some set_error_raises_exc);
  ('\000', "dr-profile", Some (fun() ->  set_profile "true"),
      Some set_profile);
  ('\000', "dr-profile-db", None, Some set_profile_db);
  ('\000', "dr-transform", Some (fun() ->  set_transform "true"),
      Some set_transform);
  ('\000', "dr-use-annotations", Some (fun() ->  set_use_annotations "true"), 
      Some set_use_annotations);
  ('\000', "dr-check-annotations", 
      Some (fun() ->  set_check_annotations "true"), 
      Some set_check_annotations);
  ('\000', "dr-stub-dir", None, Some set_stub_dir);
  ('\000', "dr-use-base-types", Some (fun() ->  set_use_base_types "true"), 
      Some set_use_base_types);
  ('\000', "dr-mine-dyn-feats",
      Some (fun() -> set_mine_dyn_feats "true"),
      Some set_mine_dyn_feats);
  ('\000', "dr-clean-cached", 
      Some (fun() -> set_clean_cached "true"), None);
  ('\000', "dr-print-error-ctx", 
      Some (fun() ->  set_print_error_ctx "true"), 
      Some set_print_error_ctx);
  ('\000', "dr-no-dup-errors", 
      Some (fun() ->  set_no_dup_errors "true"), 
      Some set_no_dup_errors);
  ('\000', "dr-print-filenames", 
      Some (fun() ->  set_print_filenames "true"), 
      Some set_print_filenames);
  ('\000', "dr-debug-constraints", 
      Some (fun() ->  set_debug_constraints "true"), 
      Some set_debug_constraints);
  ('\000', "dr-resolve-scopes", 
      Some (fun() ->  set_resolve_scopes "true"), 
      Some set_resolve_scopes);
  ('\000', "dr-run-ruby", 
      Some (fun() ->  set_run_ruby "true"), 
      Some set_run_ruby);
  ('\000', "dr-show-time",
      Some (fun() ->  set_show_time "true"),
      Some set_show_time);
  ('\000', "dr-warn-empty-body",
      Some (fun() ->  set_warn_empty_body "true"),
      Some set_warn_empty_body);
  ('\000', "dr-emit-runtime-casts",
      Some (fun() ->  set_emit_runtime_casts "true"),
      Some set_emit_runtime_casts);
  ('\000', "dr-single-entry", 
      Some (fun() ->  set_single_entry "true"), 
      Some set_single_entry);
  ('\000', "dr-save-temps", 
      Some (fun() ->  set_save_temps "true"), 
      Some set_save_temps);
  ('\000', "dr-type-inference", 
      Some (fun() ->  set_type_inference "true"), 
      Some set_type_inference);
  ('h', "help", Some show_usage, None);
  (* shared with ruby *)
  ('I', "", None, Some set_idirectory);
  ('r', "", None, Some set_rlibrary);
  (* ruby option *)
  ('0', "", 
      Some (fun() ->  (set_ruby_options "-0") ""),
      Some (set_ruby_options "-0"));
  ('a', "", Some (fun() ->  (set_ruby_options "-a") ""), None);
  ('c', "", Some (fun() ->  (set_ruby_options "-c") ""), None);
  ('C', "", None, Some(set_ruby_options "-C"));
  ('d', "", Some (fun() ->  (set_ruby_options "-d") ""), None);
  ('e', "", None, Some (set_ruby_options "-e ")); (* notice the space! *)
  ('F', "", None, Some (set_ruby_options "-F"));
  ('i', "", 
      Some (fun() ->  (set_ruby_options "-i") ""), 
      Some (set_ruby_options "-i"));
  ('K', "", None, Some (set_ruby_options "-K"));
  ('l', "", Some (fun() ->  (set_ruby_options "-l") ""), None);
  ('n', "", Some (fun() ->  (set_ruby_options "-n") ""), None);
  ('p', "", Some (fun() ->  (set_ruby_options "-p") ""), None);
  ('s', "", Some (fun() ->  (set_ruby_options "-s") ""), None);
  ('S', "", Some (fun() ->  (set_ruby_options "-S") ""), None);
  ('T', "", 
      Some (fun() ->  (set_ruby_options "-T") ""), 
      Some (set_ruby_options "-T"));
  ('v', "", Some (fun() ->  (set_ruby_options "-v") ""), None);
  ('w', "", Some (fun() ->  (set_ruby_options "-w") ""), None);
  ('W', "", 
      Some (fun() ->  (set_ruby_options "-W") ""),
      Some (set_ruby_options "-W"));
  ('x', "", 
      Some (fun() ->  (set_ruby_options "-x") ""),
      Some (set_ruby_options "-x"));
  (* secret *)
  ('\000', "dr-gen-manpage", Some(generate_manpage), None);
]

let re = Str.regexp "^[ ]*\\([^ ]*\\)[ ]*=[ ]*\\([^ \t\r]*\\)[ \t\r]*$"
let ws_re = Str.regexp "^[ \t\r]*$"
let comment_re = Str.regexp "^[ ]*#.*$"

(* parses the specified config file; *fails* if any arg is invalid *)
let parse_config conf_file =
  (* parses each line and queue the parsed option *)
  let parse_line q l = 
    if Str.string_match re l 0
    then
      let arg = 
        "--" ^ (Str.matched_group 1 l) ^ "=" ^ (Str.matched_group 2 l) 
      in
        (* print_endline arg; *) Queue.push arg q
      (* Queue.push (Str.matched_group 2 l) q *)
    else if Str.string_match ws_re l 0
    then ()
    else if Str.string_match comment_re l 0
    then ()
    else (Printf.eprintf "syntax error in config file: '%s'\n%!" l;exit 1)
  in
  (* parses options from the given input channel *)
  let parse_all ic = 
    let q = Queue.create () in
      try while true do
        parse_line q (input_line ic)
      done;
      assert false
    with End_of_file -> q
  in
    try 
      let ic = open_in conf_file in
      let q = parse_all ic in
      let l = Queue.length q in
      let ary = Array.init l (fun _ -> Queue.pop q) in
      let fail_parse _ = 
        failwith "Cannot use config_file option in configuration file!" 
      in
      let anon _ = failwith "configuration syntax error" in
        close_in ic;
        try Getopt.parse (opts fail_parse) anon ary 0 ((Array.length ary) - 1)
        with Failure s -> Printf.eprintf "configuration syntax error: %s\n%!" s; exit 1
    with
      | Sys_error _ ->
          Printf.eprintf "unable to find/open configuration file %s\n%!" conf_file;
          exit 1

(* parses the command line arguments and the config file if exists *)
let parse_cmdline () = 
  let etc_cf = Filename.concat Build_vars.sysconfdir "druby.conf" in
  let home_cf = 
    try (Sys.getenv "HOME") ^ "/.druby.conf"
    with Not_found -> ""
  in
  let cwd_cf = (Sys.getcwd ()) ^ "/druby.conf" in
  let () =
    if (Sys.file_exists etc_cf) then parse_config etc_cf;
    if (Sys.file_exists home_cf) then parse_config home_cf;
    if (Sys.file_exists cwd_cf) then parse_config cwd_cf
  in
  let anon s = 
    if conf.ruby_file = "" then conf.ruby_file <- s
    else conf.ruby_args <- conf.ruby_args @ [s]
  in
    Getopt.parse_cmdline (opts parse_config) anon;
    if conf.ruby_file = "" then 
      (print_endline "**No ruby file has been specified."; show_usage ())

