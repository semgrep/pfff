(*
 * The author disclaims copyright to this source code.  In place of
 * a legal notice, here is a blessing:
 *
 *    May you do good and not evil.
 *    May you find forgiveness for yourself and forgive others.
 *    May you share freely, never taking more than you give.
 *)
open Common

module Flag = Flag_parsing
module E = Error_code
module PI = Parse_info
module J = Json_type

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(* 
 * A lint-like checker. https://github.com/facebook/pfff/wiki/Scheck
 * Right now there is support mainly for PHP, as well as for 
 * C, Java, and OCaml (via graph_code_checker.ml), and for Python
 * and Javascript (via the generic AST).
 * 
 * Note that scheck is mostly for generic bugs (which sometimes
 * requires global analysis). For API-specific bugs, use 'sgrep'.
 * todo: implement SgrepLint for scheck.
 * 
 * 'scheck' can leverage expensive global analysis results to find more 
 * bugs. It can use a light database (see pfff_db) or a graph_code database
 * (see codegraph).
 * 
 * todo:
 *  - https://medium.com/@Coder_HarryLee/the-way-static-analyzers-fight-against-false-positives-and-why-they-do-it-743de1f2a1bd#.x1ams12x6
 *  - more soft quality bugs, e.g. deadcode, dead parameter,
 *    passing whole structure when could just pass a subfield,
 *    wrong cohesion where a function should be in another file
 *    (e.g. when use 5 functions from another file and none of your file),
 *    or when a constant is used only in another file,
 *    etc
 * 
 * related:
 *  - https://atom.io/packages/linter
 *)

(*---------------------------------------------------------------------------*)
(* PHP specific doc *)
(*---------------------------------------------------------------------------*)
(*
 * By default 'scheck' performs only a local analysis of the file(s) passed
 * on the command line. It is thus quite fast while still detecting a few
 * important bugs like the use of undefined variables. 
 * It can also leverage more expensive global analysis to find more 
 * bugs. Doing so requires a PHP "code database", abstracted below
 * under the 'entity_finder' interface. 
 * 
 * Computing a code database is usually expensive to 
 * build (see pfff_db_heavy) and takes lots of space. 
 * Fortunately one can now build this database in memory, on the fly. 
 * Indeed, thanks to the include_require_php.ml analysis, we can now
 * build only the db for the files that matters, cutting significantly
 * the time to build the db (going down from 40 000 files to about 1000
 * files on average on facebook code). In a way it is similar
 * to what gcc does when it calls 'cpp' to get the full information for
 * a file. 
 * 
 * 'scheck' could also use the heavy database but this requires to have
 * the program linked with Berkeley DB, adding some dependencies to 
 * the user of the program. But because BDB is not very multi-user
 * friendly for now, and because Berkeley DB has been deprecated
 * in favor of the Prolog database (see main_codequery.ml) or 
 * graph_code database (see main_codegraph.ml), this option is not
 * supported anymore.
 * 
 * modes:
 *  - local analysis
 *  - perform global analysis "lazily" by building db on-the-fly
 *    of the relevant included files (configurable via a -depth_limit flag)
 *  - TODO leverage global analysis computed previously by pfff_db(light)
 *  - leverage global analysis computed previously by codegraph
 *  - nomore: global analysis computed by main_scheck_heavy.ml
 * 
 * current checks:
 *   - variable related (use of undeclared variable, unused variable, etc)
 *     with good handling of references false positives when have a code db
 *   - use/def of entities (e.g. use of undefined class/function/constant
 *     a la checkModule)
 *   - function call related (wrong number of arguments, bad keyword
 *     arguments, etc)
 *   - SEMI class related (use of undefined member, wrong number of arguments
 *     in method call, etc)
 *   - include/require and file related (e.g. including file that do not
 *     exist anymore); needs to pass an env
 *   - SEMI dead code (dead function in callgraph, DONE dead block in CFG,
 *     dead assignement in dataflow)
 *   - TODO type related
 *   - TODO resource related (open/close match)
 *   - TODO security related? via sgrep?
 *   - TODO require_strict() related (see facebook/.../main_linter.ml)
 * 
 * related: 
 *   - TODO lint_php.ml (small syntactic conventions, e.g. bad defines)
 *   - TODO check_code_php.ml (include/require stuff)
 *   - TODO check_module.ml (require_module() stuff), 
 *   - TODO main_linter.ml (require_strict() stuff), 
 *   - TODO main_checker.ml (flib-aware  checker),
 * 
 * todo: 
 *  - make it possible to take a db in parameter so
 *    for other functions, we can also get their prototype.
 *  - https://github.com/php-fig/fig-standards/blob/master/accepted/PSR-2-coding-style-guide.md
 * 
 * The checks leverage also info about builtins, so when one calls preg_match(),
 * we know that this function takes things by reference which avoids
 * some false positives regarding the use of undeclared variable
 * for instance.
 * 
 * later: it could later also check javascript, CSS, sql, etc
 *)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

(* less: infer from basename argv(0) ? *)
let lang = ref "python"

(* show only bugs with "rank" superior to this *)
let filter = ref 2
(* rank errors *)
let rank = ref false

(* see also the -report_xxx in error_code.ml *)

(* In strict mode, we can be more aggressive regarding scope like in JsLint. 
 * (This is a copy of a similar variable in Error_php.ml) 
*)
let strict = ref false

let auto_fix = ref false

(* running heavy analysis using the graph_code as the entity finder *)
let graph_code = ref (None: Common.filename option)

(* for codemap or layer_stat *)
let layer_file = ref (None: filename option)

(* display *)
let verbose = ref false
let show_progress = ref true
let r2c = ref false

(* action mode *)
let action = ref ""

(*---------------------------------------------------------------------------*)
(* language specific flags *)
(*---------------------------------------------------------------------------*)

(* running the heavy analysis by processing the included files *)
let heavy = ref false
(* depth_limit is used to stop the expensive recursive includes process.
 * I put 5 because it's fast enough at depth 5, and 
 * I think it's good enough as it is probably bad for a file to use
 * something that is distant by more than 5 includes. 
 * 
 * todo: one issue is that some code like facebook uses special 
 *  require/include directives that include_require_php.ml is not aware of.
 *  Maybe we should have a unfacebookizer preprocessor that removes
 *  this sugar. The alternative right now is to copy most of the code
 *  in this file in facebook/qa_code/checker.ml :( and plug in the
 *  special include_require_php.ml hooks. 
 * Another alternative is to use the light_db or graph_code for the 
 * entity finder.
 *)
let depth_limit = ref (Some 5: int option)

let php_stdlib = 
  ref (Filename.concat Config_pfff.path "/data/php_stdlib")

(* old: main_scheck_heavy: let metapath = ref "/tmp/pfff_db" *)

(* take care, putting this to true can lead to performance regression
 * actually, because of the bigger stress on the GC
 *)
let cache_parse = ref false

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr2_dbg s =
  if !verbose then pr2 s

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let set_gc () =
(*
  if !Flag.debug_gc
  then Gc.set { (Gc.get()) with Gc.verbose = 0x01F };
*)
  (* see www.elehack.net/michael/blog/2010/06/ocaml-memory-tuning *)
  Gc.set { (Gc.get()) with Gc.minor_heap_size = 2_000_000 };
  Gc.set { (Gc.get()) with Gc.space_overhead = 200 };
  ()

(* for testing *)
let ast_of_file file =
  let prog = Parse_python.parse_program file in
  Python_to_generic.program prog

(*****************************************************************************)
(* Language specific *)
(*****************************************************************************)

(*---------------------------------------------------------------------------*)
(* PHP Entity finders *)
(*---------------------------------------------------------------------------*)

(* Build the database of information. Take the name of the file
 * we want to check and process all the files that are needed (included)
 * to check it, a la cpp.
 * 
 * Some checks needs to have a global view of the code, for instance
 * to know what are the sets of valid protected variable that can be used
 * in a child class.
 *)
let build_mem_db _file =
(*

  (* todo: could infer PHPROOT at least ? just look at
   * the include in the file and see where the files are.
   *)
  let env = 
    Env_php.mk_env (Common2.dirname file)
  in
  let root = "/" in (* todo ? *)

  let all_files = 
    Include_require_php.recursive_included_files_of_file 
      ~verbose:!verbose 
      ~depth_limit:!depth_limit
      env file
  in
  let builtin_files =
    Lib_parsing_php.find_php_files_of_dir_or_files [!php_stdlib]
  in
  Common.save_excursion Flag_analyze_php.verbose_database !verbose (fun()->
    Database_php_build.create_db
      ~db_support:(Database_php.Mem)
      ~phase:2 (* TODO ? *)
      ~files:(Some (builtin_files ++ all_files))
      ~verbose_stats:false
      ~annotate_variables_program:None
      (Database_php.prj_of_dir root) 
  )
*)
  raise Todo

let entity_finder_of_db file =
  let _db = build_mem_db file in
  raise Todo
(*
  Database_php_build.build_entity_finder db
*)

let entity_finder_of_graph_file graph_file root =
  let g = Graph_code.load graph_file in
  pr2 (spf "using %s for root" root);
  (* todo: the graph_code contains absolute path?? *)
  (Entity_php.entity_finder_of_graph_code g root, g)

(*---------------------------------------------------------------------------*)
(* C++ false positive checker *)
(*---------------------------------------------------------------------------*)

(* mv types in a generic file in h_program-lang/? generalize this code
 * to use different lang, and so pass different tokenize and TIdent extractor.
 *)
module T = Parser_cpp
let build_identifier_index lang xs =
  let _root, files = 
    match xs with
    | [root] -> 
        root, Find_source.files_of_root ~lang root
    | _ ->
        let root = Common2.common_prefix_of_files_or_dirs xs in
        let files = 
          Find_source.files_of_dir_or_files ~lang xs in
        root, files
  in

  (* we use the Hashtbl.find_all property for this h *)
  let h = Hashtbl.create 101 in
  (* we don't here *)
  let hcnt = Hashtbl.create 101 in
  Flag_parsing.verbose_lexing := false;
  files |> List.iter (fun file ->
    let toks = Parse_cpp.tokens file in
       
    toks |> List.iter (fun tok ->
      match tok with
      | T.TIdent (s, info) ->
          if Hashtbl.mem hcnt s
          then 
            let cnt = Hashtbl.find hcnt s in
            if cnt > 10 then ()
            else begin
              Hashtbl.replace hcnt s (cnt + 1);
              Hashtbl.add h s info
            end
          else begin
            Hashtbl.add hcnt s 1;
            Hashtbl.add h s info
          end
      | _ -> ()
  ));
  hcnt, h


(* todo: could have more than 2 clients and still be dead
 * if was recursive function
 *)
let false_positive_detector hidentifier g errors =
  errors |> Common.exclude (fun err ->
    match err.Error_code.typ with
    | Error_code.Deadcode ((s, Entity_code.Function) as n) ->
        let short = Graph_code.shortname_of_node n in
        let occurences = Hashtbl.find_all hidentifier short in
        let expected_minimum =
          if Graph_code.has_node (s, Entity_code.Prototype) g
          then 2 
          else 1
        in
        let fp = List.length occurences > expected_minimum in
        if fp
        then pr2_dbg (spf "%s (FP deadcode?)" (Error_code.string_of_error err));
        fp
    | _ -> false
  )

(*****************************************************************************)
(* Main action *)
(*****************************************************************************)

let main_action xs =
  set_gc ();

  let xs = List.map Common.fullpath xs in
  (* less: could use Common2.find_common_root? *)
  let root =
    match xs with
    | [x] when Common2.is_directory x -> x
    | _ -> "/"
  in
  let lang = !lang in
  let files = Find_source.files_of_dir_or_files ~lang xs in

  match lang with
  | s when Lang.lang_of_string_opt s <> None ->
(*---------------------------------------------------------------------------*)
(* AST generic checker *)
(*---------------------------------------------------------------------------*)

      let find_entity = None in
      
      files |> Console.progress ~show:!show_progress (fun k -> 
        List.iter (fun file ->
          k();
          Error_code.try_analyze_file_with_exn_to_errors file (fun () ->
            pr2_dbg (spf "processing: %s" file);
            let ast = 
              Common.save_excursion Flag.error_recovery false (fun () ->
              Common.save_excursion Flag.exn_when_lexical_error true (fun () ->
              Common.save_excursion Flag.show_parsing_error false (fun () ->
                Parse_generic.parse_program file 
             ))) in
            Check_all_generic.check_file ~find_entity ast;
          );
          if !rank || !r2c
          then ()
          else begin 
            let errs = 
              !E.g_errors 
              |> List.rev 
              |> List.filter (fun x -> 
                E.score_of_rank (E.rank_of_error x) >= !filter
              ) 
              |>  E.filter_maybe_parse_and_fatal_errors in
            errs |> List.iter (fun err -> pr (E.string_of_error err));
            E.g_errors := []
          end
        )
    );

    if !rank || !r2c then begin
      let errs = 
        if !rank
        then 
          !E.g_errors 
          |> List.map (fun x -> x, E.rank_of_error x)
          |> Common.sort_by_val_highfirst 
          |> List.map fst
          |> Common.take_safe 20 
        else !E.g_errors |> E.filter_maybe_parse_and_fatal_errors
      in
      if !r2c 
      then 
        let errs = E.adjust_paths_relative_to_root root errs in
        pr (R2c.string_of_errors "scheck" errs)
      else errs |> List.iter (fun err -> pr (E.string_of_error err))
    end

(*---------------------------------------------------------------------------*)
(* Graphcode-based checker *)
(*---------------------------------------------------------------------------*)

  | "ocaml" | "java" | "c" | "php" | "clang2"  ->
    let graph_file, _root =
      match xs, !graph_code with
      | _,    Some file -> file, Filename.dirname file
      | [dir], _ -> Filename.concat dir Graph_code.default_filename, dir
      | x::xs, _ ->
          let root = Common2.common_prefix_of_files_or_dirs (x::xs) in
          Filename.concat root Graph_code.default_filename, root
      | [], _ -> failwith (spf "%s checker needs a graph file" lang);
    in
    if not (Sys.file_exists graph_file)
    then failwith (spf "%s checker needs a graph file" lang);

    let g = Graph_code.load graph_file in
    let errs = Graph_code_checker.check g in
    (* todo: make this more lazy? it's pretty slow *)
    let hidentifier =
      if lang = "clang2" (* not for "c"! graph_code_c is robust enough :) *)
      then build_identifier_index (if lang = "clang2" then "c++" else lang) xs |> snd
      else Hashtbl.create 0
    in

    let errs = 
      errs 
      |> false_positive_detector hidentifier g
      |> Error_code.adjust_errors
      |> List.filter (fun err -> (Error_code.score_of_error err) >= !filter)
    in
    let errs = 
      if !rank 
      then
        errs |> List.map (fun err -> err, Error_code.rank_of_error err)
        |> Common.sort_by_val_highfirst
        |> Common.take_safe 40
        |> List.map fst
      else errs
    in
    errs |> List.iter (fun err ->
      (* less: confront annotation and error kind *)
      if Error_code.annotation_at err.Error_code.loc <> None
      then pr2_dbg (spf "%s (Skipping @)" (Error_code.string_of_error err))
      else pr2 (Error_code.string_of_error err)
    )

(*---------------------------------------------------------------------------*)
(* PHP checker *)
(*---------------------------------------------------------------------------*)

  | "php2" ->

    Flag_parsing.show_parsing_error := false;
    Flag_parsing.verbose_lexing := false;
    Error_php.strict := !strict;
    (* less: use a VCS.find... that is more general ?
     * infer PHP_ROOT? or take a --php_root?
     *)
    let an_arg = List.hd xs |> Common2.relative_to_absolute in
    let root = 
      try Git.find_root_from_absolute_path an_arg 
      with Not_found -> "/"
    in
    pr (spf "using %s for php_root" root);
    let env = Env_php.mk_env root in

    let (find_entity, graph_opt) =
      match () with
      | _ when !heavy ->
        Some (entity_finder_of_db (List.hd files)), None
      | _ when !graph_code <> None ->
        let (e, g) = 
          entity_finder_of_graph_file (Common2.some !graph_code) root in
        Some e, Some g
        (* old: main_scheck_heavy:
         * Database_php.with_db ~metapath:!metapath (fun db ->
         *  Database_php_build.build_entity_finder db
         *) 
      | _ -> None, None
    in
  
    Common.save_excursion Flag_parsing_php.caching_parsing !cache_parse (fun ()->
      files |> Console.progress ~show:!show_progress (fun k -> 
        List.iter (fun file ->
          k();
          try 
            pr2_dbg (spf "processing: %s" file);
            Check_all_php.check_file ~find_entity env file;
            (match graph_opt with
              | None -> ()
              | Some graph ->
                Check_classes_php.check_required_field graph file
            );
            let errs = 
              !Error_php._errors 
              |> List.rev 
              |> List.filter (fun x -> 
                Error_php.score_of_rank
                  (Error_php.rank_of_error_kind x.Error_php.typ) >= 
                  !filter
              )
            in
            if not !rank 
            then begin 
              errs |> List.iter (fun err -> pr (Error_php.string_of_error err));
              if !auto_fix then errs |> List.iter Auto_fix_php.try_auto_fix;
              Error_php._errors := []
            end
          with 
            | (Timeout | UnixExit _) as exn -> raise exn
            (*  | (Unix.Unix_error(_, "waitpid", "")) as exn -> raise exn *)
            | exn ->
              pr2 (spf "PB with %s, exn = %s" file (Common.exn_to_s exn));
              if !Common.debugger then raise exn
        )));

    if !rank then begin
      let errs = 
        !Error_php._errors 
        |> List.rev
        |> Error_php.rank_errors
        |> Common.take_safe 20 
      in
      errs |> List.iter (fun err -> pr (Error_php.string_of_error err));
      Error_php.show_10_most_recurring_unused_variable_names ();
      pr2 (spf "total errors = %d" (List.length !Error_php._errors));
      pr2 "";
      pr2 "";
    end;
    
    !layer_file |> Common.do_option (fun file ->
      (*  a layer needs readable paths, hence the root *)
      let root = Common2.common_prefix_of_files_or_dirs xs in
      Layer_checker_php.gen_layer ~root ~output:file !Error_php._errors
    );


  | _ -> failwith ("unsupported language: " ^ lang)
  

(*****************************************************************************)
(* Extra actions *)
(*****************************************************************************)

(*---------------------------------------------------------------------------*)
(* PHP type inference playground *)
(*---------------------------------------------------------------------------*)

let type_inference _file =
  raise Todo
(*

  let ast = Parse_php.parse_program file in

  (* PHP Intermediate Language *)
  try
    let pil = Pil_build.pil_of_program ast in

    (* todo: how bootstrap this ? need a bottom-up analysis but
     * we could first start with the types of the PHP builtins that
     * we already have (see builtins_php.mli in lang_php/analyze/).
     *)
    let env = () in

    (* works by side effect on the pil *)
    Type_inference_pil.infer_types env pil;

    (* simple pretty printer *)
    let s = Pretty_print_pil.string_of_program pil in
    pr s;

    (* internal representation pretty printer *)
    let s = Meta_pil.string_of_program
      ~config:{Meta_pil.show_types = true; show_tokens = false}
      pil
    in
    pr s;

  with exn ->
    pr2 "File contain constructions not supported by the PIL; bailing out";
    raise exn
*)

(* Dataflow analysis *)
let dflow file_or_dir =
  let file_or_dir = Common.fullpath file_or_dir in
  let files = Lib_parsing_php.find_source_files_of_dir_or_files [file_or_dir] in
  let dflow_of_func_def def =
    (try
       let flow = Controlflow_build_php.cfg_of_func def in
       let mapping = Dataflow_php.reaching_fixpoint flow in
       Dataflow_php.display_reaching_dflow flow mapping;
     with
     | Controlflow_build_php.Error err ->
       Controlflow_build_php.report_error err
     | Todo -> ()
     | Failure _ -> ()
    )
  in
  List.iter (fun file ->
    (try
       let ast = Parse_php.parse_program file in
       ast |> List.iter (function
       | Cst_php.FuncDef def ->
         dflow_of_func_def def
       | Cst_php.ClassDef def ->
         Cst_php.unbrace def.Cst_php.c_body |> List.iter
           (function
           | Cst_php.Method def -> dflow_of_func_def def
           | _ -> ())
       | _ -> ())
     with _ -> pr2 (spf "fail: %s" file)
    )) files

(*---------------------------------------------------------------------------*)
(* Poor's man token-based Deadcode detector for C/C++/...  *)
(*---------------------------------------------------------------------------*)
module Ent = Entity_code
module Ast = Ast_fuzzy
module V = Lib_ast_fuzzy
let entities_of_ast ast =
  let res = ref [] in
  let visitor = V.mk_visitor { V.default_visitor with
    V.ktrees = (fun (k, _) xs ->
      (match xs with
      | Ast.Tok (s, _)::Ast.Parens _::Ast.Braces _::_res ->
          Common.push (s, Ent.Function) res;
      | _ ->  ()
      );
      k xs
    )
  }
  in
  visitor ast;
  !res

let test_index xs =
  let xs = List.map Common.fullpath xs in
  let hcnt, h = build_identifier_index "c++" xs in
(*
  hcnt |> Common.hash_to_list |> Common.sort_by_val_lowfirst 
  |> Common.take_safe 50 |> List.iter pr2_gen
*)
  hcnt |> Common.hash_to_list |> List.iter (fun (s, cnt) ->
    if cnt = 1 then
      let info = Hashtbl.find h s in
      let file = PI.file_of_info info in
      if file =~ ".*\\.c" 
      then begin
        (* pr2 (spf "found? %s in %s" s file); *)
        let (ast, _toks) = 
      try Parse_cpp.parse_fuzzy file
      with exn -> 
        pr2 (spf "PB fuzzy on %s (exn = %s)" file (Common.exn_to_s exn));
        [], []
    in
        let entities = entities_of_ast ast in
        (match Common2.assoc_opt s entities with
        | Some Ent.Function ->
            pr2 (spf "DEAD FUNCTION? %s in %s" s file)
        | _ -> ()
        )
      end
  )

(*****************************************************************************)
(* Regression testing *)
(*****************************************************************************)
open OUnit

let test () =
  let suite = "scheck" >:::[
      Unit_linter.unittest ~ast_of_file;
      Unit_checker_php.unittest;
  ]
  in
  OUnit.run_test_tt suite |> ignore;
  ()

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)
     
let extra_actions () = [
  "-test", " run regression tests",
  Common.mk_action_0_arg test;

  "-test_type_inference", " <file>",
  Common.mk_action_1_arg type_inference;
  "-test_dflow", " <file/folder> run dataflow analysis",
  Common.mk_action_1_arg dflow;
  "-test_unprogress", " ",
  Common.mk_action_1_arg (fun file ->
    Common.cat file |> List.iter (fun s ->
      if s =~ ".*\\(flib/[^ ]+ CHECK: [^\n]+\\)"
      then pr (Common.matched1 s)
      else ()
    );
  );

  "-test_index", " <dirs>",
  Common.mk_action_n_arg test_index;
]

(*****************************************************************************)
(* The options *)
(*****************************************************************************)

let all_actions () =
 extra_actions() @
 Test_parsing_generic.actions() @
 Test_analyze_generic.actions() @
 []

let options () =
  [
    "-lang", Arg.Set_string lang, 
    (spf " <str> choose language (default = %s)" !lang);

    (* error filtering *)
    "-filter", Arg.Set_int filter,
    " <n> show only bugs whose importance >= n";
    "-rank", Arg.Set rank,
    " rank errors and display the 20 most important";

    (* error display *)
    "-emacs", Arg.Unit (fun () -> show_progress := false;), 
    " emacs friendly output";
    "-r2c", Arg.Unit (fun () ->
        r2c := true;
        show_progress := false),
    " use r2c platform error format for output";

    (* extra features *)
    "-with_graph_code", Arg.String (fun s -> graph_code := Some s), 
    " <file> use graph_code file for heavy analysis";
    "-gen_layer", Arg.String (fun s -> layer_file := Some s),
    " <file> save result in pfff layer file";
    "-auto_fix", Arg.Set auto_fix,
    " try to auto fix the error\n";


    (* php specific *)
    "-php_stdlib", Arg.Set_string php_stdlib, 
    (spf " <dir> path to builtins (default = %s)" !php_stdlib);
    "-strict", Arg.Unit (fun () ->
        strict := true;
        Error_code.report_parse_errors := true;
        Error_code.report_fatal_errors := true;
    ),
    " emulate block scope instead of function scope";
    "-no_scrict", Arg.Clear strict, 
    " use function scope (default)";
    "-heavy", Arg.Set heavy,
    " process included files for heavy analysis";
    "-depth_limit", Arg.Int (fun i -> depth_limit := Some i), 
    " <int> limit the number of includes to process";
    "-caching", Arg.Clear cache_parse, 
    " cache parsed ASTs";

  ] @
  Error_code.options () @
  Common2.cmdline_flags_devel () @
  Common.options_of_actions action (all_actions()) @
  [
    "-verbose", Arg.Unit (fun () -> 
      verbose := true;
      Flag_analyze_php.verbose_entity_finder := true;
    ),
    " guess what";
    "-version",   Arg.Unit (fun () ->
      pr2 (spf "scheck version: %s" Config_pfff.version);
      exit 0;
    ), " guess what";
  ]
(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let main () =

  let usage_msg =
    spf "Usage: %s [options] <file or dir> \nDoc: %s\nOptions:"
      (Common2.basename Sys.argv.(0))
      "https://github.com/facebook/pfff/wiki/Scheck"
  in
  (* does side effect on many global flags *)
  let args = Common.parse_options (options()) usage_msg Sys.argv in

  (* must be done after Arg.parse, because Common.profile is set by it *)
  Common.profile_code "Main total" (fun () ->

    (match args with

    (* --------------------------------------------------------- *)
    (* actions, useful to debug subpart *)
    (* --------------------------------------------------------- *)
    | xs when List.mem !action (Common.action_list (all_actions())) ->
        Common.do_action !action xs (all_actions())

    | _ when not (Common.null_string !action) ->
        failwith ("unrecognized action or wrong params: " ^ !action)

    (* --------------------------------------------------------- *)
    (* main entry *)
    (* --------------------------------------------------------- *)
    | x::xs ->
        main_action (x::xs)

    (* --------------------------------------------------------- *)
    (* empty entry *)
    (* --------------------------------------------------------- *)
    | [] ->
        Common.usage usage_msg (options())
    )
  )

(*****************************************************************************)
let _ =
  Common.main_boilerplate (fun () ->
    main ();
  )
