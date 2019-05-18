(* Yoann Padioleau
 *
 * Copyright (C) 2019 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 * 
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open Common

module E = Entity_code
module G = Graph_code
module PI = Parse_info

open Ast_js
module Ast = Ast_js

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Graph of dependencies for Javascript. See graph_code.ml
 * and main_codegraph.ml for more information.
 * 
 * schema:
 *  Root -> Dir -> File -> Function
 *                      -> Class (and Obj)
 *                            -> TODO Field
 *                      -> Const
 *                      -> Global TODO need type to know if not a func or class
 *       -> Dir -> SubDir -> ...
 *
 * todo: 
 *  - look at the deadcode detected by codegraph on the graph built
 *    by this file; they usually contains FP indicating bugs in this file
 *  - too many stuff
 *)
(* TODO: flow/lib/ contains declarations of all JS builtins! *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* for the extract_uses visitor *)
type env = {
  g: Graph_code.graph;

  phase: phase;

  current: Graph_code.node;
  file_readable: Common.filename;

  (* imports of external entities and abused to create
   * fake imports of the entities defined in the current file *)
  imports: (string, qualified_name (* orig name *)) Hashtbl.t;
  (* covers also the parameters; I handle block scope by not using
   * a ref of mutable here! Just build a new list and passed it down.
   *)
  locals: string list;
  (* 'var' have a function scope.
   * alt: lift var up in a ast_js_build.ml transforming phase
   *)
  vars: (string, bool) Hashtbl.t;

  (* todo: use it *)
  exports: (Common.filename, string list) Hashtbl.t;
  (* error reporting *)
  dupes: (Graph_code.node, bool) Hashtbl.t;

  log: string -> unit;
  pr2_and_log: string -> unit;
}
 and phase = Defs | Uses

(*****************************************************************************)
(* Parsing *)
(*****************************************************************************)

(* because we use a 2 passes process (should do like in PHP all in 1 pass?) *)

let _hmemo = Hashtbl.create 101

let parse file =
  Common.memoized _hmemo file (fun () ->
    try 
      let cst = Parse_js.parse_program file in
      (* far easier representation to work on than the CST *)
      Ast_js_build.program cst
    with
    | Timeout -> raise Timeout
    | exn ->
      pr2 (spf "PARSE ERROR with %s, exn = %s" file (Common.exn_to_s exn));
      raise exn
  )

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let error s tok =
  failwith (spf "%s: %s" (Parse_info.string_of_info tok) s)

let s_of_n n = 
  Ast.str_of_name n

let pos_of_tok tok file =
  { (Parse_info.token_location_of_info tok) with PI.file }

(*****************************************************************************)
(* File resolution *)
(*****************************************************************************)

(* resolve . and .. to normalize path and qualified names in the end *)
let normalize_path tok xs =
  let rec aux acc xs =
    match xs with 
    | [] -> List.rev acc
    | x::xs ->
      (match x, acc with
      | ".", _ -> aux acc xs
      | "..", [] -> 
        error "could not .." tok
      | "..", _::acc -> aux acc xs
      | s, acc -> aux (s::acc) xs
      )
  in
  aux [] xs           

let readable_of_path env (file, tok) =
  let xs = Filename.dirname env.file_readable |> Str.split (Str.regexp "/") in
  let ys = Str.split (Str.regexp "/") file in
  let zs = normalize_path tok (xs @ ys) in
  Common.join "/" zs

(*****************************************************************************)
(* Qualified Name *)
(*****************************************************************************)

let mk_qualified_name readable s =
  assert (not (readable =~ "^\\./"));
  let str = Filename.chop_extension readable in
  str ^ "." ^ s

let qualified_name env name =
  (let s = s_of_n name in
  if Hashtbl.mem env.imports s
  then Hashtbl.find env.imports s
  else s
  )|> (fun s -> assert (not (s =~ "^\\./")); s)

(*****************************************************************************)
(* Other helpers *)
(*****************************************************************************)
let is_local env n =
  let s = s_of_n n in
  List.mem s env.locals || Hashtbl.mem env.vars s

let add_locals env vs = 
  let locals = vs |> Common.map_filter (fun v ->
    let s = s_of_n v.v_name in
    match v.v_kind with
    | Let | Const -> Some s
    | Var ->
        Hashtbl.replace env.vars s true;
        None
     ) in
  { env with locals = locals @ env.locals } 

let kind_of_expr v_kind e =
  match e with
  | Fun _ -> E.Function
  | Class _ -> E.Class
  | Obj _ -> E.Class
  | _ -> 
        (* without types, this might be wrong; a constant might
         * actually refer to a function, and a global to an object
         *)
        if v_kind = Const 
        then E.Constant
        else E.Global

(*****************************************************************************)
(* Add Node *)
(*****************************************************************************)
let add_node_and_edge_if_defs_mode env (name, kind) =
  let str = s_of_n name in
  let str' =
    match env.current with
    | (readable, E.File) -> mk_qualified_name readable str
    | (s, _) -> s ^ "." ^ str
  in
  let node = (str', kind) in

  if env.phase = Defs then begin
    match () with
    (* if parent is a dupe, then don't want to attach yourself to the
     * original parent, mark this child as a dupe too.
     *)
    | _ when Hashtbl.mem env.dupes env.current ->
        Hashtbl.replace env.dupes node true

    (* already there? a dupe? *)
    | _ when G.has_node node env.g ->
        env.pr2_and_log (spf "DUPE entity: %s" (G.string_of_node node));
        let orig_file = G.file_of_node node env.g in
        env.log (spf " orig = %s" orig_file);
        env.log (spf " dupe = %s" env.file_readable);
        Hashtbl.replace env.dupes node true;
    (* ok not a dupe, let's add it then *)
    | _ ->
      (* try but should never happen, see comment below *)
      try
        let pos = pos_of_tok (snd name) env.file_readable in
        let nodeinfo = { Graph_code. pos; typ = None; props = []; } in
        env.g |> G.add_node node;
        env.g |> G.add_edge (env.current, node) G.Has;
        env.g |> G.add_nodeinfo node nodeinfo;
      (* this should never happen, but it's better to give a good err msg *)
      with Not_found ->
        error ("Not_found:" ^ str) (snd name)
  end;
  if Hashtbl.mem env.dupes node
  then env
  else { env with current = node }

(*****************************************************************************)
(* Add edges *)
(*****************************************************************************)
let add_use_edge env (name, kind) =
  let s = qualified_name env name in
  let src = env.current in
  let dst = (s, kind) in
  match () with
  | _ when Hashtbl.mem env.dupes src || Hashtbl.mem env.dupes dst ->
      (* todo: stats *)
      env.pr2_and_log (spf "skipping edge (%s -> %s), one of it is a dupe"
                         (G.string_of_node src) (G.string_of_node dst));

  | _ when not (G.has_node src env.g) ->
      error (spf "SRC FAIL: %s (-> %s)" 
               (G.string_of_node src) (G.string_of_node dst)) (snd name)
  (* the normal case *)
  | _ when G.has_node dst env.g ->
      G.add_edge (src, dst) G.Use env.g;

  | _ ->
    env.pr2_and_log (spf "Lookup failure on %s (%s)"
                       (G.string_of_node dst)
                       (Parse_info.string_of_info (snd name)))

let add_use_edge_candidates env (name, kind) =
  let kind = 
    let s = qualified_name env name in
    let dst = (s, kind) in
    if G.has_node dst env.g
    then kind
    else
      let candidates = [E.Function; E.Class; E.Constant; E.Global] in
      let valids = candidates |> List.filter (fun k -> 
           G.has_node (s, k) env.g) in
      (match valids with
      | [x] -> x
      | _ -> kind (* default to first kind, but could report error *)
      )
  in
  add_use_edge env (name, kind)

(*****************************************************************************)
(* Defs/Uses *)
(*****************************************************************************)

let rec extract_defs_uses env ast =
  if env.phase = Defs then begin
    let dir = Common2.dirname env.file_readable in
    G.create_intermediate_directories_if_not_present env.g dir;
    let node = (env.file_readable, E.File) in
    env.g |> G.add_node node;
    env.g |> G.add_edge ((dir, E.Dir), node) G.Has;
  end;
  let env = { env with current = (env.file_readable, E.File); } in
  toplevels_entities_adjust_imports env ast;
  toplevels env ast

(* The order of toplevel declarations do not matter in Javascript.
 * It is a dynamic language without static checking; if in the body of
 * a function you use an entity declared further, nothing will complain
 * about it. 
 * So we need to first extract all those toplevel entities and 
 * create an import for them to not get lookup failures otherwise 
 * on those forward uses.
 *)
and toplevels_entities_adjust_imports env xs =
  xs |> List.iter (function
    | Import _ | Export _ | S _ -> ()
    | V v ->
      let str = s_of_n v.v_name in
      Hashtbl.replace env.imports str 
        (mk_qualified_name env.file_readable str);
  )
        
(* ---------------------------------------------------------------------- *)
(* Toplevels *)
(* ---------------------------------------------------------------------- *)
and toplevel env x =
  match x with
  | Import (name1, name2, file) ->
    if env.phase = Uses then begin
      let str1 = s_of_n name1 in
      let str2 = s_of_n name2 in
      let readable = readable_of_path env file in
      Hashtbl.replace env.imports str2 (mk_qualified_name readable str1);
    end
  | Export (name) -> 
     if env.phase = Defs then begin
       let exports =
         try 
           Hashtbl.find env.exports env.file_readable
         with Not_found -> []
       in
       let str = s_of_n name in
       Hashtbl.replace env.exports env.file_readable (str::exports)
     end
  | V {v_name; v_kind; v_init; v_resolved} ->
       name_expr env v_name v_kind v_init v_resolved
  | S (tok, st) ->
      let kind = E.TopStmts in
      let s = spf "__top__%d:%d" 
          (Parse_info.line_of_info tok) (Parse_info.col_of_info tok) in
      let name = s, tok in
      let env = add_node_and_edge_if_defs_mode env (name, kind) in
      if env.phase = Uses
      then stmt env st

and toplevels env xs = List.iter (toplevel env) xs

and name_expr env name v_kind e _v_resolved =
  let kind = kind_of_expr v_kind e in
  let env = add_node_and_edge_if_defs_mode env (name, kind) in
  if env.phase = Uses 
  then expr env e

(* ---------------------------------------------------------------------- *)
(* Statements *)
(* ---------------------------------------------------------------------- *)
and stmt env = function
 | VarDecl v ->
    let env = add_locals env [v] in
    expr env v.v_init
 | Block xs -> stmts env xs
 | ExprStmt e -> expr env e
 | If (e, st1, st2) ->
   expr env e;
   stmt env st1;
   stmt env st2
 | Do (st, e) ->
   stmt env st;
   expr env e;
 | While (e, st) ->
   expr env e;
   stmt env st
 | For (header, st) ->
   let env = for_header env header in
   stmt env st
 | Switch (e, xs) ->
   expr env e;
   cases env xs
 | Continue lopt ->
   Common.opt (label env) lopt
 | Break lopt ->
   Common.opt (label env) lopt
 | Return e ->
   expr env e
 | Label (l, st) ->
   label env l;
   stmt env st
 | Throw e ->
   expr env e
 | Try (st1, catchopt, finalopt) ->
   stmt env st1;
   catchopt |> Common.opt (fun (n, st) -> 
     let v = { v_name = n; v_kind = Let; v_init = Nop; 
               v_resolved = ref Local } in
     let env = add_locals env [v] in
     stmt env st
   );
   finalopt |> Common.opt (fun (st) -> stmt env st);

and for_header env = function
 | ForClassic (e1, e2, e3) ->
   let env =
     match e1 with
     | Left vars ->
       (* less: need fold_with_env? *)
       vars |> List.iter (fun v -> stmt env (VarDecl v));
       add_locals env vars
     | Right e ->
       expr env e;
       env
   in
   expr env e2;
   expr env e3;
   env
 | ForIn (e1, e2) | ForOf (e1, e2) ->
   let env =
     match e1 with
     | Left var ->
       (* less: need fold_with_env? *)
       [var] |> List.iter (fun v -> stmt env (VarDecl v));
       add_locals env [var]
     | Right e ->
       expr env e;
       env
   in
   expr env e2;
   env

(* less: could check def/use of lbls, but less important *)
and label _env _lbl =
  ()

and cases env xs = List.iter (case env) xs

and case env = function
 | Case (e, st) ->
   expr env e;
   stmt env st
 | Default st ->
   stmt env st
   
and stmts env xs =
  let rec aux env = function
    | [] -> ()
    | x::xs ->
        stmt env x;
        let env =
          match x with
          | VarDecl v -> add_locals env [v]
          | _ -> env
        in
        aux env xs
  in
  aux env xs

(* ---------------------------------------------------------------------- *)
(* Expessions *)
(* ---------------------------------------------------------------------- *)
and expr env e =
  match e with
  | Bool _ | Num _ | String _ | Regexp _ -> ()
  | Id (n, _scope) -> 
    if not (is_local env n)
    then
     (* the big one! *)
     add_use_edge_candidates env (n, E.Global)

  | IdSpecial _ -> ()
  | Nop -> ()
  | Assign (e1, e2) ->
    expr env e1;
    expr env e2

  | Obj o ->
     obj_ env o
  | Arr xs ->
     List.iter (expr env) xs
  | Class c ->
     class_ env c
  | ObjAccess (e, prop) ->
    (match e with
    | Id (n, _scope) when not (is_local env n) -> 
       add_use_edge_candidates env (n, E.Class) 
    | _ -> 
      expr env e
    );
    property_name env prop
  | ArrAccess (e1, e2) ->
    (match e1 with
    | Id (n, _scope) when not (is_local env n) -> 
       add_use_edge_candidates env (n, E.Class) 
    | _ -> 
      expr env e1
    );
    expr env e2

  | Fun (f, nopt) ->
    let env =
      match nopt with
      | None -> env
      | Some n -> 
        let v = { v_name = n; v_kind = Let; v_init = Nop; 
                  v_resolved = ref Local}in
        add_locals env [v]
    in
    fun_ env f
  | Apply (e, es) ->
    (match e with
    | Id (n, _scope) when not (is_local env n) ->
        add_use_edge_candidates env (n, E.Function) 
    | IdSpecial (special, _tok) ->
       (match special, es with
       | New, _ -> (* TODO *) ()
       | _ -> ()
       )
    | _ ->
      expr env e
    );
    List.iter (expr env) es

  | Conditional (e1, e2, e3) ->
    List.iter (expr env) [e1;e2;e3]


(* ---------------------------------------------------------------------- *)
(* Entities *)
(* ---------------------------------------------------------------------- *)

(* todo: create nodes if global var? *)
and obj_ env xs =
  List.iter (property env) xs

and class_ env c = 
  Common.opt (expr env) c.c_extends;
  List.iter (property env) c.c_body

and property env = function
  | Field (pname, _props, e) ->
     property_name env pname;
     expr env e
  | FieldSpread e ->
     expr env e

and property_name env = function
  | PN _n2 -> ()
  | PN_Computed e -> 
     expr env e

and fun_ env f = 
  (* less: need fold_with_env here? can not use previous param in p_default? *)
  parameters env f.f_params;
  let params = f.f_params |> List.map (fun p -> s_of_n p.p_name) in
  let env = { env with 
        locals = params @ env.locals; 
        (* new scope, but still inherits enclosing vars *)
        vars = Hashtbl.copy env.vars;
  } in
  stmt env f.f_body

and parameters env xs = List.iter (parameter env) xs
and parameter env p =
  Common.opt (expr env) p.p_default  

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let build ?(verbose=false) root files =
  let g = G.create () in
  G.create_initial_hierarchy g;

  let chan = open_out (Filename.concat root "pfff.log") in

  let env = { 
    g;
    phase = Defs;
    current = G.pb;
    file_readable = "__filled_later__";
    imports = Hashtbl.create 0;
    locals = [];
    vars = Hashtbl.create 0;
    exports = Hashtbl.create 101;
    dupes = Hashtbl.create 101;

    log = (fun s -> output_string chan (s ^ "\n"); flush chan;);
    pr2_and_log = (fun s ->
      if verbose then pr2 s;
      output_string chan (s ^ "\n"); flush chan;
    );
  } in

  (* step1: creating the nodes and 'Has' edges, the defs *)
  env.pr2_and_log "\nstep1: extract defs";
  (Stdlib_js.path_stdlib::files) |> Console.progress ~show:verbose (fun k -> 
    List.iter (fun file ->
      k();
      let ast = parse file in
      let file_readable =
         if file = Stdlib_js.path_stdlib
         then "Stdlib.js"
         else Common.readable ~root file 
      in
      extract_defs_uses { env with 
        phase = Defs; file_readable; imports = Hashtbl.create 13;
      } ast
    ));
  let default_import = 
    let ast = parse Stdlib_js.path_stdlib in
    let env = { env with phase = Uses; file_readable = "Stdlib.js"; 
                locals = []; imports = Hashtbl.create 13; } in
    toplevels_entities_adjust_imports env ast;
    env.imports
  in

  (* step2: creating the 'Use' edges *)
  env.pr2_and_log "\nstep2: extract uses";
  files |> Console.progress ~show:verbose (fun k -> 
    List.iter (fun file ->
      k();
      let ast = parse file in
      let file_readable = Common.readable ~root file in
      extract_defs_uses { env with 
        phase = Uses; file_readable; 
        locals = []; imports = Hashtbl.copy default_import; 
      } ast

    ));

  env.pr2_and_log "\nstep3: adjusting";
  G.remove_empty_nodes g [G.not_found; G.dupe; G.pb];

  (* less: lookup failures summary *)

  (* finally return the graph *)
  g
