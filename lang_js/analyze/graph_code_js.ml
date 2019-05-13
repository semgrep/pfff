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
 *                      -> Class
 *                      -> Var
 *                          -> Obj
 *                            -> Field
 *       -> Dir -> SubDir -> ...
 * todo: 
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

  imports: (string, (string (* orig name *) * Common.filename)) Hashtbl.t;
  (* covers also the parameters; I handle block scope by not using
   * a ref of mutable here! Just build a new list and passed it down.
   *)
  locals: string list;

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

(*****************************************************************************)
(* File resolution *)
(*****************************************************************************)
(* TODO: resolve and normalize path *)
let resolve_path _env file =
  file

(*****************************************************************************)
(* Name resolution *)
(*****************************************************************************)


(*****************************************************************************)
(* Other helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Add Node *)
(*****************************************************************************)
let add_node_and_edge_if_defs_mode env (name, kind) =
  let str = s_of_n name in
  let str' =
    match env.current with
    | (s, E.File) -> 
       let (d, b, _e) = Common2.dbe_of_filename s in
       let s = Common2.filename_of_dbe (d,b,"") in
       s ^ "." ^ str
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
        let pos = Parse_info.token_location_of_info (snd name) in
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
let _add_use_edge env (name, kind) =
  let s = s_of_n name in
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
  toplevels env ast

(* ---------------------------------------------------------------------- *)
(* Toplevels *)
(* ---------------------------------------------------------------------- *)
and toplevel env x =
  match x with
  | Import (name1, name2, file) ->
    if env.phase = Uses then begin
      let str1 = s_of_n name1 in
      let str2 = s_of_n name2 in
      let readable = resolve_path env (Ast.unwrap file) in
      Hashtbl.add env.imports str2 (str1, readable);
    end
  | Export (name, expr) -> 
     if env.phase = Defs then begin
       let exports =
         try 
           Hashtbl.find env.exports env.file_readable
         with Not_found -> []
       in
       let str = s_of_n name in
       Hashtbl.replace env.exports env.file_readable (str::exports)
     end;
     name_expr env name Const expr
  | S st ->
    (match st with
    | VarDecl {v_name; v_kind; v_init} ->
       name_expr env v_name v_kind v_init
    | _ ->
      if env.phase = Uses
      then stmt env st
    )

and toplevels env xs = List.iter (toplevel env) xs

and name_expr env name v_kind e =
  let kind =
    match e with
    | Fun _ -> E.Function
    | Class _ -> E.Class
    | Obj _ -> E.Class
    | _ -> if v_kind = Const then E.Constant else E.Global
  in
  let env = add_node_and_edge_if_defs_mode env (name, kind) in
  if env.phase = Uses 
  then expr env e
(* ---------------------------------------------------------------------- *)
(* Statements *)
(* ---------------------------------------------------------------------- *)
and stmt env = function
 | VarDecl v ->
    expr {env with locals = s_of_n v.v_name::env.locals } v.v_init
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
   catchopt |> Common.opt (fun (_, st) -> stmt env st);
   finalopt |> Common.opt (fun (st) -> stmt env st);

and for_header env = function
 | ForClassic (e1, e2, e3) ->
   let env =
     match e1 with
     | Left vars ->
       (* less: need fold_with_env? *)
       vars |> List.iter (fun v -> stmt env (VarDecl v));
       { env with locals = (vars |> List.map (fun v -> s_of_n v.v_name))
                    @env.locals}
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
       { env with locals = ([var] |> List.map (fun v -> s_of_n v.v_name))
                    @env.locals}
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
          | VarDecl v ->
              { env with locals = s_of_n v.v_name :: env.locals }
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
  | Id n -> 
    if List.mem (s_of_n n) env.locals
    then ()
    else 
     (* the big one! *)
     raise Todo
  | IdSpecial _ -> ()
  | Nop -> ()
  | Assign (e1, e2) ->
    expr env e1;
    expr env e2

  | Obj o ->
     obj_ env o
  | Class c ->
     class_ env c
  | ObjAccess (e, prop) ->
    (match e with
    | Id _n -> 
        (* todo: check if local *)
        raise Todo
    | _ -> 
      expr env e
    );
    property_name env prop

  | Fun (f, _nopt) ->
    fun_ env f
  | Apply (e, es) ->
    (match e with
    | Id n when not (List.mem (s_of_n n) env.locals) ->
        (* todo: check if local *) 
        raise Todo
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
  let env = { env with locals = params @ env.locals} in
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
    imports = Hashtbl.create 13;
    locals = [];
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
  files |> Console.progress ~show:verbose (fun k -> 
    List.iter (fun file ->
      k();
      let ast = parse file in
      let file_readable = Common.readable ~root file in
      extract_defs_uses { env with 
        phase = Defs; file_readable; imports = Hashtbl.create 13;
      } ast
    ));

  (* step2: creating the 'Use' edges *)
  env.pr2_and_log "\nstep2: extract uses";
  files |> Console.progress ~show:verbose (fun k -> 
    List.iter (fun file ->
      k();
      let ast = parse file in
      let file_readable = Common.readable ~root file in
      extract_defs_uses { env with 
        phase = Uses; file_readable; 
        locals = []; imports = Hashtbl.create 13; 
      } ast

    ));
  env.pr2_and_log "\nstep3: adjusting";
  G.remove_empty_nodes g [G.not_found; G.dupe; G.pb];

  (* lookup failures summary *)

  (* finally return the graph *)
  g
