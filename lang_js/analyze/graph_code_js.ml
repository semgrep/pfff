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

  (* covers also the parameters; the type_ is really only for datalog_c *)
  locals: (string) list ref;

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

(*****************************************************************************)
(* Naming helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Other helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Add Node *)
(*****************************************************************************)
let add_node_and_edge_if_defs_mode env (name, kind) =
  let str = Ast.str_of_name name in
  let str' =
    match env.current with
    | (s, _) -> s ^ "::" ^ str
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
        let typ = None in
        let nodeinfo = { Graph_code.
          pos; typ;
          props = [];
        } in
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
  let s = Ast.str_of_name name in
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

let extract_defs_uses env ast =
  ()

(* ---------------------------------------------------------------------- *)
(* Toplevels *)
(* ---------------------------------------------------------------------- *)

(* ---------------------------------------------------------------------- *)
(* Statements *)
(* ---------------------------------------------------------------------- *)

(* ---------------------------------------------------------------------- *)
(* Expessions *)
(* ---------------------------------------------------------------------- *)


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
    locals = ref [];
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
        phase = Defs; file_readable; 
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
      } ast

    ));
  env.pr2_and_log "\nstep3: adjusting";
  G.remove_empty_nodes g [G.not_found; G.dupe; G.pb];

  (* lookup failures summary *)

  (* finally return the graph *)
  g
