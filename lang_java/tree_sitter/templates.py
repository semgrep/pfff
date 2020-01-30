ast_header = '''
(* 
 * Yoann Padioleau, Sharon Lin
 * 2020 initial draft
 *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * An AST for Java.
 *
 *)
 (*****************************************************************************)
(* The Tree-sitter AST java related types *)
(*****************************************************************************)
(* ------------------------------------------------------------------------- *)
(* Token/info *)
(* ------------------------------------------------------------------------- *)

type 'a wrap  = 'a * string
'''

json_header = '''
(* Yoann Padioleau, Sharon Lin
 *
 * Copyright (C) 2020 r2c
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

open Ast_java
module J = Json_type
module PI = Parse_info

(*****************************************************************************)
(* Call external program *)
(*****************************************************************************)
let json_of_filename_with_external_prog _file =
  (* should call tree-sitter or babelfish on _file, generate
   * temporary JSON output, and read this JSON output
   * (or use a pipe to the external program to avoid using an
   *  intermediate temporary file)
   *)
  let lines = "node ../tree-sitter/tree-sitter-parser.js " ^ _file ^ " > results.json"
  Sys.command lines
  raise Todo

(*****************************************************************************)
(* JSON boilerplate (we should find a way to generate this code) *)
(*****************************************************************************)

let error str json =
  pr2 (spf "bad json: was expecting a %s (got %s)" str 
        (Json_io.string_of_json json));
  failwith "BAD JSON"

let todo str json = 
  pr2 (spf "TODO: in %s need to handle this JSON: %s" str 
    (Json_io.string_of_json json));
  failwith "TODO"

(* this can not be in the recursive 'let rec ... and ...' below because
 * of weird OCaml restrictions too long to explain
 *)
let list str f = function
    | J.Array xs -> List.map f xs
    | J.String "None" -> []
    | x -> error (spf "list[%s]" str) x
let option str _f = function
    | J.String "None" -> None
    | x -> todo (spf "option[%s]" str) x

let wrap str = 
  let tok = Parse_info.fake_info str in
  str, tok

'''

json_footer = '''
(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)
let parse file = 
  let json = json_of_filename_with_external_prog file in
  (* to decide wether this or program_of_babelfish_json *)
  let program = program_of_tree_sitter_json file json in
  program
'''