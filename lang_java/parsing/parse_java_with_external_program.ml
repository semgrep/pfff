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
module JI = Json_io

(* module PI = Parse_info *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Parsing Java using an external parser (e.g., tree-sitter or babelfish).
 *
 * Right now, the conversion from JSON to the AST defined in ast_java.ml is
 * done manually, which is very tedious
 * given the size of the Java language and the number of constructs to handle.
 *
 * alternatives to hand-coded JSON reader:
 *  - for tree-sitter, process
 *    https://github.com/tree-sitter/tree-sitter-java/blob/master/grammar.js
 *    and try autogenerate the OCaml code that will read the JSON deriving
 *    from this grammar.
 *  - use atdgen https://github.com/ahrefs/atd
 *    (doc at https://atd.readthedocs.io/en/latest ) to
 *    define the JSON structure and let atdgen auto generate the JSON
 *    reader code.
 *  - use jsonpat: https://github.com/pikatchu/jsonpat to infer the
 *    JSON structure and then auto generate the JSON reader code?
 *  - autogenerate the code from babelfish and treesitter themselves given
 *    they have more knowledge about the structure of what they output.
 *)

(*****************************************************************************)
(* Call external program *)
(*****************************************************************************)
let json_of_filename_with_external_prog _file =
  (* should call tree-sitter or babelfish on _file, generate
   * temporary JSON output, and read this JSON output
   * (or use a pipe to the external program to avoid using an
   *  intermediate temporary file)
   *) 
  let file = "results.json" in 
  let line = "node lang_java/tree_sitter/tree-sitter-parser.js " ^ _file ^ " > " ^ file in
  let _ = Sys.command line in
  let json = JI.load_json file in
  json

(*****************************************************************************)
(* JSON boilerplate (we should find a way to generate this code) *)
(*****************************************************************************)

let error str json =
  pr2 (spf "bad json: was expecting a %s (got %s)" str 
        (Json_io.string_of_json json));
  failwith "BAD JSON"

(* let todo str json = 
  pr2 (spf "TODO: in %s need to handle this JSON: %s" str 
    (Json_io.string_of_json json));
  failwith "TODO" *)

(* this can not be in the recursive 'let rec ... and ...' below because
 * of weird OCaml restrictions too long to explain
 *)
let list str f = function
    | J.Array xs -> List.map f xs
    | J.String "None" -> []
    | x -> error (spf "list[%s]" str) x
(* let option str _f = function
    | J.String "None" -> None
    | x -> todo (spf "option[%s]" str) x *)


(*****************************************************************************)
(* JSON boilerplate for tree-sitter this time *)
(*****************************************************************************)

(* TODO: I do not currently have the position information in the 
 * tree-sitter JSON returned by sharon.
 *)
 let wrap str = 
  let tok = Parse_info.fake_info str in
  str, tok

(*let tokenize t_start t_end = 
  raise Todo *)

(*****************************************************************************)
(* JSON boilerplate for new tree-sitter this time *)
(*****************************************************************************)

let program_of_tree_sitter_json _file json = 
  let rec program = function
  | J.Object [
    "type", J.String "program";
    "startPosition", _;
    "endPosition", _;
    "children", xs;
  ] -> { 
    package = None;
    imports = [];
    decls = decls xs;
  }
  | x -> error "program" x

  and decls xs = list "decl" decl xs

  and decl = function
   | J.Object [
     "type", J.String "class_declaration";
     "startPosition", _;
      "endPosition", _;
     "children", J.Array xs;
    ] -> let identity = 
      match xs with 
        | _::J.Object ["type", J.String "identifier"; "startPosition", _; "endPosition", _; "children", _;]::_ ->  wrap "void" 
        | _ -> wrap "void"
      in 
      let bodies = 
      match xs with
        | _::J.Object ["type", J.String "class_body"; "startPosition", _; "endPosition", _; "children", b;]::_ -> list "decl" method_decl b
        | _ -> []
      in
        Class { 
          cl_name = identity;
          cl_kind = ClassRegular;
          cl_body = bodies;
          cl_mods = []; cl_extends = None; cl_impls = []; cl_tparams = [];
          } 
   | x -> error "decl" x

  and method_decl = function
  | J.Object [
     "type", J.String "method_declaration";
     "startPosition", _;
      "endPosition", _;
     "children", _;
    ] ->  Method {
      m_var = { name = wrap "str"; mods = []; type_ = None; };
      m_formals = [];
      m_throws = [];
      m_body = Empty;
    }
   | x -> error "method_decl" x

  (* and typ = function
  | J.Object [
     "type", J.String "void_type";
     "startPosition", _;
      "endPosition", _;
     "children", _;
    ] ->  TBasic (wrap "void")
   | x -> error "typ" x

  and block = function
  | J.Object [
     "type", J.String "block";
     "startPosition", _;
      "endPosition", _;
     "children", xs;
    ] ->  Return (Some (expr xs))
   | x -> error "block" x

  and expr = function
  | J.Object [
     "type", J.String "decimal_integer_literal";
     "startPosition", _;
      "endPosition", _;
     "children", _;
    ] ->  Literal (Int (wrap "str"))
   | x -> error "expr" x *)


  (* and package_declaration = function
   | J.Object [
     "type", J.String "package_declaration";
     "startPosition", _;
      "endPosition", _;
     "children", xs
    ] -> _ambiguous_name xs
   | x -> error "package_declaration" x

  and _ambiguous_name = function
   | J.Object [
     "type", J.String "_ambiguous_name";
     "startPosition", _;
      "endPosition", _;
     "children", xs
    ] -> identifier xs
   | x -> error "_ambiguous_name" x *)

  (* and identifier = function
   | J.Object [
     "type", J.String "identifier";
     "startPosition", t_start;
      "endPosition", t_end;
     "children", _;
    ] -> "void"
   | x -> error "identifier" x *)

  (* and import_declaration = function
   | J.Object [
     "type", J.String "import_declaration";
     "startPosition", _;
      "endPosition", _;
     "children", xs
    ] -> list (identifier xs)
   | x -> error "import_declaration" x

  and asterisk = function
   | J.Object [
     "type", J.String "asterisk";
     "startPosition", t_start;
      "endPosition", t_end;
     "children", _;
    ] -> identifier "void"
   | x -> error "asterisk" x *)

  (* and modifiers = function
  | J.Object [
     "type", _;
     "startPosition", _;
      "endPosition", _;
     "children", xs
    ] -> identifier "void"
  | x -> todo "modifiers" x

  and type_parameters = function
   | J.Object [
     "type", J.String "type_parameters";
     "startPosition", _;
      "endPosition", _;
     "children", xs
    ] -> list (type_parameter xs)
   | x -> error "type_parameters" x

  and type_parameter = function
   | J.Object [
     "type", J.String "type_parameter";
     "startPosition", _;
      "endPosition", _;
     "children", xs
    ] -> list (_annotation xs) * identifier xs * option (type_bound xs)
   | x -> error "type_parameter" x

  and superclass = function
  | J.Object [
     "type", _;
     "startPosition", _;
      "endPosition", _;
     "children", xs
    ] -> identifier "void"
  | x -> todo "superclass" x

  and class_body = function
  | J.Object [
     "type", _;
     "startPosition", _;
      "endPosition", _;
     "children", xs
    ] -> identifier "void"
  | x -> todo "super_interfaces" x

  and super_interfaces = function
  | J.Object [
     "type", _;
     "startPosition", _;
      "endPosition", _;
     "children", xs
    ] -> identifier "void"
  | x -> todo "super_interfaces" x *)

  in
  program json


(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)
let parse file = 
  let json = json_of_filename_with_external_prog file in
  (* to decide wether this or program_of_babelfish_json *)
  let program = program_of_tree_sitter_json file json in
  program