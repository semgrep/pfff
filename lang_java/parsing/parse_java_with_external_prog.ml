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
  (* let lines = "node ../parsing/tree-sitter-parser.js " ^ _file ^ " > results.json"
  Sys.command lines *)
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

(* time to write all this boilerplate: 2h ... and it just
 * handles the JSON for tests/java/json/babelfish-toyex1-pp.json *)
let program_of_babelfish_json file json = 
  let rec program = function
   | J.Object [
     "@pos", _;
     "@role", _;
     "@type", _;
     "imports", imp;
     "package", pack;
     "types", typs;
      ] -> { package = package pack;
             imports = imports imp;
             decls = decls typs;
           }
   | x -> error "program" x

  and package = function
    | J.Object [
        "@pos", _; "@role", _; "@token", _; "@type", _;
        "annotations", _; "javadoc", _;
        "name", n;
        ] -> Some (qualified_ident n)
    | x -> todo "package" x

  and qualified_ident = function
    | J.Object [
        "@pos", pos; "@type", _;
        "Name", J.String str;
      ] -> [wrap pos str]
   | x -> todo "qualified_ident" x

  and ident = function
    | J.Object [
        "@pos", pos; "@type", _;
        "Name", J.String str;
      ] -> wrap pos str
   | x -> todo "ident" x

  and wrap pos x =
    let tok = parse_info pos in
    x, tok

  and parse_info = function
    | J.Object [
        "@type", _;
        "end", _;
        "start", J.Object [
          "@type", _;
          "col", J.Int col;
          "line", J.Int line;
          "offset", J.Int offset;
        ]
      ] -> { PI.token = 
              PI.OriginTok {
                 PI.charpos = offset;
                 PI.str     = "TODO";
                 PI.line = line;
                 PI.column = col;
                 PI.file = file;
              };
            PI.transfo = PI.NoTransfo; }
   | x -> error "parse_info" x

  and imports = function
    | J.String "None" -> []
    | x -> todo "imports" x

  and decls xs = list "decl" decl xs
  and decl = function
   | J.Object [
        "@pos", _; "@role", _; "@type", _;
        "bodyDeclarations", body_decls;
        "interface", J.String "false"; 
        "javadoc", _; 
        "modifiers", mods;
        "name", n;
        "superInterfaceTypes", superinterface;
        "superclassType", superclass;
        "typeParameters", tparams;
      ] -> 
        Class {
          cl_name = ident n;
          cl_kind = ClassRegular;
          cl_tparams = list "type_parameter" type_parameter tparams;
          cl_mods = list "modifier" modifier mods;
          cl_extends = option "typ" typ superclass;
          cl_impls = list "ref_type" ref_type superinterface; 
          cl_body = list "body_decl" body_decl body_decls;
        }
   | x -> todo "decl" x

  and body_decl = function
   | J.Object [
        "@pos", _; 
        "@type", _;
        "Nodes", J.Array [
          J.String "None"; (* ?? *)
          J.String "None"; (* ?? *)
          met;
          J.Object [
            "constructor", _ctorIGNORED;
            "thrownExceptionTypes", exns;
            "typeParameters", _tparamsTODO_ADD_IN_AST_JAVA;
          ]
        ]]
      -> 
        let throws = list "qualified_ident" qualified_ident exns in
        Method (method_decl throws met)
   | x -> todo "body_decl" x

  and method_decl throws = function
   | J.Object [
        "@type", _;
        "Name", n;
        "Node", J.Object [
          "@type", _;
          "Body", body;
          "Type", J.Object [
            "@type", _;
            "Arguments", params;
            "Returns", ret_type;
          ]
         ]
        ] ->
        { m_var = { name = ident n; mods = []; 
                    type_ = option "typ" typ ret_type };
          m_formals = list "parameter" parameter params;
          m_throws = throws;
          m_body = body_statement body;
          }
   | x -> todo "method_decl" x

  and body_statement = function
   | J.Object [
        "@pos", _; "@type", _;
        "Statements", sts;
        ] -> Block (list "stmt" stmt sts)
   | x -> todo "body_statement" x
  and stmt = function
   | J.Object [
        "@pos", _; "@role", _;
        "@type", J.String "java:ReturnStatement";
        "expression", e;
     ] ->
        Return (Some (expr e))
   | x -> todo "stmt" x

  and expr = function
   | J.Object [
        "@pos", pos;
        "@role", _;
        "@token", J.String s;
        "@type", J.String "java:NumberLiteral";
        ] ->
        Literal (Int (wrap pos s))
   | x -> todo "expr" x

  and parameter = function
   | x -> todo "parameter" x
  and type_parameter = function
   | x -> todo "type_parameter" x

  and modifier = function
   | x -> todo "modifier" x

  and typ = function
   | x -> todo "typ" x
  and ref_type x = typ x


  in
  program json



(*****************************************************************************)
(* JSON boilerplate for tree-sitter this time *)
(*****************************************************************************)

(* TODO: I do not currently have the position information in the 
 * tree-sitter JSON returned by sharon.
 *)
let wrap str = 
  let tok = Parse_info.fake_info str in
  str, tok

(* time to write this boilerplate: 1h30
 * the structure of the JSON is deriving from the grammar spec of Java 
 * used by tree-sitter and defined here:
 *  https://github.com/tree-sitter/tree-sitter-java/blob/master/grammar.js
 *
*)

let program_of_tree_sitter_json _file json = 
  let rec program = function
  | J.Object [
    "type", J.String "program";
    "startPosition", _;
    "endPosition", _;
    "body", J.Object xs;
  ] -> let package, rest = 
        match xs with
          | (_, J.Object [
            "type", J.String "package_declaration";
            "startPosition", _;
            "endPosition", _;
            "value", J.String pack;
            "body", _;
          ])::rest -> Some (qualified_ident pack), rest
          | _ -> None, xs
       in 
       let import, rest = 
         match xs with
          | (_, J.Object [
            "type", J.String "import_declaration";
            "startPosition", _;
            "endPosition", _;
            "value", J.String imp;
            "body", _;
          ])::rest -> Some (qualified_ident imp), rest
          | _ -> None, xs
       in 
       { package = package; imports = import; 
        decls = decls rest }
  | x -> error "program" x
 
  (* from grammar.js: 
    package_declaration: $ => seq(
      repeat($._annotation),
      'package',
      $._ambiguous_name,
      $._semicolon
    ),
   *)

  and qualified_ident = function
    | J.Object [
      "type", J.String "identifier";
      "startPosition", _;
      "endPosition", _;
      "value", J.String str;
      "body", _;
    ] -> [wrap star]
    | x -> todo "qualified_ident" x
  and ident = function
    | J.Object [
      "type", J.String "identifier";
      "startPosition", _;
      "endPosition", _;
      "value", J.String str;
      "body", _;
    ] -> wrap str
    | x -> todo "ident" x

  and decls xs = List.map (fun (_idx, a) -> decl a) xs

  (* from grammar.js: 
    class_declaration: $ => seq(
      repeat($.modifier),
      'class',
      $.identifier,
      optional($.type_parameters),
      optional($.superclass),
      optional($.super_interfaces),
      $.class_body
    ),
   *)
  and decl = function
    | J.Object [
        "type", J.String "class_declaration";
        "startPosition", _;
        "endPosition", _;
        "body", J.Object cx;
      ] ->
        Class { cl_name = ident n;
          cl_name = None;
          cl_kind = ClassRegular;
          cl_body = class_body cx;
          cl_mods = []; 
          cl_extends = None; 
          cl_impls = []; 
          cl_tparams = [];
        }
    | x -> todo "decl" x

  and class_body = function
   | J.Object [
     "type", J.String "class_body";
      "startPosition", _;
      "endPosition", _;
      "body", J.Object met;
      ] -> [Method (method_decl met)]
   | x -> todo "class_body" x

  and method_decl = function
   | J.Object [
     "type", J.String "method_declaration";
     "startPosition", _;
      "endPosition", _;
     "body", J.Object met_id;
     ] ->
        { m_var = { name = ident n; mods = []; type_ = Some (typ ret_typ); };
          m_formals = parameters params;
          m_throws = [];
          m_body = block st;
        }
   | x -> todo "method_decl" x

  and typ = function
   | J.Object [
     "type", "void_type";
      _
    ] -> TBasic (wrap "void")
   | x -> todo "typ" x

  and parameters = function
   | J.Object [
     "type", J.String "formal_parameters";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "parameters" x

   and parameter = function
   | J.Object [
     "type", J.String "formal_parameter";
     "startPosition", _;
      "endPosition", _;
     "value", param
    ] -> param
   | x -> todo "parameter" x

  and block = function
   | J.Object [
     "type", J.String "block";
     "startPosition", _;
    "endPosition", _;
     "body", J.Object st
     ] -> Block [stmt st]
   | x -> todo "block" x

  and stmt = function
   | J.Object [
     "type", J.String "return_statement";
     "startPosition", _;
    "endPosition", _;
     "body", J.Object e
     ] -> Return (Some (expr e))
   | x -> todo "stmt" x

  and expr = function
   | J.Object [
     "type", J.String "decimal_integer_literal";
     "startPosition", _;
    "endPosition", _;
     "body", J.Object str
     ] -> Literal (Int (wrap str))
   | x -> todo "expr" x

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
