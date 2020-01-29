
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

open Tree_sitter_ast_java
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
  let lines = "node ../tree-sitter/tree-sitter-parser.js " ^ _file ^ " > results.json" in
  Sys.command lines

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

let wrap t_start t_end = 
  raise Todo
  (* let tok = Parse_info.fake_info str in
  str, tok *)


let program_of_tree_sitter_json _file json = 
  let rec program = function
  | J.Object [
    "type", J.String "program";
    "startPosition", _;
    "endPosition", _;
    "children", J.Object xs;
  ] -> { 
    package = package_declaration xs;
    imports = import_declaration xs list;
    decls = class_declaration xs list option;
  }
  | x -> error "program" x

  and t_literal = function
   | J.Object [
     "type", J.String "_literal";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs;
    ] -> Some (interm0 xs) 
   | x -> error "_literal" x

   and interm0 = function
    | J.Object [
      "type", J.String "decimal_integer_literal";
      "startPosition", t_start;
      "endPosition", t_end;
      "children", _;
    ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "hex_integer_literal";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "octal_integer_literal";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "binary_integer_literal";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "decimal_floating_point_literal";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "hex_floating_point_literal";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "true";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "false";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "character_literal";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "string_literal";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "null_literal";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]

  and decimal_integer_literal = function
   | J.Object [
     "type", J.String "decimal_integer_literal";
     "startPosition", t_start;
      "endPosition", t_end;
     "children", _;
    ] -> [wrap t_start t_end]
   | x -> error "decimal_integer_literal" x

  and hex_integer_literal = function
   | J.Object [
     "type", J.String "hex_integer_literal";
     "startPosition", t_start;
      "endPosition", t_end;
     "children", _;
    ] -> [wrap t_start t_end]
   | x -> error "hex_integer_literal" x

  and octal_integer_literal = function
   | J.Object [
     "type", J.String "octal_integer_literal";
     "startPosition", t_start;
      "endPosition", t_end;
     "children", _;
    ] -> [wrap t_start t_end]
   | x -> error "octal_integer_literal" x

  and binary_integer_literal = function
   | J.Object [
     "type", J.String "binary_integer_literal";
     "startPosition", t_start;
      "endPosition", t_end;
     "children", _;
    ] -> [wrap t_start t_end]
   | x -> error "binary_integer_literal" x

  and decimal_floating_point_literal = function
   | J.Object [
     "type", J.String "decimal_floating_point_literal";
     "startPosition", t_start;
      "endPosition", t_end;
     "children", _;
    ] -> [wrap t_start t_end]
   | x -> error "decimal_floating_point_literal" x

  and hex_floating_point_literal = function
   | J.Object [
     "type", J.String "hex_floating_point_literal";
     "startPosition", t_start;
      "endPosition", t_end;
     "children", _;
    ] -> [wrap t_start t_end]
   | x -> error "hex_floating_point_literal" x

  and t_true = function
   | J.Object [
     "type", J.String "true";
     "startPosition", t_start;
      "endPosition", t_end;
     "children", _;
    ] -> [wrap t_start t_end]
   | x -> error "true" x

  and t_false = function
   | J.Object [
     "type", J.String "false";
     "startPosition", t_start;
      "endPosition", t_end;
     "children", _;
    ] -> [wrap t_start t_end]
   | x -> error "false" x

  and character_literal = function
   | J.Object [
     "type", J.String "character_literal";
     "startPosition", t_start;
      "endPosition", t_end;
     "children", _;
    ] -> [wrap t_start t_end]
   | x -> error "character_literal" x

  and string_literal = function
   | J.Object [
     "type", J.String "string_literal";
     "startPosition", t_start;
      "endPosition", t_end;
     "children", _;
    ] -> [wrap t_start t_end]
   | x -> error "string_literal" x

  and null_literal = function
   | J.Object [
     "type", J.String "null_literal";
     "startPosition", t_start;
      "endPosition", t_end;
     "children", _;
    ] -> [wrap t_start t_end]
   | x -> error "null_literal" x

  and t_expression = function
   | J.Object [
     "type", J.String "_expression";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs;
    ] -> Some (assignment_expression xs) 
    | J.Object [
     "type", J.String "_expression";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs;
    ] -> Some (binary_expression xs) 
    | J.Object [
     "type", J.String "_expression";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs;
    ] -> Some (instanceof_expression xs) 
    | J.Object [
     "type", J.String "_expression";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs;
    ] -> Some (lambda_expression xs) 
    | J.Object [
     "type", J.String "_expression";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs;
    ] -> Some (ternary_expression xs) 
    | J.Object [
     "type", J.String "_expression";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs;
    ] -> Some (update_expression xs) 
    | J.Object [
     "type", J.String "_expression";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs;
    ] -> Some (_ambiguous_name xs) 
    | J.Object [
     "type", J.String "_expression";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs;
    ] -> Some (_primary xs) 
    | J.Object [
     "type", J.String "_expression";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs;
    ] -> Some (unary_expression xs) 
     | J.Object [
     "type", J.String "_expression";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs;
    ] -> Some (cast_expression xs)  
   | x -> error "_expression" x
 
  and cast_expression = function
   | J.Object [
     "type", J.String "cast_expression";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs;
    ] -> {
      t_type = t_type xs list;
      value4 = t_expression xs; 
    }
   | x -> error "cast_expression" x 

  and assignment_expression = function
   | J.Object [
     "type", J.String "assignment_expression";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs;
    ] -> {
      left = interm94 xs;
      operator = operator xs;
      right = t_expression xs;
    }
   | x -> error "assignment_expression" x

  and operator = function
  | J.Object [
     "type", J.String _;
     "startPosition", t_start;
      "endPosition", t_end;
     "children", _;
    ] -> [wrap t_start t_end]

  and interm94 = function
  | J.Object [
     "type", J.String "_ambiguous_name";
     "startPosition", t_start;
      "endPosition", t_end;
     "children", _;
    ] -> [wrap t_start t_end]
  | J.Object [
     "type", J.String "field_access";
     "startPosition", t_start;
      "endPosition", t_end;
     "children", _;
    ] -> [wrap t_start t_end]
  | J.Object [
     "type", J.String "array_access";
     "startPosition", t_start;
      "endPosition", t_end;
     "children", _;
    ] -> [wrap t_start t_end]

  and binary_expression = function
   | J.Object [
     "type", J.String "binary_expression";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      left: t_expression xs;
      right: t_expression xs;
    }
   | x -> error "binary_expression" x

  and instanceof_expression = function
   | J.Object [
     "type", J.String "instanceof_expression";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      left: t_expression xs;  
      right: t_type xs;
    }
   | x -> error "instanceof_expression" x

  and lambda_expression = function
   | J.Object [
     "type", J.String "lambda_expression";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      parameters: identifier xs;
      body: t_expression xs;
    }
    | J.Object [
     "type", J.String "lambda_expression";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      parameters: identifier xs;
      body: block xs;
    }
    | J.Object [
     "type", J.String "lambda_expression";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      parameters: formal_parameters xs;
      body: t_expression xs;
    }
    | J.Object [
     "type", J.String "lambda_expression";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      parameters: formal_parameters xs;
      body: block xs;
    }
    | J.Object [
     "type", J.String "lambda_expression";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      parameters: inferred_parameters xs;
      body: t_expression xs;
    }
    | J.Object [
     "type", J.String "lambda_expression";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      parameters: inferred_parameters xs;
      body: block xs;
    }
   | x -> error "lambda_expression" x

  and parameters = function
  | J.Object [
     "type", J.String "identifier";
     "startPosition", t_start;
      "endPosition", t_end;
     "children", _
    ] -> [wrap t_start t_end]
  | J.Object [
     "type", J.String "formal_parameters";
     "startPosition", t_start;
      "endPosition", t_end;
     "children", _
    ] -> [wrap t_start t_end]
  | J.Object [
     "type", J.String "inferred_parameters";
     "startPosition", t_start;
      "endPosition", t_end;
     "children", _
    ] -> [wrap t_start t_end]

  and body = function
  | J.Object [
     "type", J.String "_expression";
     "startPosition", t_start;
      "endPosition", t_end;
     "children", _
    ] -> [wrap t_start t_end]
  | J.Object [
     "type", J.String "block";
     "startPosition", t_start;
      "endPosition", t_end;
     "children", _
    ] -> [wrap t_start t_end]

  and inferred_parameters = function
   | J.Object [
     "type", J.String "inferred_parameters";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      identifier: identifier xs list;  
    }
   | x -> error "inferred_parameters" x

  and ternary_expression = function
   | J.Object [
     "type", J.String "ternary_expression";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      condition: t_expression xs;
      consequence: t_expression xs;
      alternative: t_expression xs;  
    }
   | x -> error "ternary_expression" x

  and unary_expression = function
   | J.Object [
     "type", J.String "unary_expression";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      field: t_expression xs;
    }
   | x -> error "unary_expression" x

  and update_expression = function
   | J.Object [
     "type", J.String "update_expression";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      expression: t_expression xs;
    }
   | x -> error "update_expression" x

  and _primary = function
   | J.Object [
     "type", J.String "_primary";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> Some (interm4 xs) 
   | x -> error "_primary" x

   and interm4 = function
    | J.Object [
        "type", J.String "_literal";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "class_literal";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "this";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "parenthesized_expression";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "object_creation_expression";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "field_access";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "array_access";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "method_invocation";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "method_reference";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "array_creation_expression";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]

  and array_creation_expression = function
   | J.Object [
     "type", J.String "array_creation_expression";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      t_type: simple_type xs;
      dimensions: dimensions_expr xs list;
      dim: dimensions xs option;
    }
    | J.Object [
     "type", J.String "array_creation_expression";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      t_type: simple_type xs;
      dimensions: dimensions xs;
      value: array_initializer xs;
    }
   | x -> error "array_creation_expression" x


  and dimensions_expr = function
   | J.Object [
     "type", J.String "dimensions_expr";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      annotation: (annotation xs) list;
      expression: expression xs;
    }
   | x -> error "dimensions_expr" x

  and annotation = function
  | J.Object [
     "type", J.String "_annotation";
     "startPosition", t_start;
      "endPosition", t_end;
     "children", _
    ] -> [wrap t_start t_end]

  and parenthesized_expression = function
   | J.Object [
     "type", J.String "parenthesized_expression";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      expression: expression xs;
    }
   | x -> error "parenthesized_expression" x

  and class_literal = function
   | J.Object [
     "type", J.String "class_literal";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> Some (interm6 xs) 
   | x -> error "class_literal" x

   and interm6 = function
    | J.Object [
        "type", J.String "_ambiguous_name";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "_numeric_type";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "boolean_type";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
      | x -> error "boolean_type" x
    | J.Object [
        "type", J.String "void_type";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
        
  and object_creation_expression = function
   | J.Object [
     "type", J.String "object_creation_expression";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> Some (interm7 xs) 
   | x -> error "object_creation_expression" x

   and interm7 = function
    | J.Object [
        "type", J.String "_unqualified_object_creation_expression";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "_ambiguous_name";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "_unqualified_object_creation_expression";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "_primary";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "_unqualified_object_creation_expression";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    
  and unqualified_object_creation_expression = function
   | J.Object [
     "type", J.String "_unqualified_object_creation_expression";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      type_arguments: type_arguments xs option;
      t_type: simple_type xs;
      arguments: argument_list xs;
      body: class_body xs option;
    }
   | x -> error "_unqualified_object_creation_expression" x

  and field_access = function
   | J.Object [
     "type", J.String "field_access";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      t_object: _ambiguous_name xs;
      field: this xs;
    }
    | J.Object [
     "type", J.String "field_access";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      t_object: _primary xs;
      field: identifier xs;
    }
    | J.Object [
     "type", J.String "field_access";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      t_object: super xs;
      field: identifier xs;
    }
    | J.Object [
     "type", J.String "field_access";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      t_object: _ambiguous_name xs;
      super: super xs;
      field: identifier xs;
    }
   | x -> error "field_access" x

    
  and array_access = function
   | J.Object [
     "type", J.String "array_access";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      array: array xs;
      index: expression xs;
    }
   | x -> error "array_access" x

  and array = function
   | J.Object [
     "type", J.String "_ambiguous_name";
     "startPosition", t_start;
      "endPosition", t_end;
     "children", _
    ] -> [wrap t_start t_end]
   | J.Object [
     "type", J.String "_primary";
     "startPosition", t_start;
      "endPosition", t_end;
     "children", _
    ] -> [wrap t_start t_end]


  and method_invocation = function
   | J.Object [
     "type", J.String "method_invocation";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      name: identifier xs;
      arguments: argument_list xs;
    }
    | J.Object [
     "type", J.String "method_invocation";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      name: _reserved_identifier xs;
      arguments: argument_list xs;
    }
    | J.Object [
     "type", J.String "method_invocation";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      t_object: _ambiguous_name xs;
      type_arguments: type_arguments option;
      name: identifier xs;
      arguments: argument_list xs;
    }
    | J.Object [
     "type", J.String "method_invocation";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      t_object: _primary xs;
      type_arguments: type_arguments option;
      name: identifier xs;
      arguments: argument_list xs;
    }
    | J.Object [
     "type", J.String "method_invocation";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      t_object: super xs;
      type_arguments: type_arguments option;
      name: identifier xs;
      arguments: argument_list xs;
    }
   | J.Object [
     "type", J.String "method_invocation";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      t_object: _ambiguous_name xs;
      super: super xs;
      type_arguments: type_arguments option;
      name: identifier xs;
      arguments: argument_list xs;
    }
   | x -> error "method_invocation" x

  and argument_list = function
   | J.Object [
     "type", J.String "argument_list";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      expression: expression list option;
    }
   | x -> error "argument_list" x

  and method_reference = function
   | J.Object [
     "type", J.String "method_reference";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      identifier: identifier xs option;
      arguments: type_arguments xs option;
      name: interm12 xs;
    }

  and interm12 = function
  | J.Object [
      "type", J.String "_type";
      "startPosition", t_start;
      "endPosition", t_end;
      "children", _;
    ] -> [wrap t_start t_end]
  | J.Object [
      "type", J.String "_ambiguous_name";
      "startPosition", t_start;
      "endPosition", t_end;
      "children", _;
    ] -> [wrap t_start t_end]
  | J.Object [
      "type", J.String "_primary";
      "startPosition", t_start;
      "endPosition", t_end;
      "children", _;
    ] -> [wrap t_start t_end]
  | J.Object [
      "type", J.String "super";
      "startPosition", t_start;
      "endPosition", t_end;
      "children", _;
    ] -> [wrap t_start t_end]
  | x -> error "interm12" x

  and type_arguments = function
   | J.Object [
     "type", J.String "type_arguments";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> Some (interm14 xs list option)
   | x -> error "type_arguments" x

  and interm14 = function
  | J.Object [
     "type", J.String "_type";
     "startPosition", t_start;
      "endPosition", t_end;
     "children", _
    ] -> [wrap t_start t_end]
  | J.Object [
     "type", J.String "wildcard";
     "startPosition", t_start;
      "endPosition", t_end;
     "children", _
    ] -> [wrap t_start t_end]

  and wildcard = function
   | J.Object [
     "type", J.String "wildcard";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      left: annotation xs list;
      right: wildcard_bounds xs option;
    }
   | x -> error "wildcard" x

  and wildcard_bounds = function
    | J.Object [
      "type", J.String "_wildcard_bounds";
      "startPosition", t_start;
      "endPosition", t_end;
      "children", _;
    ] -> {
      left: extends xs;
      right: t_type xs;
    }
    | J.Object [
      "type", J.String "_wildcard_bounds";
      "startPosition", t_start;
      "endPosition", t_end;
      "children", _;
    ] -> {
      left: super xs;
      right: t_type xs;
    }
    
  and dimensions = function
   | J.Object [
     "type", J.String "dimensions";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> Some (annotation xs list)
   | x -> error "dimensions" x

  and _statement = function
   | J.Object [
     "type", J.String "_statement";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> Some (interm18 xs) 
   | x -> error "_statement" x

   and interm18 = function
    | J.Object [
        "type", J.String "_declaration";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "expression_statement";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "labeled_statement";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "if_statement";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "while_statement";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "for_statement";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "enhanced_for_statement";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "block";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "assert_statement";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "switch_statement";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "do_statement";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "break_statement";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "continue_statement";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "return_statement";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "synchronized_statement";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "local_variable_declaration";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "throw_statement";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]

    | J.Object [
        "type", J.String "try_statement";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "try_with_resources_statement";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
  
  and block = function
   | J.Object [
     "type", J.String "block";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> Some (statement xs list)
   | x -> error "block" x

  and expression_statement = function
   | J.Object [
     "type", J.String "expression_statement";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> Some (expression xs)
   | x -> error "expression_statement" x

  and labeled_statement = function
   | J.Object [
     "type", J.String "labeled_statement";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      left: identifier xs;
      right: statement xs;
    }
   | x -> error "labeled_statement" x

  and assert_statement = function
   | J.Object [
     "type", J.String "assert_statement";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> Some (expression xs) 
  | J.Object [
     "type", J.String "assert_statement";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> Some (expression xs) * Some (expression xs) 
   | x -> error "assert_statement" x
    
  and switch_statement = function
   | J.Object [
     "type", J.String "switch_statement";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      condition: parenthesized_expression xs;
      body: switch_block xs;
    }
   | x -> error "switch_statement" x

  and switch_block = function
   | J.Object [
     "type", J.String "switch_block";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> Some (switch_label xs list)
   | J.Object [
     "type", J.String "switch_block";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> Some (statement xs list)
   | x -> error "switch_block" x

  and switch_label = function
   | J.Object [
     "type", J.String "switch_label";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> Some (expression xs option)
   | x -> error "switch_label" x
    
  and do_statement = function
   | J.Object [
     "type", J.String "do_statement";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      body: statement xs;
      condition: parenthesized_expression xs;
    }
   | x -> error "do_statement" x

  and break_statement = function
   | J.Object [
     "type", J.String "break_statement";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> Some (identifier xs option)
   | x -> error "break_statement" x

  and continue_statement = function
   | J.Object [
     "type", J.String "continue_statement";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> Some (identifier xs option)
   | x -> error "continue_statement" x

  and return_statement = function
   | J.Object [
     "type", J.String "return_statement";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> Some (expression xs option)
   | x -> error "return_statement" x

  and synchronized_statement = function
   | J.Object [
     "type", J.String "synchronized_statement";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      left: parenthesized_expression xs;
      body: block xs;
    }
   | x -> error "synchronized_statement" x

  and throw_statement = function
   | J.Object [
     "type", J.String "throw_statement";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> Some (expression xs)
   | x -> error "throw_statement" x

  and try_statement = function
   | J.Object [
     "type", J.String "try_statement";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      body: block xs;
      right: catch_clause xs list;
      left: finally_clause xs;
    }
   | x -> error "try_statement" x

  and catch_clause = function
   | J.Object [
     "type", J.String "catch_clause";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      left: catch_formal_parameter xs;
      body: block xs;
    }
   | x -> error "catch_clause" x

  and catch_formal_parameter = function
   | J.Object [
     "type", J.String "catch_formal_parameter";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      modifiers: modifiers xs option;
      right: catch_type xs;
      left: variable_declarator_id xs;
    }
   | x -> error "catch_formal_parameter" x

  and catch_type = function
   | J.Object [
     "type", J.String "catch_type";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> Some (unannotated_type xs list)
   | x -> error "catch_type" x

  and finally_clause = function
   | J.Object [
     "type", J.String "finally_clause";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> Some (block xs)
   | x -> error "finally_clause" x

  and try_with_resources_statement = function
   | J.Object [
     "type", J.String "try_with_resources_statement";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      resources: resource_specification xs;
      body: block xs;
      right: catch_clause xs list;
      left: finally_clause xs option;
    }
   | x -> error "try_with_resources_statement" x

  and resource_specification = function
   | J.Object [
     "type", J.String "resource_specification";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> Some (resource xs list)
   | x -> error "resource_specification" x

  and resource = function
   | J.Object [
     "type", J.String "resource";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      mods: modifiers xs option;
      t_type: unannotated_type xs;
      variable: variable_declarator_id xs;
      value: expression xs;
    }
    | J.Object [
     "type", J.String "resource";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> Some (ambiguous_name xs)
    | J.Object [
     "type", J.String "resource";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> Some (field_access xs)
   | x -> error "resource" x

  and variable_declarator_id = function
    | J.Object [
        "type", J.String "_variable_declarator_id";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
      | x -> error "_variable_declarator_id" x

  and ambiguous_name = function
  | J.Object [
    "type", J.String "_ambiguous_name";
    "startPosition", t_start;
    "endPosition", t_end;
    "children", _;
  ] -> [wrap t_start t_end]
  | x -> error "_ambiguous_name" x

  and if_statement = function
   | J.Object [
     "type", J.String "if_statement";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      condition: parenthesized_expression xs;
      consequence: statement xs;
      alternative: statement xs option;
    }
   | x -> error "if_statement" x

  and while_statement = function
   | J.Object [
     "type", J.String "while_statement";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      condition: carenthesized_expression xs;
      body: statement xs;
    }
   | x -> error "while_statement" x

  and for_statement = function
   | J.Object [
     "type", J.String "for_statement";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      init: local_variable_declaration xs;
      condition: expression xs option;
      update: (expression xs) list option;
      body: statement xs;
    }
   | J.Object [
     "type", J.String "for_statement";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      init: (expression xs) list option;
      condition: expression xs option;
      update: (expression xs) list option;
      body: statement xs;
    }
   | x -> error "for_statement" x

  and enhanced_for_statement = function
   | J.Object [
     "type", J.String "enhanced_for_statement";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      mods: modifiers xs option;
      t_type: unannotated_type xs;
      variable: variable_declarator_id xs;
      value: expression xs; 
      body: statement xs; 
    }
   | x -> error "enhanced_for_statement" x

  and _annotation = function
   | J.Object [
     "type", J.String "_annotation";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> Some (annotation xs) 
    | J.Object [
     "type", J.String "_annotation";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> Some (marker_annotation xs) 
  | x -> error "annotation" x

  and marker_annotation = function
   | J.Object [
     "type", J.String "annotation";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      identifier: identifier xs;
      arguments: annotation_argument_list xs;
    }
   | J.Object [
     "type", J.String "annotation";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      identifier: scoped_identifier xs;
      arguments: annotation_argument_list xs;
    }
   | x -> error "annotation" x

  and annotation_argument_list = function
   | J.Object [
     "type", J.String "annotation_argument_list";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> Some (element_value_pair xs list option)
    | x -> error "annotation_argument_list" x

  and element_value_pair = function
   | J.Object [
     "type", J.String "element_value_pair";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      key: identifier xs;
      value: _element_value xs;
    }
   | x -> error "element_value_pair" x

  and _element_value = function
   | J.Object [
     "type", J.String "_element_value";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> Some (expression xs)
    | J.Object [
     "type", J.String "_element_value";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> Some (element_value_array_initializer xs)
    | J.Object [
     "type", J.String "_element_value";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> Some (_annotation xs)
   | x -> error "_element_value" x

  and element_value_array_initializer = function
   | J.Object [
     "type", J.String "element_value_array_initializer";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> Some (_element_value xs list option)
   | x -> error "element_value_array_initializer" x

  and _declaration = function
   | J.Object [
     "type", J.String "_declaration";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> Some (interm13 xs)

  and interm13 = function
    | J.Object [
        "type", J.String "module_declaration";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "package_declaration";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "import_declaration";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "class_declaration";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "interface_declaration";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "annotation_type_declaration";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "enum_declaration";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
 
  and module_declaration = function
   | J.Object [
     "type", J.String "module_declaration";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      annotation: _annotation xs list;
      name: _ambiguous_name xs;
      t_module: module_directive xs list;
    }
   | x -> error "module_declaration" x

  and module_directive = function
   | J.Object [
     "type", J.String "module_directive";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      modifier: requires_modifier xs list;
      t_module: module_name xs;
    }
    | J.Object [
     "type", J.String "module_directive";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      name: _ambiguous_name xs list;
      t_module: module_name xs list option;
    }
    | J.Object [
     "type", J.String "module_directive";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      name: _ambiguous_name xs list;
      t_module: module_name xs list;
    }
    | J.Object [
     "type", J.String "module_directive";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      name: _ambiguous_name xs list;
    }
    | J.Object [
     "type", J.String "module_directive";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      name: _ambiguous_name xs list;
    }
   

  and requires_modifier = function
   | J.Object [
     "type", J.String "requires_modifier";
     "startPosition", t_start;
      "endPosition", t_end;
     "children", _
    ] -> [wrap t_start t_end]
   | x -> error "requires_modifier" x
  
  and module_name = function
   | J.Object [
     "type", J.String "module_name";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      right: identifier param lists;
      left: module_name xs;
    }
   | x -> error "module_name" x

  and package_declaration = function
   | J.Object [
     "type", J.String "package_declaration";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> Some (_ambiguous_name xs list)
   | x -> error "package_declaration" x

  and import_declaration = function
   | J.Object [
     "type", J.String "import_declaration";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      name: (identifier xs) list;
      symbol: asterisk xs option;
    }
   | x -> error "import_declaration" x

  and asterisk = function
   | J.Object [
     "type", J.String "asterisk";
     "startPosition", t_start;
      "endPosition", t_end;
     "children", _;
    ] -> [wrap t_start t_end]
   | x -> error "asterisk" x

  and enum_declaration = function
   | J.Object [
     "type", J.String "enum_declaration";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      mods: modifiers xs option;
      name: identifier xs;
      interfaces: super_interfaces xs option;
      body: enum_body xs;
    }
   | x -> error "enum_declaration" x

  and enum_body = function
   | J.Object [
     "type", J.String "enum_body";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      head: enum_constant xs list option;
      body: enum_body_declarations xs option;
    }
   | x -> error "enum_body" x

  and enum_body_declarations = function
   | J.Object [
     "type", J.String "enum_body_declarations";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> Some (_class_body_declaration xs list)
   | x -> error "enum_body_declarations" x

  and enum_constant = function
   | J.Object [
     "type", J.String "enum_constant";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      mods: modifiers xs option;
      name: identifier xs;
      arguments: argument_list xs option;
      body: class_body xs option;
    }
   | x -> error "enum_constant" x

  and class_declaration = function
   | J.Object [
     "type", J.String "class_declaration";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      mods: modifiers xs option;
      name: identifier xs;
      t_type: type_parameters xs option;
      superclass: superclass xs option;
      interfaces: super_interfaces xs option;
      body: class_body xs option;
    }
   | x -> error "class_declaration" x

  and type_parameters = function
   | J.Object [
     "type", J.String "type_parameters";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> Some (type_parameter xs list)
   | x -> error "type_parameters" x

  and type_parameter = function
   | J.Object [
     "type", J.String "type_parameter";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      annotation: _annotation xs list;
      identifier: identifier xs;
      t_type: type_bound xs option;
    }
   | x -> error "type_parameter" x

  and type_bound = function
   | J.Object [
     "type", J.String "type_bound";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      right: t_type xs;
      left: t_type xs list;
    }
   | x -> error "type_bound" x

  and superclass = function
   | J.Object [
     "type", J.String "superclass";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> Some (_type xs)
   | x -> error "superclass" x

  and super_interfaces = function
   | J.Object [
     "type", J.String "super_interfaces";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> Some (interface_type_list xs)
   | x -> error "super_interfaces" x

  and interface_type_list = function
   | J.Object [
     "type", J.String "interface_type_list";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      right: t_type xs;
      left: t_type xs list;
    }
   | x -> error "interface_type_list" x

  and class_body = function
   | J.Object [
     "type", J.String "class_body";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> Some (_class_body_declaration xs list)
   | x -> error "class_body" x

  and _class_body_declaration = function
   | J.Object [
     "type", J.String "_class_body_declaration";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> Some (interm54 xs) 
   | x -> error "_class_body_declaration" x

   and interm54 = function
    | J.Object [
        "type", J.String "_class_member_declaration";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "block";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "static_initializer";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "constructor_declaration";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]


  and static_initializer = function
   | J.Object [
     "type", J.String "static_initializer";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> Some (block xs)
   | x -> error "static_initializer" x

  and constructor_declaration = function
   | J.Object [
     "type", J.String "constructor_declaration";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      mods: modifiers xs option;
      constructor: _constructor_declarator xs;
      throws: throws xs option;
      body: constructor_body xs;
    }
   | x -> error "constructor_declaration" x

  and _constructor_declarator = function
   | J.Object [
     "type", J.String "_constructor_declarator";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      type_parameters: type_parameters xs option;
      name: identifier xs;
      parameters: formal_parameters xs;
    }
   | x -> error "_constructor_declarator" x

  and constructor_body = function
   | J.Object [
     "type", J.String "constructor_body";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      constructor: explicit_constructor_invocation xs option;
      statement: _statement xs list;
    }
   | x -> error "constructor_body" x

  and explicit_constructor_invocation = function
   | J.Object [
     "type", J.String "explicit_constructor_invocation";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      type_arguments: type_arguments xs option;
      constructor: this xs;
    }
   | J.Object [
     "type", J.String "explicit_constructor_invocation";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      type_arguments: type_arguments xs option;
      constructor: super xs;
    }
    | J.Object [
     "type", J.String "explicit_constructor_invocation";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      t_object: _ambiguous_name xs;
      type_arguments: type_arguments xs option;
      constructor: super xs;
    }
    | J.Object [
     "type", J.String "explicit_constructor_invocation";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      t_object: _primary xs;
      type_arguments: type_arguments xs option;
      constructor: super xs;
    }
   | x -> error "explicit_constructor_invocation" x

  and _ambiguous_name = function
   | J.Object [
     "type", J.String "_ambiguous_name";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> Some (identifier xs) 
    | J.Object [
     "type", J.String "_ambiguous_name";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> Some (_reserved_identifier xs) 
    | J.Object [
     "type", J.String "_ambiguous_name";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> Some (scoped_identifier xs) 
   | x -> error "_ambiguous_name" x

  and scoped_identifier = function
   | J.Object [
     "type", J.String "scoped_identifier";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      scope: identifier xs;
      name: identifier xs;
    }
    | J.Object [
     "type", J.String "scoped_identifier";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      scope: _reserved_identifier xs;
      name: identifier xs;
    }
    | J.Object [
     "type", J.String "scoped_identifier";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      scope: scoped_identifier xs;
      name: identifier xs;
    }
   | x -> error "scoped_identifier" x

  and _class_member_declaration = function
   | J.Object [
     "type", J.String "_class_member_declaration";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> Some (interm60 xs) 
   | x -> error "_class_member_declaration" x

   and interm60 = function
    | J.Object [
        "type", J.String "field_declaration";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "method_declaration";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "class_declaration";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "interface_declaration";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "annotation_type_declaration";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "enum_declaration";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]

  and field_declaration = function
   | J.Object [
     "type", J.String "field_declaration";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      mods: modifiers xs option;
      t_type: _unannotated_type xs;
      variable: _variable_declarator_list xs;
    }
   | x -> error "field_declaration" x

  and annotation_type_declaration = function
   | J.Object [
     "type", J.String "annotation_type_declaration";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      mods: modifiers xs option;
      name: identifier xs;
      body: annotation_type_body xs;
    }
   | x -> error "annotation_type_declaration" x

  and annotation_type_body = function
   | J.Object [
     "type", J.String "annotation_type_body";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> Some (_annotation_type_member_declaration xs list)
   | x -> error "annotation_type_body" x

  and _annotation_type_member_declaration = function
   | J.Object [
     "type", J.String "_annotation_type_member_declaration";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> Some (interm63 xs) 
   | x -> error "_annotation_type_member_declaration" x

   and interm63 = function
    | J.Object [
        "type", J.String "annotation_type_element_declaration";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "constant_declaration";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "class_declaration";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "interface_declaration";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "annotation_type_declaration";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
      

  and annotation_type_element_declaration = function
   | J.Object [
     "type", J.String "annotation_type_element_declaration";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      mods: modifiers xs option;
      t_type: _unannotated_type xs;
      name: identifier xs;
      dimensions: dimensions xs option;
      value: _default_value xs option;
    }
   | x -> error "annotation_type_element_declaration" x

  and _default_value = function
   | J.Object [
     "type", J.String "_default_value";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      value: _element_value xs;
    }
   | x -> error "_default_value" x

  and interface_declaration = function
   | J.Object [
     "type", J.String "interface_declaration";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      mods: modifiers xs option;
      name: identifier xs;
      type_parameters: type_parameters xs option;
      extends: extends_interfaces xs option;
      body: interface_body xs;
    }
   | x -> error "interface_declaration" x

  and extends_interfaces = function
   | J.Object [
     "type", J.String "extends_interfaces";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> Some (interface_type_list xs)
   | x -> error "extends_interfaces" x

  and interface_body = function
   | J.Object [
     "type", J.String "interface_body";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> Some (_interface_member_declaration xs list)
   | x -> error "interface_body" x

  and _interface_member_declaration = function
   | J.Object [
     "type", J.String "_interface_member_declaration";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> Some (interm68 xs) 
   | x -> error "_interface_member_declaration" x

   and interm68 = function
    | J.Object [
        "type", J.String "constant_declaration";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "enum_declaration";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "method_declaration";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "class_declaration";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "interface_declaration";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "annotation_type_declaration";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
      

  and constant_declaration = function
   | J.Object [
     "type", J.String "constant_declaration";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      mods: modifiers xs option;
      t_type: _unannotated_type xs;
      variable: _variable_declarator_list xs;
    }
   | x -> error "constant_declaration" x

  and _variable_declarator_list = function
   | J.Object [
     "type", J.String "_variable_declarator_list";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      declarator: variable_declarator xs list;
    }
   | x -> error "_variable_declarator_list" x

  and variable_declarator = function
   | J.Object [
     "type", J.String "variable_declarator";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      id: _variable_declarator_id xs;
      t_initializer: _variable_initializer xs option;
    }
   | x -> error "variable_declarator" x

  and _variable_declarator_id = function
   | J.Object [
     "type", J.String "_variable_declarator_id";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      name: identifier xs;
      dimensions: dimensions xs option;
    }
    | J.Object [
     "type", J.String "_variable_declarator_id";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      name: _reserved_identifier xs;
      dimensions: dimensions xs option;
    }
   | x -> error "_variable_declarator_id" x

  and _variable_initializer = function
   | J.Object [
     "type", J.String "_variable_initializer";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> Some (interm71 xs) 
   | x -> error "_variable_initializer" x

   and interm71 = function
    | J.Object [
        "type", J.String "_expression";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "array_initializer";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]

  and array_initializer = function
   | J.Object [
     "type", J.String "array_initializer";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> Some (_variable_initializer xs list option)
   | x -> error "array_initializer" x

  and t_type = function
   | J.Object [
     "type", J.String "_type";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> Some (interm74 xs) 
   | x -> error "_type" x

   and interm74 = function
    | J.Object [
        "type", J.String "_unannotated_type";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]

    | J.Object [
        "type", J.String "annotated_type";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]


  and _unannotated_type = function
   | J.Object [
     "type", J.String "_unannotated_type";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> Some (interm75 xs) 
   | x -> error "_unannotated_type" x

   and interm75 = function
    | J.Object [
        "type", J.String "_simple_type";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "array_type";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
      

  and _simple_type = function
   | J.Object [
     "type", J.String "_simple_type";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> Some (interm76 xs) 
   | x -> error "_simple_type" x

   and interm76 = function
    | J.Object [
        "type", J.String "void_type";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "_numeric_type";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "boolean_type";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "scoped_type_identifier";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "generic_type";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]

  and annotated_type = function
   | J.Object [
     "type", J.String "annotated_type";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      annotation: _annotation xs list;
      _unannotated_type: _unannotated_type xs;
    }
   | x -> error "annotated_type" x

  and scoped_type_identifier = function
   | J.Object [
     "type", J.String "scoped_type_identifier";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      type_identifier: type_identifier xs;
      annotaiton: _annotation xs list;
      t_type: generic_type xs;
    }
    | J.Object [
     "type", J.String "scoped_type_identifier";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      type_identifier: type_identifier xs;
      annotaiton: _annotation xs list;
      t_type: scoped_type_identifier xs;
    }
    | J.Object [
     "type", J.String "scoped_type_identifier";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      type_identifier: type_identifier xs list;
      annotaiton: _annotation xs list;
    }
   | x -> error "scoped_type_identifier" x

  and generic_type = function
   | J.Object [
     "type", J.String "generic_type";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      arguments: type_arguments xs;
      type_identifier: identifier xs;
    }
    | J.Object [
     "type", J.String "generic_type";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      arguments: type_arguments xs;
      type_identifier: scoped_type_identifier xs;
    }
   | x -> error "generic_type" x

  and array_type = function
   | J.Object [
     "type", J.String "array_type";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      element: _unannotated_type xs;
      dimensions: dimensions xs;
    }
   | x -> error "array_type" x

  and _numeric_type = function
   | J.Object [
     "type", J.String "_numeric_type";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> Some (interm79 xs) 
   | x -> error "_numeric_type" x

   and interm79 = function
    | J.Object [
        "type", J.String "integral_type";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]
    | J.Object [
        "type", J.String "floating_point_type";
        "startPosition", t_start;
        "endPosition", t_end;
        "children", _;
      ] -> [wrap t_start t_end]

  and integral_type = function
   | J.Object [
     "type", J.String "integral_type";
     "startPosition", t_start;
      "endPosition", t_end;
     "children", J.Object xs
    ] -> [wrap t_start t_end]
   | x -> error "integral_type" x

  and floating_point_type = function
   | J.Object [
     "type", J.String "floating_point_type";
     "startPosition", t_start;
      "endPosition", t_end;
     "children", J.Object xs
    ] -> [wrap t_start t_end]
   | x -> error "floating_point_type" x

  and boolean_type = function
   | J.Object [
     "type", J.String "boolean_type";
     "startPosition", t_start;
      "endPosition", t_end;
     "children", _;
    ] -> [wrap t_start t_end]
   | x -> error "boolean_type" x

  and void_type = function
   | J.Object [
     "type", J.String "void_type";
     "startPosition", t_start;
      "endPosition", t_end;
     "children", _;
    ] -> [wrap t_start t_end]
   | x -> error "void_type" x

  and _method_header = function
   | J.Object [
     "type", J.String "_method_header";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      type_parameters: type_parameters xs list option;
      annotation: _annotation xs list option;
      t_type: _unannotated_type xs;
      declarator: _method_declarator xs;
      throws: throws xs option;
    }
   | x -> error "_method_header" x

  and _method_declarator = function
   | J.Object [
     "type", J.String "_method_declarator";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      name: identifier xs;
      parameters: formal_parameters xs;
      dimensions: dimensions xs option;
    }
    | J.Object [
     "type", J.String "_method_declarator";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      name: _reserved_identifier xs;
      parameters: formal_parameters xs;
      dimensions: dimensions xs option;
    }
   | x -> error "_method_declarator" x

  and formal_parameters = function
   | J.Object [
     "type", J.String "formal_parameters";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      parameter: receiver_parameter xs option;
      formal_parameter: formal_parameter xs list option;
      spread: spread_parameter xs option;
    }
   | x -> error "formal_parameters" x

  and formal_parameter = function
   | J.Object [
     "type", J.String "formal_parameter";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      mods: modifiers xs option;
      t_type: _unannotated_type xs;
      id: _variable_declarator_id xs;
    }
   | x -> error "formal_parameter" x

  and receiver_parameter = function
   | J.Object [
     "type", J.String "receiver_parameter";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      annotation: _annotation xs list;
      t_type: _unannotated_type xs;
      id: identifier xs option;
      this: this xs;
    }
   | x -> error "receiver_parameter" x

  and spread_parameter = function
   | J.Object [
     "type", J.String "spread_parameter";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      mods: modifiers xs option;
      t_type: _unannotated_type xs;
      declarator: variable_declarator xs;
    }
   | x -> error "spread_parameter" x

  and throws = function
   | J.Object [
     "type", J.String "throws";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> Some (_type xs list)
   | x -> error "throws" x

  and local_variable_declaration = function
   | J.Object [
     "type", J.String "local_variable_declaration";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      mods: modifiers xs option;
      t_type: _unannotated_type xs;
      declarator: _variable_declarator_list xs;
    }
   | x -> error "local_variable_declaration" x

  and method_declaration = function
   | J.Object [
     "type", J.String "method_declaration";
     "startPosition", _;
      "endPosition", _;
     "children", J.Object xs
    ] -> {
      mods: modifiers xs option;
      header: _method_header xs;
      body: block xs option;
    }
   | x -> error "method_declaration" x

  and _reserved_identifier = function
   | J.Object [
     "type", J.String "_reserved_identifier";
     "startPosition", t_start;
      "endPosition", t_end;
     "children", _;
    ] -> [wrap t_start t_end]
   | x -> error "_reserved_identifier" x

  and this = function
   | J.Object [
     "type", J.String "this";
     "startPosition", t_start;
      "endPosition", t_end;
     "children", _;
    ] -> [wrap t_start t_end]
   | x -> error "this" x

  and super = function
   | J.Object [
     "type", J.String "super";
     "startPosition", t_start;
      "endPosition", t_end;
     "children", _;
    ] -> [wrap t_start t_end]
   | x -> error "super" x

  and identifier = function
   | J.Object [
     "type", J.String "identifier";
     "startPosition", t_start;
      "endPosition", t_end;
     "children", _;
    ] -> [wrap t_start t_end]
   | x -> error "identifier" x

  and comment = function
   | J.Object [
     "type", J.String "comment";
     "startPosition", t_start;
      "endPosition", t_end;
     "children", _;
    ] -> [wrap t_start t_end]
   | x -> error "comment" x

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
