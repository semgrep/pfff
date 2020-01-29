
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
    "children", J.Object xs;
  ] -> { package = Some (package xs);
         imports = Some (imports xs);
         decls = Some (decls xs) }
  | x -> error "program" x

  and _literal = function
   | J.Object [
     "type", J.String "_literal";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "_literal" x

  and decimal_integer_literal = function
   | J.Object [
     "type", J.String "decimal_integer_literal";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "decimal_integer_literal" x

  and hex_integer_literal = function
   | J.Object [
     "type", J.String "hex_integer_literal";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "hex_integer_literal" x

  and octal_integer_literal = function
   | J.Object [
     "type", J.String "octal_integer_literal";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "octal_integer_literal" x

  and binary_integer_literal = function
   | J.Object [
     "type", J.String "binary_integer_literal";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "binary_integer_literal" x

  and decimal_floating_point_literal = function
   | J.Object [
     "type", J.String "decimal_floating_point_literal";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "decimal_floating_point_literal" x

  and hex_floating_point_literal = function
   | J.Object [
     "type", J.String "hex_floating_point_literal";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "hex_floating_point_literal" x

  and true = function
   | J.Object [
     "type", J.String "true";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "true" x

  and false = function
   | J.Object [
     "type", J.String "false";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "false" x

  and character_literal = function
   | J.Object [
     "type", J.String "character_literal";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "character_literal" x

  and string_literal = function
   | J.Object [
     "type", J.String "string_literal";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "string_literal" x

  and null_literal = function
   | J.Object [
     "type", J.String "null_literal";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "null_literal" x

  and _expression = function
   | J.Object [
     "type", J.String "_expression";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "_expression" x

  and cast_expression = function
   | J.Object [
     "type", J.String "cast_expression";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "cast_expression" x

  and assignment_expression = function
   | J.Object [
     "type", J.String "assignment_expression";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "assignment_expression" x

  and binary_expression = function
   | J.Object [
     "type", J.String "binary_expression";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "binary_expression" x

  and instanceof_expression = function
   | J.Object [
     "type", J.String "instanceof_expression";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "instanceof_expression" x

  and lambda_expression = function
   | J.Object [
     "type", J.String "lambda_expression";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "lambda_expression" x

  and inferred_parameters = function
   | J.Object [
     "type", J.String "inferred_parameters";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "inferred_parameters" x

  and ternary_expression = function
   | J.Object [
     "type", J.String "ternary_expression";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "ternary_expression" x

  and unary_expression = function
   | J.Object [
     "type", J.String "unary_expression";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "unary_expression" x

  and update_expression = function
   | J.Object [
     "type", J.String "update_expression";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "update_expression" x

  and _primary = function
   | J.Object [
     "type", J.String "_primary";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "_primary" x

  and array_creation_expression = function
   | J.Object [
     "type", J.String "array_creation_expression";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "array_creation_expression" x

  and dimensions_expr = function
   | J.Object [
     "type", J.String "dimensions_expr";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "dimensions_expr" x

  and parenthesized_expression = function
   | J.Object [
     "type", J.String "parenthesized_expression";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "parenthesized_expression" x

  and class_literal = function
   | J.Object [
     "type", J.String "class_literal";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "class_literal" x

  and object_creation_expression = function
   | J.Object [
     "type", J.String "object_creation_expression";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "object_creation_expression" x

  and _unqualified_object_creation_expression = function
   | J.Object [
     "type", J.String "_unqualified_object_creation_expression";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "_unqualified_object_creation_expression" x

  and field_access = function
   | J.Object [
     "type", J.String "field_access";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "field_access" x

  and array_access = function
   | J.Object [
     "type", J.String "array_access";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "array_access" x

  and method_invocation = function
   | J.Object [
     "type", J.String "method_invocation";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "method_invocation" x

  and argument_list = function
   | J.Object [
     "type", J.String "argument_list";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "argument_list" x

  and method_reference = function
   | J.Object [
     "type", J.String "method_reference";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "method_reference" x

  and type_arguments = function
   | J.Object [
     "type", J.String "type_arguments";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "type_arguments" x

  and wildcard = function
   | J.Object [
     "type", J.String "wildcard";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "wildcard" x

  and _wildcard_bounds = function
   | J.Object [
     "type", J.String "_wildcard_bounds";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "_wildcard_bounds" x

  and dimensions = function
   | J.Object [
     "type", J.String "dimensions";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "dimensions" x

  and _statement = function
   | J.Object [
     "type", J.String "_statement";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "_statement" x

  and block = function
   | J.Object [
     "type", J.String "block";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "block" x

  and expression_statement = function
   | J.Object [
     "type", J.String "expression_statement";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "expression_statement" x

  and labeled_statement = function
   | J.Object [
     "type", J.String "labeled_statement";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "labeled_statement" x

  and assert_statement = function
   | J.Object [
     "type", J.String "assert_statement";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "assert_statement" x

  and switch_statement = function
   | J.Object [
     "type", J.String "switch_statement";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "switch_statement" x

  and switch_block = function
   | J.Object [
     "type", J.String "switch_block";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "switch_block" x

  and switch_label = function
   | J.Object [
     "type", J.String "switch_label";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "switch_label" x

  and do_statement = function
   | J.Object [
     "type", J.String "do_statement";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "do_statement" x

  and break_statement = function
   | J.Object [
     "type", J.String "break_statement";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "break_statement" x

  and continue_statement = function
   | J.Object [
     "type", J.String "continue_statement";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "continue_statement" x

  and return_statement = function
   | J.Object [
     "type", J.String "return_statement";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "return_statement" x

  and synchronized_statement = function
   | J.Object [
     "type", J.String "synchronized_statement";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "synchronized_statement" x

  and throw_statement = function
   | J.Object [
     "type", J.String "throw_statement";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "throw_statement" x

  and try_statement = function
   | J.Object [
     "type", J.String "try_statement";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "try_statement" x

  and catch_clause = function
   | J.Object [
     "type", J.String "catch_clause";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "catch_clause" x

  and catch_formal_parameter = function
   | J.Object [
     "type", J.String "catch_formal_parameter";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "catch_formal_parameter" x

  and catch_type = function
   | J.Object [
     "type", J.String "catch_type";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "catch_type" x

  and finally_clause = function
   | J.Object [
     "type", J.String "finally_clause";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "finally_clause" x

  and try_with_resources_statement = function
   | J.Object [
     "type", J.String "try_with_resources_statement";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "try_with_resources_statement" x

  and resource_specification = function
   | J.Object [
     "type", J.String "resource_specification";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "resource_specification" x

  and resource = function
   | J.Object [
     "type", J.String "resource";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "resource" x

  and if_statement = function
   | J.Object [
     "type", J.String "if_statement";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "if_statement" x

  and while_statement = function
   | J.Object [
     "type", J.String "while_statement";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "while_statement" x

  and for_statement = function
   | J.Object [
     "type", J.String "for_statement";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "for_statement" x

  and enhanced_for_statement = function
   | J.Object [
     "type", J.String "enhanced_for_statement";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "enhanced_for_statement" x

  and _annotation = function
   | J.Object [
     "type", J.String "_annotation";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "_annotation" x

  and marker_annotation = function
   | J.Object [
     "type", J.String "marker_annotation";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "marker_annotation" x

  and annotation = function
   | J.Object [
     "type", J.String "annotation";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "annotation" x

  and annotation_argument_list = function
   | J.Object [
     "type", J.String "annotation_argument_list";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "annotation_argument_list" x

  and element_value_pair = function
   | J.Object [
     "type", J.String "element_value_pair";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "element_value_pair" x

  and _element_value = function
   | J.Object [
     "type", J.String "_element_value";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "_element_value" x

  and element_value_array_initializer = function
   | J.Object [
     "type", J.String "element_value_array_initializer";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "element_value_array_initializer" x

  and _declaration = function
   | J.Object [
     "type", J.String "_declaration";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "_declaration" x

  and module_declaration = function
   | J.Object [
     "type", J.String "module_declaration";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "module_declaration" x

  and module_directive = function
   | J.Object [
     "type", J.String "module_directive";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "module_directive" x

  and requires_modifier = function
   | J.Object [
     "type", J.String "requires_modifier";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "requires_modifier" x

  and module_name = function
   | J.Object [
     "type", J.String "module_name";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "module_name" x

  and package_declaration = function
   | J.Object [
     "type", J.String "package_declaration";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "package_declaration" x

  and import_declaration = function
   | J.Object [
     "type", J.String "import_declaration";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "import_declaration" x

  and asterisk = function
   | J.Object [
     "type", J.String "asterisk";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "asterisk" x

  and enum_declaration = function
   | J.Object [
     "type", J.String "enum_declaration";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "enum_declaration" x

  and enum_body = function
   | J.Object [
     "type", J.String "enum_body";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "enum_body" x

  and enum_body_declarations = function
   | J.Object [
     "type", J.String "enum_body_declarations";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "enum_body_declarations" x

  and enum_constant = function
   | J.Object [
     "type", J.String "enum_constant";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "enum_constant" x

  and class_declaration = function
   | J.Object [
     "type", J.String "class_declaration";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "class_declaration" x

  and modifiers = function
   | J.Object [
     "type", J.String "modifiers";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "modifiers" x

  and type_parameters = function
   | J.Object [
     "type", J.String "type_parameters";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "type_parameters" x

  and type_parameter = function
   | J.Object [
     "type", J.String "type_parameter";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "type_parameter" x

  and type_bound = function
   | J.Object [
     "type", J.String "type_bound";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "type_bound" x

  and superclass = function
   | J.Object [
     "type", J.String "superclass";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "superclass" x

  and super_interfaces = function
   | J.Object [
     "type", J.String "super_interfaces";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "super_interfaces" x

  and interface_type_list = function
   | J.Object [
     "type", J.String "interface_type_list";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "interface_type_list" x

  and class_body = function
   | J.Object [
     "type", J.String "class_body";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "class_body" x

  and _class_body_declaration = function
   | J.Object [
     "type", J.String "_class_body_declaration";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "_class_body_declaration" x

  and static_initializer = function
   | J.Object [
     "type", J.String "static_initializer";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "static_initializer" x

  and constructor_declaration = function
   | J.Object [
     "type", J.String "constructor_declaration";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "constructor_declaration" x

  and _constructor_declarator = function
   | J.Object [
     "type", J.String "_constructor_declarator";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "_constructor_declarator" x

  and constructor_body = function
   | J.Object [
     "type", J.String "constructor_body";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "constructor_body" x

  and explicit_constructor_invocation = function
   | J.Object [
     "type", J.String "explicit_constructor_invocation";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "explicit_constructor_invocation" x

  and _ambiguous_name = function
   | J.Object [
     "type", J.String "_ambiguous_name";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "_ambiguous_name" x

  and scoped_identifier = function
   | J.Object [
     "type", J.String "scoped_identifier";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "scoped_identifier" x

  and _class_member_declaration = function
   | J.Object [
     "type", J.String "_class_member_declaration";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "_class_member_declaration" x

  and field_declaration = function
   | J.Object [
     "type", J.String "field_declaration";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "field_declaration" x

  and annotation_type_declaration = function
   | J.Object [
     "type", J.String "annotation_type_declaration";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "annotation_type_declaration" x

  and annotation_type_body = function
   | J.Object [
     "type", J.String "annotation_type_body";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "annotation_type_body" x

  and _annotation_type_member_declaration = function
   | J.Object [
     "type", J.String "_annotation_type_member_declaration";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "_annotation_type_member_declaration" x

  and annotation_type_element_declaration = function
   | J.Object [
     "type", J.String "annotation_type_element_declaration";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "annotation_type_element_declaration" x

  and _default_value = function
   | J.Object [
     "type", J.String "_default_value";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "_default_value" x

  and interface_declaration = function
   | J.Object [
     "type", J.String "interface_declaration";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "interface_declaration" x

  and extends_interfaces = function
   | J.Object [
     "type", J.String "extends_interfaces";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "extends_interfaces" x

  and interface_body = function
   | J.Object [
     "type", J.String "interface_body";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "interface_body" x

  and _interface_member_declaration = function
   | J.Object [
     "type", J.String "_interface_member_declaration";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "_interface_member_declaration" x

  and constant_declaration = function
   | J.Object [
     "type", J.String "constant_declaration";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "constant_declaration" x

  and _variable_declarator_list = function
   | J.Object [
     "type", J.String "_variable_declarator_list";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "_variable_declarator_list" x

  and variable_declarator = function
   | J.Object [
     "type", J.String "variable_declarator";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "variable_declarator" x

  and _variable_declarator_id = function
   | J.Object [
     "type", J.String "_variable_declarator_id";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "_variable_declarator_id" x

  and _variable_initializer = function
   | J.Object [
     "type", J.String "_variable_initializer";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "_variable_initializer" x

  and array_initializer = function
   | J.Object [
     "type", J.String "array_initializer";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "array_initializer" x

  and _type = function
   | J.Object [
     "type", J.String "_type";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "_type" x

  and _unannotated_type = function
   | J.Object [
     "type", J.String "_unannotated_type";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "_unannotated_type" x

  and _simple_type = function
   | J.Object [
     "type", J.String "_simple_type";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "_simple_type" x

  and annotated_type = function
   | J.Object [
     "type", J.String "annotated_type";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "annotated_type" x

  and scoped_type_identifier = function
   | J.Object [
     "type", J.String "scoped_type_identifier";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "scoped_type_identifier" x

  and generic_type = function
   | J.Object [
     "type", J.String "generic_type";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "generic_type" x

  and array_type = function
   | J.Object [
     "type", J.String "array_type";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "array_type" x

  and _numeric_type = function
   | J.Object [
     "type", J.String "_numeric_type";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "_numeric_type" x

  and integral_type = function
   | J.Object [
     "type", J.String "integral_type";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "integral_type" x

  and floating_point_type = function
   | J.Object [
     "type", J.String "floating_point_type";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "floating_point_type" x

  and boolean_type = function
   | J.Object [
     "type", J.String "boolean_type";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "boolean_type" x

  and void_type = function
   | J.Object [
     "type", J.String "void_type";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "void_type" x

  and _method_header = function
   | J.Object [
     "type", J.String "_method_header";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "_method_header" x

  and _method_declarator = function
   | J.Object [
     "type", J.String "_method_declarator";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "_method_declarator" x

  and formal_parameters = function
   | J.Object [
     "type", J.String "formal_parameters";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "formal_parameters" x

  and formal_parameter = function
   | J.Object [
     "type", J.String "formal_parameter";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "formal_parameter" x

  and receiver_parameter = function
   | J.Object [
     "type", J.String "receiver_parameter";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "receiver_parameter" x

  and spread_parameter = function
   | J.Object [
     "type", J.String "spread_parameter";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "spread_parameter" x

  and throws = function
   | J.Object [
     "type", J.String "throws";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "throws" x

  and local_variable_declaration = function
   | J.Object [
     "type", J.String "local_variable_declaration";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "local_variable_declaration" x

  and method_declaration = function
   | J.Object [
     "type", J.String "method_declaration";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "method_declaration" x

  and _reserved_identifier = function
   | J.Object [
     "type", J.String "_reserved_identifier";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "_reserved_identifier" x

  and this = function
   | J.Object [
     "type", J.String "this";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "this" x

  and super = function
   | J.Object [
     "type", J.String "super";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "super" x

  and identifier = function
   | J.Object [
     "type", J.String "identifier";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "identifier" x

  and comment = function
   | J.Object [
     "type", J.String "comment";
     "startPosition", _;
      "endPosition", _;
     "value", params
    ] -> params
   | x -> todo "comment" x

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)
let parse file = 
  let json = json_of_filename_with_external_prog file in
  (* to decide wether this or program_of_babelfish_json *)
  let program = program_of_tree_sitter_json file json in
  program
