
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

module J = Json_type

type 'a wrap  = 'a * string

and _literal =
| Decimal of decimal_integer_literal
| Hex of hex_integer_literal
| Octal of octal_integer_literal
| Binary of binary_integer_literal
| Decimal1 of decimal_floating_point_literal
| Hex1 of hex_floating_point_literal
| Bool of string wrap
| Character of character_literal
| String of string_literal
| Null of null_literal


and decimal_integer_literal = string wrap

and hex_integer_literal = string wrap

and octal_integer_literal = string wrap 

and binary_integer_literal = string wrap 

and decimal_floating_point_literal = string wrap 

and hex_floating_point_literal = string wrap 

and character_literal = string wrap 

and string_literal = string wrap 

and null_literal = string wrap 

and _expression =
| Assignment of assignment_expression
| Binary1 of binary_expression
| Instanceof of instanceof_expression
| Lambda of lambda_expression
| Ternary of ternary_expression
| Update of update_expression
| Ambiguous1 of _ambiguous_name
| Primary2 of _primary
| Unary of unary_expression
| Cast of cast_expression


and cast_expression = {
  t_type: _type;
  t_type12: _type list;
  value4: _expression;
}

and assignment_expression = {
  left: interm17;
  operator: string wrap;
  right: _expression;
}

and interm17 = 
 | Ambiguous5 of _ambiguous_name
 | Field4 of field_access
 | Array3 of array_access

and binary_expression = {
  left2: _expression;
  right2: _expression;
}


and instanceof_expression = {
  left1: _expression;
  operator1: string wrap;
  right1: _type;
}

and lambda_expression = {
  body: interm15;
  parameters1: interm16;
}

and interm15 =
| Expression2 of _expression
| Block of block

and interm16 =
| Formal of formal_parameters
| Identifier1 of identifier
| Inferred of inferred_parameters


and inferred_parameters = identifier * ( identifier list)

and ternary_expression = {
  condition1: _expression;
  consequence1: _expression;
  alternative: _expression;
}

and unary_expression = {
  operator2: string wrap;
  operand: _expression;
}

and update_expression = _expression
 
and _primary =
| Literal of _literal
| Class of class_literal
| This of this
| Parenthesized of parenthesized_expression
| Object of object_creation_expression
| Field2 of field_access
| Array of array_access
| Method2 of method_invocation
| Method11 of method_reference
| Array11 of array_creation_expression


and array_creation_expression = {
  t_type10: _simple_type;
  t_dimensions: interm1;
  value5: array_initializer;
}

and interm1 = 
| Dimensions1 of dimensions_expr list
| Dimensions of dimensions option

and dimensions_expr = ( _annotation list) * _expression

and parenthesized_expression = _expression

and class_literal =
| Ambiguous2 of _ambiguous_name
| Numeric1 of _numeric_type
| Boolean1 of boolean_type
| Void1 of void_type


and object_creation_expression =
| Unqualified of _unqualified_object_creation_expression
| Ambiguous3 of _ambiguous_name * _unqualified_object_creation_expression
| Primary3 of _primary * _unqualified_object_creation_expression


and _unqualified_object_creation_expression = {
  type_arguments1: type_arguments option;
  t_type1: _simple_type;
  arguments1: argument_list;
  class_body: class_body option;
}

and field_access =
| Interm2 of interm2
| Interm3 of interm3
| Interm4 of interm4

and interm2 = {
  t_object2: _ambiguous_name;
  field1: this;
}

and interm3 = {
  t_object3: _primary * super;
  field2: identifier;
}

and interm4 = {
  t_object4: _ambiguous_name;
  mods15: super;
  field3: identifier;
}
  

and array_access = {
  array: _ambiguous_name * _primary;
  index: _expression;
}

and method_invocation = 
| Interm5 of interm5
| Interm6 of interm6
| Interm7 of interm7

and interm5 = {
  t_object5: _ambiguous_name * _primary * super * _ambiguous_name;
  type_arguments2: type_arguments;
  name1: identifier;
}

and interm6 = {
  t_object11: _ambiguous_name;
  supertype_arguments2: type_arguments;
  name2: identifier;
  arguments2: argument_list;
}

and interm7 = {
  name3: identifier * _reserved_identifier;
}

and argument_list = (_expression * ( _expression list)) option

and method_reference = interm8 * type_arguments option * identifier option

and interm8 = 
| Type2 of _type
| Ambiguous4 of _ambiguous_name
| Primary1 of _primary
| Super1 of super

and type_arguments = (interm9 * (_type * wildcard) list) option

and interm9 =
| Type1 of _type
| Wildcard of wildcard

and wildcard = ( _annotation list) * _wildcard_bounds option

and _wildcard_bounds =
| Type of _type
| Super of super * _type

and dimensions = ( _annotation list) 

and _statement =
| Declaration of _declaration
| Expression3 of expression_statement
| Labeled of labeled_statement
| If of if_statement
| While of while_statement
| For of for_statement
| Enhanced of enhanced_for_statement
| Block1 of block
| Assert of assert_statement
| Switch1 of switch_statement
| Do of do_statement
| Break of break_statement
| Continue of continue_statement
| Return of return_statement
| Synchronized of synchronized_statement
| Local1 of local_variable_declaration
| Throw of throw_statement
| Try of try_statement
| Try1 of try_with_resources_statement


and block = ( _statement list)

and expression_statement = _expression

and labeled_statement = identifier * _statement

and assert_statement =
| Expression4 of _expression
| Expression11 of _expression * _expression


and switch_statement = {
  condition2: parenthesized_expression;
  body20: switch_block;
}

and switch_block = interm10 list

and interm10 = 
| Switch of switch_label
| Statement of _statement 

and switch_label = _expression

and do_statement = {
  body2: _statement;
  condition3: parenthesized_expression;
}

and break_statement = identifier option

and continue_statement = identifier option

and return_statement = _expression option

and synchronized_statement = {
 paren: parenthesized_expression;
 body3: block;
}

and throw_statement = _expression

and try_statement = {
  body4: block;
  extra3: ( catch_clause list) * finally_clause option
}

and catch_clause = {
 extra22: catch_formal_parameter;
 body5: block;
}

and catch_formal_parameter = modifiers option * catch_type * _variable_declarator_id

and catch_type = _unannotated_type * ( _unannotated_type list)

and finally_clause = block

and try_with_resources_statement = {
  resources: resource_specification;
  body6: block;
  extra1: ( catch_clause list) * finally_clause option
}

and resource_specification = (resource * ( resource list)) option

and resource = {
  mods1: modifiers option;
  t_type2: _unannotated_type;
  id: _variable_declarator_id;
  value6: _expression;
  extra21: interm11;
}

and interm11 = 
| Ambiguous of _ambiguous_name
| Field1 of field_access

and if_statement = {
  condition4: parenthesized_expression;
  consequence: _statement;
  alternative1: _statement option;
}

and while_statement = {
  condition5: parenthesized_expression;
  body7: _statement;
}

and for_statement = {
  init: interm12;
  condition6: _expression;
  update: (_expression list) option;
  body8: _statement;
}

and interm12 = 
| Local of local_variable_declaration
| Expression5 of (_expression list) option

and enhanced_for_statement = {
  mods2: modifiers option;
  t_type3: _unannotated_type;
  extra2: _variable_declarator_id;
  value1: _expression;
  body9: _statement;
}

and _annotation =
| Marker of marker_annotation
| Annotation1 of annotation

and marker_annotation = {
  name4: identifier * scoped_identifier;
}

and annotation = {
  name5: identifier * scoped_identifier;
  arguments3: annotation_argument_list;
}

and annotation_argument_list = 
|  Element1 of _element_value
|  Element11 of (element_value_pair * ( element_value_pair list)) option

and element_value_pair = {
  key: identifier;
  value2: _element_value;
}

and _element_value =
| Expression of _expression
| Element of element_value_array_initializer
| Annotaiton of _annotation

and element_value_array_initializer = (_element_value * ( _element_value list)) option

and _declaration =
| Module1 of module_declaration
| Package of package_declaration
| Import of import_declaration
| Class1 of class_declaration
| Interface1 of interface_declaration
| Annotation2 of annotation_type_declaration
| Enum2 of enum_declaration


and module_declaration = ( _annotation list) option *  _ambiguous_name * ( module_directive list)

and module_directive = ( requires_modifier list) * module_name * _ambiguous_name option * ( module_name list) option * _ambiguous_name option * ( module_name list) option * ( _ambiguous_name list)


and requires_modifier = string wrap


and module_name =
| Identifier2 of identifier
| Module of module_name * identifier


and package_declaration = ( _annotation list) * _ambiguous_name

and import_declaration = identifier list

and asterisk = string wrap

and enum_declaration = {
  mods3: modifiers option;
  name6: identifier;
  interfaces1: super_interfaces;
  body10: enum_body;
}

and enum_body = (enum_constant * ( enum_constant list)) option * enum_body_declarations option

and enum_body_declarations = ( _class_body_declaration list)

and enum_constant = {
  mods4: modifiers option;
  name7: identifier;
  arguments4: argument_list option;
  body11: class_body option;
}

and class_declaration = {
  mods5: modifiers option;
  name8: identifier;
  type_parameters2: type_parameters option;
  superclass: superclass option;
  interfaces: super_interfaces option;
  body12: class_body;
}

and modifiers = string wrap

and type_parameters = type_parameter * ( type_parameter list)

and type_parameter = ( _annotation list) * identifier * type_bound option

and type_bound = _type * ( _type list)

and superclass = _type

and super_interfaces = interface_type_list

and interface_type_list = _type * ( _type list)

and class_body = ( _class_body_declaration list)

and _class_body_declaration =
| Class2 of _class_member_declaration
| Block2 of block
| Static of static_initializer
| Constructor of constructor_declaration

and static_initializer = block

and constructor_declaration = {
  mods6: modifiers option; 
  extra31: _constructor_declarator;
  throw: throws option;
  body13: constructor_body;
}

and _constructor_declarator = {
  type_parameters3: type_parameters;
  name9: identifier;
  parameters4: formal_parameters;
}

and constructor_body = explicit_constructor_invocation option * ( _statement list)

and explicit_constructor_invocation = 
| Interm13 of interm13
| Interm14 of interm14

and interm13 = {
  type_arguments3: type_arguments;
  constructor1: this * super;
}

and interm14 = {
  t_object1: _ambiguous_name * _primary;
  type_arguments4: type_arguments;
  constructor: super;
  arguments5: argument_list;
}

and _ambiguous_name =
| Identifier of identifier
| Reserved of _reserved_identifier
| Scoped1 of scoped_identifier

and scoped_identifier = {
  scope: identifier * _reserved_identifier * scoped_identifier;
  name10: identifier;
}

and _class_member_declaration =
| Field of field_declaration
| Method3 of method_declaration
| Class3 of class_declaration
| Interface2 of interface_declaration
| Annotation3 of annotation_type_declaration
| Enum3 of enum_declaration


and field_declaration = {
  mods7: modifiers option; 
  t_type4: _unannotated_type;
  extra4: _variable_declarator_list;
}

and annotation_type_declaration = {
  mods8: modifiers option; 
  name11: identifier;
  body14: annotation_type_body;
}

and annotation_type_body = ( _annotation_type_member_declaration list)

and _annotation_type_member_declaration =
| Annotation4 of annotation_type_element_declaration
| Constant1 of constant_declaration
| Class4 of class_declaration
| Interface3 of interface_declaration
| Annotation11 of annotation_type_declaration


and annotation_type_element_declaration = {
  mods9: modifiers option; 
  t_type5: _unannotated_type;
  name12: identifier;
  dimensions1: dimensions;
  extra: _default_value option;
}

and _default_value = {
  value: _element_value;
}

and interface_declaration = {
  mods10: modifiers option; 
  name13: identifier;
  type_parameters5: type_parameters;
  extra5: extends_interfaces option;
  body15: interface_body;
}

and extends_interfaces = interface_type_list

and interface_body = ( _interface_member_declaration list)

and _interface_member_declaration =
| Constant of constant_declaration
| Enum4 of enum_declaration
| Method4 of method_declaration
| Class5 of class_declaration
| Interface of interface_declaration
| Annotation5 of annotation_type_declaration


and constant_declaration = {
  mods11: modifiers option;
  t_type6: _unannotated_type;
  extra51: _variable_declarator_list;
}

and _variable_declarator_list = {
  declarator: variable_declarator list;
}

and variable_declarator = {
  extra41: _variable_declarator_id;
  value3: _variable_initializer option;
}

and _variable_declarator_id = {
  name14: identifier * _reserved_identifier;
  dimensions2: dimensions;
}

and _variable_initializer =
| Expression1 of _expression
| Array1 of array_initializer


and array_initializer = (_variable_initializer * ( _variable_initializer list)) option

and _type =
| Unannotated of _unannotated_type
| Annotated of annotated_type


and _unannotated_type =
| Simple of _simple_type
| Array2 of array_type

and _simple_type =
| Void of void_type
| Numeric of _numeric_type
| Boolean of boolean_type
| Scoped2 of scoped_type_identifier
| Generic1 of generic_type


and annotated_type = ( _annotation list) * _unannotated_type

and scoped_type_identifier = 
| Scoped of scoped_type_identifier
| Generic of generic_type * ( _annotation list)

and generic_type = string wrap

and array_type = {
  element: _unannotated_type;
  dimensions3: dimensions;
}

and _numeric_type =
| Integral of integral_type
| Floating of floating_point_type

and integral_type = string

and floating_point_type = string

and boolean_type = string

and void_type = string

and _method_header = {
  type_parameters6: type_parameters * ( _annotation list) option;
  t_type7: _unannotated_type;
  extra6: _method_declarator;
  throw1: throws option;
}

and _method_declarator = {
  name15: identifier * _reserved_identifier;
  parameters: formal_parameters;
  dimensions4: dimensions;
}

and formal_parameters = receiver_parameter option * (formal_parameter * ( formal_parameter list)) option * spread_parameter option

and formal_parameter = {
  mods12: modifiers option;
  t_type8: _unannotated_type;
  extra7: _variable_declarator_id;
}

and receiver_parameter = ( _annotation list) * _unannotated_type * identifier option * this

and spread_parameter = modifiers option * _unannotated_type * variable_declarator

and throws =  _type * ( _type list)

and local_variable_declaration = {
  mods13: modifiers option;
  t_type9: _unannotated_type;
  extra8: _variable_declarator_list;
}

and method_declaration = {
  mods14: modifiers option;
  extra9: _method_header;
  body1: block;
}

and _reserved_identifier = identifier

and this = string 

and super = string 

and comment = string 

and identifier = string 

and program = compilation_unit

and compilation_unit = {
  package: identifier option;
  imports: identifier list;
  decls: decls;
}

and decl =
  | Class6 of class_declaration
  | Enum1 of enum_declaration
  | Method1 of method_declaration
  | Field11 of field_declaration

(* FOR TESTING ONLY *)
and decls = class_declaration

and any = 
  | TProgram of program