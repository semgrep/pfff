
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

type _literal =
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
| Binary of binary_expression
| Instanceof of instanceof_expression
| Lambda of lambda_expression
| Ternary of ternary_expression
| Update of update_expression
| Ambiguous of _ambiguous_name
| Primary of _primary
| Unary of unary_expression
| Cast of cast_expression


and cast_expression = {
  t_type: _type;
  t_type1: _type list;
  value: _expression;
}

and assignment_expression = {
  left: _ambiguous_name * field_access * array_access;
  operator: string wrap;
  right: _expression;
}

and binary_expression = string wrap


and instanceof_expression = {
  left: _expression;
  right: _type;
}

and lambda_expression = {
  body: interm15 option;
  parameters: interm16 option;
}

and interm15 =
| Expression of _expression
| Block of block

and interm16 =
| Formal of formal_parameters
| Identifier of identifier
| Inferred of inferred_parameters


and inferred_parameters = identifier * ( identifier list)

and ternary_expression = {
  condition: _expression;
  consequence: _expression;
  alternative: _expression;
}

and unary_expression = string wrap

and update_expression = string wrap
 
and _primary =
| Literal of _literal
| Class of class_literal
| This of this
| Parenthesized of parenthesized_expression
| Object of object_creation_expression
| Field of field_access
| Array of array_access
| Method of method_invocation
| Method1 of method_reference
| Array1 of array_creation_expression


and array_creation_expression = {
  t_type: _simple_type;
  t_dimensions: interm1;
  value: array_initializer;
}

and interm1 = 
| Dimensions1 of dimensions_expr list
| Dimensions of dimensions option

and dimensions_expr = ( _annotation list) * _expression

and parenthesized_expression = _expression

and class_literal =
| Ambiguous of _ambiguous_name
| Numeric of _numeric_type
| Boolean of boolean_type
| Void of void_type


and object_creation_expression =
| Unqualified of _unqualified_object_creation_expression
| Ambiguous of _ambiguous_name * _unqualified_object_creation_expression
| Primary of _primary * _unqualified_object_creation_expression


and _unqualified_object_creation_expression = {
  type_arguments: type_arguments;
  t_type: _simple_type;
  arguments: argument_list;
  class_body: class_body option;
}

and field_access =
| Interm2 of interm2
| Interm3 of interm3
| Interm4 of interm4

and interm2 = {
  t_object: _ambiguous_name;
  field: this;
}

and interm3 = {
  t_object: _primary * super;
  field: identifier;
}

and interm4 = {
  t_object: _ambiguous_name;
  mods: super;
  field: identifier;
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
  t_object: _ambiguous_name * _primary * super * _ambiguous_name;
  type_arguments: type_arguments;
  name: identifier;
}

and interm6 = {
  t_object: _ambiguous_name;
  supertype_arguments: type_arguments;
  name: identifier;
  arguments: argument_list;
}

and interm7 = {
  name: identifier * _reserved_identifier;
}

and argument_list = (_expression * ( _expression list)) option

and method_reference = interm8 * type_arguments option * identifier option

and interm8 = 
| Type of _type
| Ambiguous of _ambiguous_name
| Primary of _primary
| Super of super

and type_arguments = (interm9 * (_type * wildcard) list) option

and interm9 =
| Type of _type
| Wildcard of wildcard

and wildcard = ( _annotation list) * _wildcard_bounds option

and _wildcard_bounds =
| Type of _type
| Super of super * _type

and dimensions = ( _annotation list) 

and _statement =
| Declaration of _declaration
| Expression of expression_statement
| Labeled of labeled_statement
| If of if_statement
| While of while_statement
| For of for_statement
| Enhanced of enhanced_for_statement
| Block of block
| Assert of assert_statement
| Switch of switch_statement
| Do of do_statement
| Break of break_statement
| Continue of continue_statement
| Return of return_statement
| Synchronized of synchronized_statement
| Local of local_variable_declaration
| Throw of throw_statement
| Try of try_statement
| Try1 of try_with_resources_statement


and block = ( _statement list)

and expression_statement = _expression

and labeled_statement = identifier * _statement

and assert_statement =
| Expression of _expression
| Expression1 of _expression * _expression


and switch_statement = {
  condition: parenthesized_expression;
  body: switch_block;
}

and switch_block = interm10 list

and interm10 = 
| Switch of switch_label
| Statement of _statement 

and switch_label = _expression

and do_statement = {
  body: _statement;
  condition: parenthesized_expression;
}

and break_statement = identifier option

and continue_statement = identifier option

and return_statement = _expression option

and synchronized_statement = {
 paren: parenthesized_expression;
 body: block;
}

and throw_statement = _expression

and try_statement = {
  body: block;
  extra: ( catch_clause list) * finally_clause option
}

and catch_clause = {
 extra: catch_formal_parameter;
 body: block;
}

and catch_formal_parameter = modifiers option * catch_type * _variable_declarator_id

and catch_type = _unannotated_type * ( _unannotated_type list)

and finally_clause = block

and try_with_resources_statement = {
  resources: resource_specification;
  body: block;
  extra: ( catch_clause list) * finally_clause option
}

and resource_specification = (resource * ( resource list)) option

and resource = {
  mods: modifiers option;
  t_type: _unannotated_type;
  id: _variable_declarator_id;
  value: _expression;
  extra: interm11;
}

and interm11 = 
| Ambiguous of _ambiguous_name
| Field of field_access

and if_statement = {
  condition: parenthesized_expression;
  consequence: _statement;
  alternative: _statement option;
}

and while_statement = {
  condition: parenthesized_expression;
  body: _statement;
}

(* GET BACK TO THIS!!!!! *)
and for_statement = {
  init: interm12;
  condition: _expression;
  update: (_expression list) option;
  body: _statement;
}

and interm12 = 
| Local of local_variable_declaration
| Expression of (_expression list) option

and enhanced_for_statement = {
  mods: modifiers option;
  t_type: _unannotated_type;
  extra: _variable_declarator_id;
  value: _expression;
  body: _statement;
}

and _annotation =
| Marker of marker_annotation
| Annotation of annotation

and marker_annotation = {
  name: identifier * scoped_identifier;
}

and annotation = {
  name: identifier * scoped_identifier;
  arguments: annotation_argument_list;
}

and annotation_argument_list = 
|  Element of _element_value
|  Element1 of (element_value_pair * ( element_value_pair list)) option

and element_value_pair = {
  key: identifier;
  value: _element_value;
}

and _element_value =
| Expression of _expression
| Element of element_value_array_initializer
| Annotaiton of _annotation

and element_value_array_initializer = (_element_value * ( _element_value list)) option

and _declaration =
| Module of module_declaration
| Package of package_declaration
| Import of import_declaration
| Class of class_declaration
| Interface of interface_declaration
| Annotation of annotation_type_declaration
| Enum of enum_declaration


and module_declaration = ( _annotation list) option *  _ambiguous_name * ( module_directive list)

and module_directive = ( requires_modifier list) * module_name * _ambiguous_name option * ( module_name list) option * _ambiguous_name option * ( module_name list) option * ( _ambiguous_name list)


and requires_modifier = string wrap


and module_name =
| Identifier of identifier
| Module of module_name * identifier


and package_declaration = ( _annotation list) * _ambiguous_name

and import_declaration = identifier * ( identifier list) * asterisk option

and asterisk = string wrap

and enum_declaration = {
  mods: modifiers option;
  name: identifier;
  interfaces: super_interfaces;
  body: enum_body;
}

and enum_body = (enum_constant * ( enum_constant list)) option * enum_body_declarations option

and enum_body_declarations = ( _class_body_declaration list)

and enum_constant = {
  mods: modifiers option;
  name: identifier;
  arguments: argument_list;
  body: class_body;
}

and class_declaration = {
  mods: modifiers option;
  name: identifier;
  type_parameters: type_parameters option;
  superclass: superclass option;
  interfaces: super_interfaces option;
  body: class_body;
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
| Class of _class_member_declaration
| Block of block
| Static of static_initializer
| Constructor of constructor_declaration

and static_initializer = block

and constructor_declaration = {
  mods: modifiers option; 
  extra: _constructor_declarator;
  throw: throws option;
  body: constructor_body;
}

and _constructor_declarator = {
  type_parameters: type_parameters;
  name: identifier;
  parameters: formal_parameters;
}

and constructor_body = explicit_constructor_invocation option * ( _statement list)

and explicit_constructor_invocation = 
| Interm13 of interm13
| Interm14 of interm14

and interm13 = {
  type_arguments: type_arguments;
  constructor: this * super;
}

and interm14 = {
  t_object: _ambiguous_name * _primary;
  type_arguments: type_arguments;
  constructor: super;
  arguments: argument_list;
}

and _ambiguous_name =
| Identifier of identifier
| Reserved of _reserved_identifier
| Scoped of scoped_identifier

and scoped_identifier = {
  scope: identifier * _reserved_identifier * scoped_identifier;
  name: identifier;
}

and _class_member_declaration =
| Field of field_declaration
| Method of method_declaration
| Class of class_declaration
| Interface of interface_declaration
| Annotation of annotation_type_declaration
| Enum of enum_declaration


and field_declaration = {
  mods: modifiers option; 
  t_type: _unannotated_type;
  extra: _variable_declarator_list;
}

and annotation_type_declaration = {
  mods: modifiers option; 
  name: identifier;
  body: annotation_type_body;
}

and annotation_type_body = ( _annotation_type_member_declaration list)

and _annotation_type_member_declaration =
| Annotation of annotation_type_element_declaration
| Constant of constant_declaration
| Class of class_declaration
| Interface of interface_declaration
| Annotation1 of annotation_type_declaration


and annotation_type_element_declaration = {
  mods: modifiers option; 
  t_type: _unannotated_type;
  name: identifier;
  dimensions: dimensions;
  extra: _default_value option;
}

and _default_value = {
  value: _element_value;
}

and interface_declaration = {
  mods: modifiers option; 
  name: identifier;
  type_parameters: type_parameters;
  extra: extends_interfaces option;
  body: interface_body;
}

and extends_interfaces = interface_type_list

and interface_body = ( _interface_member_declaration list)

and _interface_member_declaration =
| Constant of constant_declaration
| Enum of enum_declaration
| Method of method_declaration
| Class of class_declaration
| Interface of interface_declaration
| Annotation of annotation_type_declaration


and constant_declaration = {
  mods: modifiers option;
  t_type: _unannotated_type;
  extra: _variable_declarator_list;
}

and _variable_declarator_list = {
  declarator: variable_declarator list;
}

and variable_declarator = {
  extra: _variable_declarator_id;
  value: _variable_initializer option;
}

and _variable_declarator_id = {
  name: identifier * _reserved_identifier;
  dimensions: dimensions;
}

and _variable_initializer =
| Expression of _expression
| Array of array_initializer


and array_initializer = (_variable_initializer * ( _variable_initializer list)) option

and _type =
| Unannotated of _unannotated_type
| Annotated of annotated_type


and _unannotated_type =
| Simple of _simple_type
| Array of array_type

and _simple_type =
| Void of void_type
| Numeric of _numeric_type
| Boolean of boolean_type
| Scoped of scoped_type_identifier
| Generic of generic_type


and annotated_type = ( _annotation list) * _unannotated_type

and scoped_type_identifier = 
| Scoped of scoped_type_identifier
| Generic of generic_type * ( _annotation list)

and generic_type = string wrap

and array_type = {
  element: _unannotated_type;
  dimensions: dimensions;
}

and _numeric_type =
| Integral of integral_type
| Floating of floating_point_type

and integral_type = string wrap

and floating_point_type = string wrap

and boolean_type = string wrap

and void_type = string wrap

and _method_header = {
  type_parameters: type_parameters * ( _annotation list) option;
  t_type: _unannotated_type;
  extra: _method_declarator;
  throw: throws option;
}

and _method_declarator = {
  name: identifier * _reserved_identifier;
  parameters: formal_parameters;
  dimensions: dimensions;
}

and formal_parameters = receiver_parameter option * (formal_parameter * ( formal_parameter list)) option * spread_parameter option

and formal_parameter = {
  mods: modifiers option;
  t_type: _unannotated_type;
  extra: _variable_declarator_id;
}

and receiver_parameter = ( _annotation list) * _unannotated_type * identifier option * this

and spread_parameter = modifiers option * _unannotated_type * variable_declarator

and throws =  _type * ( _type list)

and local_variable_declaration = {
  mods: modifiers option;
  t_type: _unannotated_type;
  extra: _variable_declarator_list;
}

and method_declaration = {
  mods: modifiers option;
  extra: _method_header;
  body: block;
}

and _reserved_identifier = identifier

and this = string wrap

and super = string wrap

and comment = string wrap

and identifier = string wrap