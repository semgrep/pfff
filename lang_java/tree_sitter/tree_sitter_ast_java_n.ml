
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

and _declaration =
| Annotation of annotation_type_declaration
| Class of class_declaration
| Enum of enum_declaration
| Import of import_declaration
| Interface of interface_declaration
| Module of module_declaration
| Package of package_declaration

and _expression =
| Primary of _primary
| Assignment of assignment_expression
| Binary of binary_expression
| Cast of cast_expression
| Identifier of identifier
| Instanceof of instanceof_expression
| Lambda of lambda_expression
| Scoped of scoped_identifier
| Ternary of ternary_expression
| Unary of unary_expression
| Update of update_expression

and _literal =
| Binary1 of binary_integer_literal
| Character of character_literal
| Decimal of decimal_floating_point_literal
| Decimal1 of decimal_integer_literal
| Bool of string wrap
| Hex of hex_floating_point_literal
| Hex1 of hex_integer_literal
| Null of null_literal
| Octal of octal_integer_literal
| String of string_literal

and _primary =
| Literal of _literal
| Array of array_access
| Array1 of array_creation_expression
| Class1 of class_literal
| Field of field_access
| Method of method_invocation
| Method1 of method_reference
| Object of object_creation_expression
| Parenthesized of parenthesized_expression
| This of this

and _simple_type =
| Boolean of boolean_type
| Floating of floating_point_type
| Generic of generic_type
| Integral of integral_type
| Scoped1 of scoped_type_identifier
| Type of type_identifier
| Void of void_type

and _statement =
| Declaration of _declaration
| Assert of assert_statement
| Block of block
| Break of break_statement
| Continue of continue_statement
| Do of do_statement
| Enhanced of enhanced_for_statement
| Expression of expression_statement
| For of for_statement
| If of if_statement
| Labeled of labeled_statement
| Local of local_variable_declaration
| Return of return_statement
| Switch of switch_statement
| Synchronized of synchronized_statement
| Throw of throw_statement
| Try of try_statement
| Try1 of try_with_resources_statement
| While of while_statement

and _type =
| Unannotated of _unannotated_type
| Annotated of annotated_type

and _unannotated_type =
| Simple of _simple_type
| Array2 of array_type

and annotated_type =( _unannotated_type list option * annotation list option * marker_annotation list option)

and annotation = {
  t_arguments: annotation_argument_list option;
  t_name: interm1 option;
}

and interm1 =
| Identifier of identifier
| Scoped of scoped_identifier

and annotation_argument_list =( _expression list * annotation list * element_value_array_initializer list * element_value_pair list * marker_annotation list)

and annotation_type_body =( annotation_type_declaration list * annotation_type_element_declaration list * class_declaration list * constant_declaration list * interface_declaration list)

and annotation_type_declaration = {
  t_body: annotation_type_body option;
  t_name: identifier option;
  mods: mods0;
}
  
and mods0 =
| Modifiers of modifiers

and annotation_type_element_declaration = {
  t_dimensions: dimensions;
  t_name: identifier option;
  t_type: _unannotated_type option;
  t_value: interm2;
  mods: mods1;
}
  
and mods1 =
| Modifiers1 of modifiers

and interm2 =
| Expression of _expression
| Annotation of annotation
| Element of element_value_array_initializer
| Marker of marker_annotation

and argument_list =( _expression list)

and array_access = {
  t_array: interm3 option;
  t_index: _expression option;
}

and interm3 =
| Primary of _primary
| Identifier of identifier
| Scoped of scoped_identifier

and array_creation_expression = {
  t_dimensions: dimensions list option * dimensions_expr list option;
  t_type: _simple_type option;
  t_value: array_initializer;
}

and array_initializer =( _expression list * array_initializer list)

and array_type = {
  t_dimensions: dimensions option;
  t_element: _unannotated_type option;
}

and assert_statement =( _expression list option)

and assignment_expression = {
  t_left: interm4 option;
  operator: string wrap;
  t_right: _expression option;
}

and interm4 =
| Array of array_access
| Field of field_access
| Identifier of identifier
| Scoped of scoped_identifier

and asterisk = string wrap

and binary_expression = {
  t_left: _expression option;
  operator: string wrap;
  t_right: _expression option;
}

and block =( _statement list)

and break_statement =
| Identifier1 of identifier

and cast_expression = {
  t_type: _type list option;
  t_value: _expression option;
}

and catch_clause = {
  t_body: block option;
  mods: mods2 option;
}
  
and mods2 =
| Catch of catch_formal_parameter

and catch_formal_parameter = {
  t_dimensions: dimensions;
  t_name: identifier option;
  mods: mods3 list option;
}
  
and mods3 =
| Catch1 of catch_type
| Modifiers2 of modifiers

and catch_type =( _unannotated_type list option)

and class_body =( annotation_type_declaration list * block list * class_declaration list * constructor_declaration list * enum_declaration list * field_declaration list * interface_declaration list * method_declaration list * static_initializer list)

and class_declaration = {
  t_body: class_body option;
  t_interfaces: super_interfaces;
  t_name: identifier option;
  t_superclass: superclass;
  t_type_parameters: type_parameters;
  mods: mods4;
}
  
and mods4 =
| Modifiers3 of modifiers

and class_literal =
| Boolean1 of boolean_type option
| Floating1 of floating_point_type option
| Identifier2 of identifier option
| Integral1 of integral_type option
| Scoped2 of scoped_identifier option
| Void1 of void_type option

and constant_declaration = {
  t_declarator: variable_declarator list option;
  t_type: _unannotated_type option;
  mods: mods5;
}
  
and mods5 =
| Modifiers4 of modifiers

and constructor_body =( _statement list * explicit_constructor_invocation list)

and constructor_declaration = {
  t_body: constructor_body option;
  t_name: identifier option;
  t_parameters: formal_parameters option;
  t_type_parameters: type_parameters;
  mods: mods6 list;
}
  
and mods6 =
| Modifiers5 of modifiers
| Throws of throws

and continue_statement =
| Identifier3 of identifier

and dimensions =( annotation list * marker_annotation list)

and dimensions_expr =( _expression list option * annotation list option * marker_annotation list option)

and do_statement = {
  t_body: _statement option;
  t_condition: parenthesized_expression option;
}

and element_value_array_initializer =( _expression list * annotation list * element_value_array_initializer list * marker_annotation list)

and element_value_pair = {
  t_key: identifier option;
  t_value: interm5 option;
}

and interm5 =
| Expression of _expression
| Annotation of annotation
| Element of element_value_array_initializer
| Marker of marker_annotation

and enhanced_for_statement = {
  t_body: _statement option;
  t_dimensions: dimensions;
  t_name: identifier option;
  t_type: _unannotated_type option;
  t_value: _expression option;
  mods: mods7;
}
  
and mods7 =
| Modifiers6 of modifiers

and enum_body =( enum_body_declarations list * enum_constant list)

and enum_body_declarations =( annotation_type_declaration list * block list * class_declaration list * constructor_declaration list * enum_declaration list * field_declaration list * interface_declaration list * method_declaration list * static_initializer list)

and enum_constant = {
  t_arguments: argument_list;
  t_body: class_body;
  t_name: identifier option;
  mods: mods8;
}
  
and mods8 =
| Modifiers7 of modifiers

and enum_declaration = {
  t_body: enum_body option;
  t_interfaces: super_interfaces;
  t_name: identifier option;
  mods: mods9;
}
  
and mods9 =
| Modifiers8 of modifiers

and explicit_constructor_invocation = {
  t_arguments: argument_list option;
  t_constructor: interm6 option;
  t_object: interm7;
  t_type_arguments: type_arguments;
}

and interm6 =
| Super of super
| This of this

and interm7 =
| Primary of _primary
| Identifier of identifier
| Scoped of scoped_identifier

and expression_statement =
| Expression1 of _expression option

and extends_interfaces =
| Interface1 of interface_type_list option

and field_access = {
  t_field: interm8 option;
  t_object: interm9 option;
  mods: mods10;
}
  
and mods10 =
| Super of super

and interm8 =
| Identifier of identifier
| This of this

and interm9 =
| Primary of _primary
| Identifier of identifier
| Scoped of scoped_identifier
| Super of super

and field_declaration = {
  t_declarator: variable_declarator list option;
  t_type: _unannotated_type option;
  mods: mods11;
}
  
and mods11 =
| Modifiers9 of modifiers

and finally_clause =
| Block1 of block option

and floating_point_type = string wrap

and for_statement = {
  t_body: _statement option;
  t_condition: _expression;
  t_init: _expression list * local_variable_declaration list;
  t_update: _expression list;
}

and formal_parameter = {
  t_dimensions: dimensions;
  t_name: identifier option;
  t_type: _unannotated_type option;
  mods: mods12;
}
  
and mods12 =
| Modifiers10 of modifiers

and formal_parameters =( formal_parameter list * receiver_parameter list * spread_parameter list)

and generic_type =( scoped_type_identifier list option * type_arguments list option * type_identifier list option)

and if_statement = {
  t_alternative: _statement;
  t_condition: parenthesized_expression option;
  t_consequence: _statement option;
}

and import_declaration =( asterisk list option * identifier list option)

and inferred_parameters =( identifier list option)

and instanceof_expression = {
  t_left: _expression option;
  t_right: _type option;
}

and integral_type = string wrap

and interface_body =( annotation_type_declaration list * class_declaration list * constant_declaration list * enum_declaration list * interface_declaration list * method_declaration list)

and interface_declaration = {
  t_body: interface_body option;
  t_name: identifier option;
  t_type_parameters: type_parameters;
  mods: mods13 list;
}
  
and mods13 =
| Extends of extends_interfaces
| Modifiers11 of modifiers

and interface_type_list =( _type list option)

and labeled_statement =( _statement list option * identifier list option)

and lambda_expression = {
  t_body: interm10 option;
  t_parameters: interm11 option;
}

and interm10 =
| Expression of _expression
| Block of block

and interm11 =
| Formal of formal_parameters
| Identifier of identifier
| Inferred of inferred_parameters

and local_variable_declaration = {
  t_declarator: variable_declarator list option;
  t_type: _unannotated_type option;
  mods: mods14;
}
  
and mods14 =
| Modifiers12 of modifiers

and marker_annotation = {
  t_name: interm12 option;
}

and interm12 =
| Identifier of identifier
| Scoped of scoped_identifier

and method_declaration = {
  t_body: block;
  t_dimensions: dimensions;
  t_name: identifier option;
  t_parameters: formal_parameters option;
  t_type: _unannotated_type option;
  t_type_parameters: type_parameters;
  mods: mods15 list;
}
  
and mods15 =
| Annotation1 of annotation
| Marker of marker_annotation
| Modifiers13 of modifiers
| Throws1 of throws

and method_invocation = {
  t_arguments: argument_list option;
  t_name: identifier option;
  t_object: interm13;
  t_type_arguments: type_arguments;
  mods: mods16;
}
  
and mods16 =
| Super1 of super

and interm13 =
| Primary of _primary
| Identifier of identifier
| Scoped of scoped_identifier
| Super of super

and method_reference =( _primary list option * _type list option * identifier list option * scoped_identifier list option * super list option * type_arguments list option)

and modifiers =( annotation list * marker_annotation list)

and module_declaration =( annotation list option * identifier list option * marker_annotation list option * module_directive list option * scoped_identifier list option)

and module_directive =( identifier list option * module_name list option * requires_modifier list option * scoped_identifier list option)

and module_name =( identifier list option * module_name list option)

and object_creation_expression = {
  t_arguments: argument_list option;
  t_type: _simple_type option;
  t_type_arguments: type_arguments;
  mods: mods17 list;
}
  
and mods17 =
| Primary1 of _primary
| Class2 of class_body
| Identifier4 of identifier
| Scoped3 of scoped_identifier

and package_declaration =( annotation list option * identifier list option * marker_annotation list option * scoped_identifier list option)

and parenthesized_expression =
| Expression2 of _expression option

and program =( _statement list)

and receiver_parameter =( _unannotated_type list option * annotation list option * identifier list option * marker_annotation list option * this list option)

and requires_modifier = string wrap

and resource = {
  t_dimensions: dimensions;
  t_name: identifier;
  t_type: _unannotated_type;
  t_value: _expression;
  mods: mods18;
}
  
and mods18 =
| Field1 of field_access
| Identifier5 of identifier
| Modifiers14 of modifiers
| Scoped4 of scoped_identifier

and resource_specification =( resource list option)

and return_statement =
| Expression3 of _expression

and scoped_identifier = {
  t_name: identifier option;
  t_scope: interm14 option;
}

and interm14 =
| Identifier of identifier
| Scoped of scoped_identifier

and scoped_type_identifier =( annotation list option * generic_type list option * marker_annotation list option * scoped_type_identifier list option * type_identifier list option)

and spread_parameter =( _unannotated_type list option * modifiers list option * variable_declarator list option)

and static_initializer =
| Block2 of block option

and super_interfaces =
| Interface2 of interface_type_list option

and superclass =
| Type1 of _type option

and switch_block =( _statement list * switch_label list)

and switch_label =
| Expression4 of _expression

and switch_statement = {
  t_body: switch_block option;
  t_condition: parenthesized_expression option;
}

and synchronized_statement = {
  t_body: block option;
  mods: mods19 option;
}
  
and mods19 =
| Parenthesized1 of parenthesized_expression

and ternary_expression = {
  t_alternative: _expression option;
  t_condition: _expression option;
  t_consequence: _expression option;
}

and throw_statement =
| Expression5 of _expression option

and throws =( _type list option)

and try_statement = {
  t_body: block option;
  mods: mods20 list option;
}
  
and mods20 =
| Catch2 of catch_clause
| Finally of finally_clause

and try_with_resources_statement = {
  t_body: block option;
  t_resources: resource_specification option;
  mods: mods21 list;
}
  
and mods21 =
| Catch3 of catch_clause
| Finally1 of finally_clause

and type_arguments =( _type list * wildcard list)

and type_bound =( _type list option)

and type_parameter =( annotation list option * identifier list option * marker_annotation list option * type_bound list option)

and type_parameters =( type_parameter list option)

and unary_expression = {
  t_operand: _expression option;
  operator: string wrap;
}

and update_expression =
| Expression6 of _expression option

and variable_declarator = {
  t_dimensions: dimensions;
  t_name: identifier option;
  t_value: interm15;
}

and interm15 =
| Expression of _expression
| Array of array_initializer

and while_statement = {
  t_body: _statement option;
  t_condition: parenthesized_expression option;
}

and wildcard =( _type list * annotation list * marker_annotation list * super list)

and binary_integer_literal = string wrap

and boolean_type = string wrap

and character_literal = string wrap

and decimal_floating_point_literal = string wrap

and decimal_integer_literal = string wrap

and hex_floating_point_literal = string wrap

and hex_integer_literal = string wrap

and identifier = string wrap

and null_literal = string wrap

and octal_integer_literal = string wrap

and string_literal = string wrap

and super = string wrap

and this = string wrap

and type_identifier = string wrap

and void_type = string wrap
