
type t =
    ClassType of class_annotation
  | MethodType of method_annotation list
  | ExprType of type_expr

and class_annotation = string * bounded_quantifier list * type_expr list

and method_annotation = type_ident * bounded_quantifier list * method_type

and method_type =
    MethodSig of type_expr list * method_type option * type_expr

and type_expr =
    Type_Var of quant_var
  | Type_Ident of type_ident
  | Type_Object of object_type
  | Type_Union of type_expr list
  | Type_App of type_ident * type_expr list
  | Type_Tuple of type_expr list
  | Type_Dynamic
  | Type_Fixme
  | Type_Optional of type_expr
  | Type_Varargs of type_expr
  | Type_ParamList of type_expr list

and type_ident =
    TIdent_Relative of string
  | TIdent_Absolute of string
  | TIdent_Scoped of type_ident * string

and object_type = field_type list * method_annotation list

and field_type = string * type_expr

and bounded_quantifier = quant_var * type_expr option

and quant_var = QSelf | QVar of string | QParam of string

(* pretty printers *)
val format_quant_var : Format.formatter -> quant_var -> unit
val format_type_ident : Format.formatter -> type_ident -> unit
val format_type_expr : Format.formatter -> type_expr -> unit
val format_field_type : Format.formatter -> field_type -> unit
val format_method_type : Format.formatter -> method_type -> unit
val format_type_block : Format.formatter -> method_type option -> unit
val format_method_annotation : Format.formatter -> method_annotation -> unit
val format_bounded_quantifier : Format.formatter -> bounded_quantifier -> unit
val format_class_annotation : Format.formatter -> class_annotation -> unit
val format_annotation : Format.formatter -> t -> unit

val string_of_quant_var : quant_var -> string
val string_of_annotation : t -> string


val compare_class_annot : class_annotation -> class_annotation -> int
val compare_method_annot : method_annotation list -> method_annotation list -> int
val compare_expr_annot : type_expr -> type_expr -> int
val compare_annotation : t -> t -> int
val equal_annotation : t -> t -> bool
