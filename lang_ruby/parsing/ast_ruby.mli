
type pos = Lexing.position

type string_contents = 
  | StrChars of string
  | StrExpr of expr

and interp_string = string_contents list

and formal_param =
    Formal_id of expr
  | Formal_amp of string
  | Formal_star of string
  | Formal_rest
  | Formal_tuple of formal_param list
  | Formal_default of string * expr

and string_kind =
    String_Single of string
  | String_Double of interp_string
  | String_Tick of interp_string

and inheritance_kind = Class_Inherit of expr | Inst_Inherit of expr

and body_exn = {
  body_exprs : expr list;
  rescue_exprs : (expr * expr) list;
  ensure_expr : expr list;
  else_expr : expr list;
}

and case_block = {
  case_guard : expr;
  case_whens: (expr list * expr list) list;
  case_else: expr list;
}

and expr =
  | E_Empty
  | E_Literal of lit_kind * pos
  | E_Alias of expr * expr* pos
  | E_Undef of expr list * pos
  | E_Identifier of id_kind * string * pos
  | E_Unary of unary_op * expr * pos
  | E_Binop of expr * binary_op * expr * pos
  | E_Ternary of expr * expr * expr * pos
  | E_Hash of bool (* parsed with braces*) * expr list * pos
  | E_Array of expr list * pos
  | E_Tuple of expr list * pos
  | E_MethodCall of expr * expr list * expr option * pos
  | E_While of bool * expr * expr list * pos
  | E_Until of bool * expr * expr list * pos
  | E_Unless of expr * expr list * expr list * pos
  | E_For of formal_param list * expr * expr list * pos
  | E_If of expr * expr list * expr list * pos
  | E_ModuleDef of expr * body_exn * pos
  | E_MethodDef of expr * formal_param list * body_exn * pos
  | E_CodeBlock of bool * formal_param list option * expr list * pos
  | E_ClassDef of expr * inheritance_kind option * body_exn * pos
  | E_BeginBlock of expr list * pos
  | E_EndBlock of expr list * pos
  | E_ExnBlock of body_exn * pos
  | E_Case of case_block * pos
  | E_Operator of binary_op * pos
  | E_UOperator of unary_op * pos
  | E_Return of expr list * pos
  | E_Yield of expr list * pos
  | E_Block of expr list * pos
  | E_Annotate of expr * Annotation.t * pos

and lit_kind =
    Lit_FixNum of int
  | Lit_BigNum of Big_int.big_int
  | Lit_Float of string * float
  | Lit_String of string_kind
  | Lit_Atom of interp_string
  | Lit_Regexp of interp_string * string
  | Lit_Nil
  | Lit_Self
  | Lit_True
  | Lit_False

and id_kind =
    ID_Lowercase
  | ID_Instance
  | ID_Class
  | ID_Global
  | ID_Uppercase
  | ID_Builtin
  | ID_Assign of id_kind

and unary_op =
    Op_UMinus
  | Op_UPlus
  | Op_UBang
  | Op_UTilde
  | Op_UNot
(*  | Op_UDefined*)
  | Op_UAmper
  | Op_UStar
  | Op_UScope

and binary_op =
    Op_ASSIGN
  | Op_PLUS
  | Op_MINUS
  | Op_TIMES
  | Op_REM
  | Op_DIV
  | Op_CMP
  | Op_EQ
  | Op_EQQ
  | Op_NEQ
  | Op_GEQ
  | Op_LEQ
  | Op_LT
  | Op_GT
  | Op_AND
  | Op_OR
  | Op_BAND
  | Op_BOR
  | Op_MATCH
  | Op_NMATCH
  | Op_XOR
  | Op_POW
  | Op_kAND
  | Op_kOR
  | Op_ASSOC
  | Op_DOT
  | Op_SCOPE
  | Op_AREF
  | Op_ASET
  | Op_LSHIFT
  | Op_RSHIFT
(*  | Op_Custom of string*)
  | Op_OP_ASGN of binary_op
  | Op_DOT2
  | Op_DOT3

type ast = expr list

val compare_expr : expr -> expr -> int
val equal_expr : expr -> expr -> bool

val compare_ast : expr list -> expr list -> int
val equal_ast : expr list -> expr list -> bool

val pos_of : expr -> pos

val binary_op_of_string : string -> binary_op

val mod_ast : (expr -> expr) -> expr list -> expr list

val set_pos : pos -> expr -> expr

(* converts the method name or operator in the given string and
   returns an expression suitable for use within a MethodDef *)
val msg_of_str : string -> pos -> expr

val verify_annotation_name : expr -> Annotation.t -> pos -> unit

val str_uop : unary_op -> string
val str_binop : binary_op -> string

module Abbr : sig

  val fixnum : int -> pos -> expr
  val float : float -> pos -> expr
  val ltrue : pos -> expr
  val lfalse : pos -> expr
  val lself : pos -> expr
  val lnil : pos -> expr
  val ident : string -> pos -> expr
  val str : string_kind -> pos -> expr
  val single_str : string -> pos -> expr
  val double_str : string -> pos -> expr
  val tick_str : string -> pos -> expr
  val regexp : string -> string -> pos -> expr
  val atom : string -> pos -> expr
  val scoped_ident : string list -> pos -> expr
  val dp : Lexing.position

  val mcall : expr -> expr list -> ?cb:expr -> pos -> expr
  val cb : ?args:(formal_param list) -> ast -> pos -> expr

end
