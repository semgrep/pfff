(* TODO: pad:
* [AST Format of the Whitequark parser](https://github.com/whitequark/parser/blob/master/doc/AST_FORMAT.md)
 * https://rubygems.org/gems/ast
 *)

type pos = Lexing.position

type string_contents = 
  | StrChars of string
  | StrExpr of expr

and interp_string = string_contents list

and formal_param = 
  | Formal_id of expr
  | Formal_amp of string
  | Formal_star of string (* as in *x *)
  | Formal_rest (* just '*' *)
  | Formal_tuple of formal_param list
  | Formal_default of string * expr

and string_kind = 
  | String_Single of string
  | String_Double of interp_string
  | String_Tick of interp_string

and inheritance_kind = 
  | Class_Inherit of expr
  | Inst_Inherit of expr

and body_exn = {
  body_exprs: expr list;
  rescue_exprs: (expr*expr) list;
  ensure_expr: expr list;
  else_expr: expr list;
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
  | E_Hash of bool * expr list * pos
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
  | Lit_FixNum of int
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
  | ID_Lowercase (* prefixed by [a-z] or _ *)
  | ID_Instance  (* prefixed by @ *)
  | ID_Class     (* prefixed by @@ *)
  | ID_Global    (* prefixed by $ *)
  | ID_Uppercase (* prefixed by [A-Z] *)
  | ID_Builtin   (* prefixed by $, followed by non-alpha *)
  | ID_Assign of id_kind (* postfixed by = *)

and unary_op = 
  | Op_UMinus    (* -x *)
  | Op_UPlus     (* +x *)
  | Op_UBang     (* !x *)
  | Op_UTilde    (* ~x *)
  | Op_UNot      (* not x *)
(*  | Op_UDefined*)  (* defined? x *)
  | Op_UAmper    (* & *)
  | Op_UStar     (* * *)
  | Op_UScope    (* ::x *)


and binary_op = 
  | Op_ASSIGN   (* = *)
  | Op_PLUS     (* + *)
  | Op_MINUS    (* - *)
  | Op_TIMES    (* * *)
  | Op_REM      (* % *)
  | Op_DIV      (* / *)
  | Op_CMP  	(* <=> *)
  | Op_EQ  	(* == *)
  | Op_EQQ  	(* === *)
  | Op_NEQ  	(* != *)
  | Op_GEQ  	(* >= *)
  | Op_LEQ  	(* <= *)
  | Op_LT       (* < *)
  | Op_GT       (* > *)
  | Op_AND      (* && *)
  | Op_OR	(* || *)
  | Op_BAND     (* & *)
  | Op_BOR      (* | *)
  | Op_MATCH    (* =~ *)
  | Op_NMATCH   (* !~ *)
  | Op_XOR      (* ^ *)
  | Op_POW      (* ** *)
  | Op_kAND     (* and *)
  | Op_kOR      (* or *)
  | Op_ASSOC    (* => *)
  | Op_DOT      (* . *)
  | Op_SCOPE    (* :: *)
  | Op_AREF     (* [] *)
  | Op_ASET     (* []= *)
  | Op_LSHIFT   (* < < *)
  | Op_RSHIFT   (* > > *)
(*  | Op_Custom of string*)   (* ?? *)
  | Op_OP_ASGN of binary_op  (* +=, -=, ... *)
  | Op_DOT2     (* .. *)
  | Op_DOT3     (* ... *)


type ast = expr list


