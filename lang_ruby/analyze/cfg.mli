
open Utils

type pos = Lexing.position

type unary_op =
    Op_UMinus
  | Op_UPlus
  | Op_UTilde

type binary_op =
  | Op_Plus
  | Op_Minus
  | Op_Times
  | Op_Rem
  | Op_Div
  | Op_CMP
  | Op_EQ
  | Op_EQQ
  | Op_GEQ
  | Op_LEQ
  | Op_LT
  | Op_GT
  | Op_BAnd
  | Op_BOr
  | Op_Match
  | Op_XOR
  | Op_Pow
  | Op_ARef
  | Op_ASet
  | Op_LShift
  | Op_RShift

type var_kind = [
    `Var_Local
  | `Var_Instance
  | `Var_Class
  | `Var_Global
  | `Var_Constant
  | `Var_Builtin
]

type identifier = [
  | `ID_Var of var_kind * string
  | `ID_Self
  | `ID_Scope of identifier * string
  | `ID_UScope of string
  | `ID_Nil
  | `ID_True
  | `ID_False
]

type msg_id = [
  | `ID_UOperator of unary_op
  | `ID_Operator of binary_op
  | `ID_MethodName of string
  | `ID_Assign of string
  | `ID_Super
]

(* convenience alias that is a subtype of identifier *)
type builtin_or_global = [`ID_Var of [`Var_Builtin|`Var_Global] * string]

type alias_kind = 
  | Alias_Method of msg_id * msg_id
  | Alias_Global of builtin_or_global * builtin_or_global

(* As implied by their name, the type variables below will be filled
   in with the types "expr" and "star_expr" respectively. *)
type ('expr,'star_expr) literal_ = [
    `Lit_FixNum of int
  | `Lit_BigNum of Big_int.big_int
  | `Lit_Float of string * float
  | `Lit_String of string
  | `Lit_Atom of string
  | `Lit_Regexp of string * string
  | `Lit_Array of 'star_expr list
  | `Lit_Hash of ('expr * 'expr) list
  | `Lit_Range of bool * 'expr * 'expr
  ]

type 'a star = [ `Star of 'a]
type 'a tuple = [`Tuple of 'a list]

type 'a expr_ = [ identifier | ('a expr_,'a) literal_ ]

(* a star_expr is either an expr or a (`Star of expr), i.e., no
   nested Star's are allowed *)
type star_expr = [ star_expr expr_ | (star_expr expr_) star]

type expr = star_expr expr_

type literal = (expr,star_expr) literal_

type tuple_expr = [
  | tuple_expr tuple
  | expr
  | [tuple_expr tuple | expr] star (* again, no nested stars *)
]

(* lhs is like a tuple expression, but no literals are allowed *)
type lhs = [
  | identifier
  | lhs tuple
  | identifier star
 ]

type def_name = 
  | Instance_Method of msg_id
  | Singleton_Method of identifier * msg_id

type rescue_guard = 
  | Rescue_Expr of tuple_expr
  | Rescue_Bind of tuple_expr * identifier

and class_kind = 
  | MetaClass of identifier
  | NominalClass of identifier * identifier option

and method_formal_param = [
    `Formal_meth_id of string
  | `Formal_amp of string
  | `Formal_star of string
  | `Formal_default of string * tuple_expr
]

and block_formal_param = [
    `Formal_block_id of var_kind * string
  | `Formal_star of string
  | `Formal_tuple of block_formal_param list
]

type any_formal = [block_formal_param|method_formal_param]

(* phony type to hide the need for recursive modules for StmtSet *)
type 'a set

type stmt = private {
  snode : stmt_node;
  pos : pos;
  sid : int;
  annotation : Annotation.t option;
  mutable lexical_locals : StrSet.t;
  (* temp_map *)
  mutable preds : stmt set;
  mutable succs : stmt set;
}

and stmt_node = 
  | Seq of stmt list
  | Alias of alias_kind
  | If of expr * stmt * stmt
  | Case of case_block
  | While of expr * stmt
  | For of block_formal_param list * expr * stmt 
  | MethodCall of lhs option * method_call
  | Assign of lhs * tuple_expr
  | Expression of expr
  | Return of tuple_expr option
  | Yield of lhs option * star_expr list
  | Module of lhs option * identifier * stmt
  | Method of def_name * method_formal_param list * stmt
  | Class of lhs option * class_kind * stmt
  | ExnBlock of exn_block
  | Begin of stmt 
  | End of stmt 
  | Defined of identifier * stmt (* id = defined(stmt) *)
  | Undef of msg_id list
  | Break of tuple_expr option
  | Next of tuple_expr option
  | Redo
  | Retry

and exn_block = {
  exn_body : stmt;
  exn_rescue : rescue_block list;
  exn_else : stmt option;
  exn_ensure : stmt option;
}
    
and rescue_block = {
  rescue_guards : rescue_guard list;
  rescue_body : stmt;
}
    
and case_block = {
  case_guard : expr;
  case_whens: (tuple_expr * stmt) list;
  case_else: stmt option;
}
    
and method_call = {
  mc_target : expr option;
  mc_msg : msg_id;
  mc_args : star_expr list;
  mc_cb : codeblock option;
}
    
and codeblock = 
  | CB_Arg of expr
  | CB_Block of block_formal_param list * stmt


type t = stmt

module StmtSet : Set.S with type elt = stmt and type t = stmt set
    
val compare : t -> t -> int
  
val mkstmt : stmt_node -> pos -> stmt

val update_stmt : stmt -> stmt_node -> stmt

val add_annotation : stmt -> Annotation.t -> stmt
  
val fold_stmt : ('a -> stmt -> 'a) -> 'a -> stmt -> 'a
val compute_cfg : stmt -> unit

val empty_stmt : unit -> stmt

val fresh_local : stmt -> identifier

val pos_of : stmt -> pos

val stmt_eq : stmt -> stmt -> bool

val msg_id_of_string : string -> msg_id

class type cfg_visitor = object

  method visit_annotation : Annotation.t Visitor.visit_method
  method visit_stmt : stmt Visitor.visit_method

  method visit_id : identifier Visitor.visit_method
  method visit_literal : literal Visitor.visit_method
  method visit_expr : expr Visitor.visit_method
  method visit_lhs : lhs Visitor.visit_method
  method visit_tuple : tuple_expr Visitor.visit_method
  method visit_rescue_guard : rescue_guard Visitor.visit_method
  method visit_def_name : def_name Visitor.visit_method
  method visit_class_kind : class_kind Visitor.visit_method
  method visit_method_param : method_formal_param Visitor.visit_method
  method visit_msg_id : msg_id Visitor.visit_method
  method visit_block_param : block_formal_param Visitor.visit_method
end

(* A visitor that walks every node in a CFG *)
class default_visitor : cfg_visitor

(* A visitor that walks every node in the current scope.  For
   instance, nested method bodies are not visited, however method
   blocks are. *)
class scoped_visitor : cfg_visitor

val visit_stmt : cfg_visitor -> stmt -> stmt

val visit_msg_id : cfg_visitor -> msg_id -> msg_id
val visit_id : cfg_visitor -> identifier -> identifier
val visit_block_param : cfg_visitor -> block_formal_param -> block_formal_param

val visit_literal : cfg_visitor ->literal -> literal
val visit_expr : cfg_visitor ->expr -> expr
val visit_lhs : cfg_visitor -> lhs -> lhs
val visit_star_expr : cfg_visitor -> star_expr -> star_expr
val visit_tuple : cfg_visitor -> tuple_expr -> tuple_expr
val visit_rescue_guard : cfg_visitor -> rescue_guard -> rescue_guard
val visit_class_kind : cfg_visitor -> class_kind -> class_kind
val visit_def_name : cfg_visitor -> def_name -> def_name
val visit_method_param : cfg_visitor -> method_formal_param -> method_formal_param

(* Converts all instances of the local variable [var] to [sub] in the
   current scope of s. *)
val alpha_convert_local : var:string -> sub:string -> stmt -> stmt

val compute_cfg_locals : ?env:StrSet.t -> stmt -> unit

(* A convenience module for easier construction of stmt nodes.  All
   expression forms include an explicit row variable to prevent
   unnecessary coercions, optional forms are presented as optional
   arguments, and common use cases have a separate, simplified
   signature.
*)
module Abbr : sig

  val local : string -> [>identifier]
  val ivar : string -> [>identifier]
  val cvar : string -> [>identifier]
  val global  : string -> [>identifier]
  val const  : string -> [>identifier]
  val builtin : string -> [>identifier]

  val var : string -> [>identifier]
  val iself : [>identifier]
  val inil : [>identifier]
  val itrue : [>identifier]
  val ifalse : [>identifier]
  val access_path : string list -> [>identifier]
    
  val num : int -> [>literal]
  val bignum : Big_int.big_int -> [>literal]
  val float : float -> [>literal]
  val str : string -> [>literal]
  val atom : string -> [>literal]
  val regexp : ?o:string -> string-> [>literal]
  val array : [<star_expr] list -> [>literal]
  val hash : ([<expr]*[<expr]) list -> [>literal]
  val range : ?inc:bool -> [<expr] -> [<expr] -> [>literal]

  val seq : stmt list -> pos -> stmt

  val alias_g : link:[<builtin_or_global] -> orig:[<builtin_or_global] -> pos -> stmt

  val alias_m : link:[<msg_id] -> orig:[<msg_id] -> pos -> stmt

  val if_s : [<expr] -> t:stmt -> f:stmt -> pos -> stmt

  val case : ?default:stmt -> [<expr] -> ([<tuple_expr]*stmt) list -> pos -> stmt

  val while_s : [<expr] -> stmt -> pos -> stmt

  val for_s : [<block_formal_param] list -> [<expr] -> stmt -> pos -> stmt

  val uop : ?lhs:[<lhs] -> unary_op -> [<expr] -> ?cb:codeblock -> pos -> stmt

  val binop : ?lhs:[<lhs] -> [<expr] ->
    binary_op -> [<star_expr] -> ?cb:codeblock -> pos -> stmt

  val call : ?lhs:[<lhs] -> ?targ:[<expr] ->
    string -> [<star_expr] list -> ?cb:codeblock -> pos -> stmt

  val massign : ?lhs:[<lhs] -> ?targ:[<expr] ->
    string -> [<star_expr] list -> ?cb:codeblock -> pos -> stmt

  val super : ?lhs:[<lhs] -> [<star_expr] list -> ?cb:codeblock -> pos -> stmt

  val mcall : ?lhs:[<lhs] -> ?targ:[<expr] ->
    [<msg_id] -> [<star_expr] list -> ?cb:codeblock -> pos -> stmt

  val assign : [<lhs] -> [<tuple_expr] -> pos -> stmt

  val expr : [<expr] -> pos -> stmt
  val return : ?v:[<tuple_expr] -> pos -> stmt
  val yield : ?lhs:[<lhs] -> ?args:[<star_expr] list-> pos -> stmt

  val module_s : ?lhs:lhs -> [<identifier] -> stmt -> pos -> stmt

  val nameclass : ?lhs:lhs -> [<identifier] -> ?inh:[<identifier] -> stmt
    -> pos -> stmt
  val metaclass : ?lhs:lhs -> [<identifier] -> stmt -> pos -> stmt

  (* Method definition for a regular method name *)
  val mdef : ?targ:[<identifier] -> string -> method_formal_param list
    -> stmt -> pos -> stmt

  (* Method definition for a assignable method name *)
  val adef : ?targ:[<identifier] -> string -> method_formal_param list
    -> stmt -> pos -> stmt

  (* Method definition for a binary operator *)
  val opdef : ?targ:[<identifier] -> binary_op -> method_formal_param list
    -> stmt -> pos -> stmt

  (* Method definition for a unary operator *)
  val uopdef : ?targ:[<identifier] -> unary_op -> method_formal_param list
    -> stmt -> pos -> stmt

  val meth : ?targ:[<identifier] -> def_name -> method_formal_param list
    -> stmt -> pos -> stmt

  val rguard : ?bind:[<identifier] -> [<tuple_expr] -> rescue_guard
  val rblock : rescue_guard list -> stmt -> rescue_block

  (* A rescue clause with a single guard *)
  val rescue : ?bind:[<identifier] -> [<tuple_expr] -> stmt -> rescue_block

  val exnblock : stmt -> rescue_block list -> ?eelse:stmt -> ?ensure:stmt
    -> pos -> stmt

  val defined : [<identifier] -> stmt -> pos -> stmt
  val undef : [<msg_id] list -> pos -> stmt
  val break : ?v:[<tuple_expr] -> pos -> stmt
  val next : ?v:[<tuple_expr] -> pos -> stmt
  val redo : pos -> stmt
  val retry : pos -> stmt

end
