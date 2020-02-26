open Utils_ruby

open Il_ruby

val compare : t -> t -> int
  
val mkstmt : stmt_node -> pos -> stmt

val update_stmt : stmt -> stmt_node -> stmt

 
val fold_stmt : ('a -> stmt -> 'a) -> 'a -> stmt -> 'a
val compute_cfg : stmt -> unit


val empty_stmt : unit -> stmt

val fresh_local : stmt -> identifier

val pos_of : stmt -> pos

val stmt_eq : stmt -> stmt -> bool

val msg_id_of_string : string -> msg_id

class type cfg_visitor = object

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

  val alias_m : link:msg_id -> orig:msg_id -> pos -> stmt

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
    msg_id -> [<star_expr] list -> ?cb:codeblock -> pos -> stmt

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
  val undef : msg_id list -> pos -> stmt
  val break : ?v:[<tuple_expr] -> pos -> stmt
  val next : ?v:[<tuple_expr] -> pos -> stmt
  val redo : pos -> stmt
  val retry : pos -> stmt

end
