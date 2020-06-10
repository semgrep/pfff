
val state_override : bool ref
val begin_override : unit -> bool

val enter_scope : 'a -> unit
val leave_scope : 'a -> unit

module Env = Utils_ruby.StrSet
val clear_env : unit -> unit
val set_env : Env.t -> unit

val seen : 'a -> Ast_ruby.expr -> unit
val seen_str : 'a -> string -> unit

val is_cond_modifier : Ast_ruby.expr -> bool


val unfold_dot :
  Ast_ruby.expr -> Ast_ruby.expr -> Ast_ruby.tok -> Ast_ruby.expr
val methodcall :
  Ast_ruby.expr ->
  Ast_ruby.expr list -> Ast_ruby.expr option -> Ast_ruby.tok -> Ast_ruby.expr
val command_codeblock : Ast_ruby.expr -> Ast_ruby.expr -> Ast_ruby.expr

val scope : Ast_ruby.expr -> Ast_ruby.expr -> Ast_ruby.expr

val well_formed_command : 'a -> Ast_ruby.expr list -> unit
val well_formed_return : Ast_ruby.expr list -> unit
val well_formed_do : Ast_ruby.expr -> 'a -> unit

val process_user_string :
  string -> Ast_ruby.interp_string -> Ast_ruby.tok -> Ast_ruby.expr

val assigned_id : string -> bool



val prune_uop :
  Ast_ruby.unary_op -> Ast_ruby.expr -> Ast_ruby.tok -> Ast_ruby.expr

val prune_tern :
  Ast_ruby.expr -> Ast_ruby.expr -> Ast_ruby.expr -> 
  Ast_ruby.tok -> Ast_ruby.tok -> 
  Ast_ruby.expr

val prune_left_assoc :
  Ast_ruby.expr -> Ast_ruby.binary_op -> Ast_ruby.expr -> Ast_ruby.expr

val prune_right_assoc :
  Ast_ruby.expr -> Ast_ruby.binary_op -> Ast_ruby.expr -> Ast_ruby.expr

val wrap : ('a * 'b * 'c) list -> ('a list -> 'd) -> 'd * 'b * 'c

val merge_binop :
  (Ast_ruby.expr * 'a * 'b) list -> Ast_ruby.expr list * 'a * 'b

val merge_topcall :
  (Ast_ruby.expr * 'a * 'b) list -> Ast_ruby.expr list * 'a * 'b


val merge_expr :
  string -> (Ast_ruby.expr * 'a * 'b) list -> Ast_ruby.expr list * 'a * 'b

val merge_stmt :
  (Ast_ruby.expr * 'a * 'b) list -> Ast_ruby.expr list * 'a * 'b


val merge_expr_list :
  string -> (Ast_ruby.program * 'a * 'b) list -> Ast_ruby.program list * 'a * 'b

val merge_formal_list :
  string ->
  (Ast_ruby.formal_param list * 'a * 'b) list ->
  Ast_ruby.formal_param list list * 'a * 'b

val merge_rescue :
  string ->
  ((Ast_ruby.expr * Ast_ruby.expr) * 'a * 'b) list ->
  (Ast_ruby.expr * Ast_ruby.expr) list * 'a * 'b

val merge_string_lits : Ast_ruby.expr -> Ast_ruby.expr -> Ast_ruby.expr


val merge_rest : string -> ('a * 'b * 'c) list -> 'a list * 'b * 'c

(* helpers used also in parse_ruby.ml *)
val do_fail:
  string -> 'a list -> ('a -> string) -> ('a -> OCaml.v) -> unit
val uniq_list:
  ('a -> 'a -> int) -> 'a list -> 'a list
