module Utils = Utils_ruby
module Ast_printer = Ast_ruby_printer
module H = Ast_ruby_helpers
val uniq_list : ('a -> 'a -> int) -> 'a list -> 'a list
val state_override : bool ref
val begin_override : unit -> bool
module Env = Utils.StrSet
val env_stack : Env.t Stack.t
val enter_scope : 'a -> unit
val leave_scope : 'a -> unit
val clear_env : unit -> unit
val set_env : Env.t -> unit
val env : unit -> Env.t
val assigned_id : Env.elt -> bool
val seen_str : 'a -> Env.elt -> unit
val seen : 'a -> Ast_ruby.expr -> unit
val bslash_spc_re : Str.regexp
val ws_re : Str.regexp
val split_single_string_to_array : string -> Ast_ruby.tok -> Ast_ruby.expr
val split_double_string_to_array :
  Ast_ruby.string_contents list -> Ast_ruby.tok -> Ast_ruby.expr
val str_of_interp : Ast_ruby.string_contents list -> string
val merge_string_lits : Ast_ruby.expr -> Ast_ruby.expr -> Ast_ruby.expr
val process_user_string :
  string -> Ast_ruby.interp_string -> Ast_ruby.tok -> Ast_ruby.expr
val starts_with : Ast_ruby.expr -> Ast_ruby.expr
val ends_with : Ast_ruby.expr -> Ast_ruby.expr
val replace_end : Ast_ruby.expr -> Ast_ruby.expr -> Ast_ruby.expr
val is_cond_modifier : Ast_ruby.expr -> bool
val well_formed_do : Ast_ruby.expr -> 'a -> unit
val well_formed_return : Ast_ruby.expr list -> unit
val well_formed_command : 'a -> Ast_ruby.expr list -> unit
val hash_literal_as_args : Ast_ruby.expr list -> Ast_ruby.expr list
val methodcall :
  Ast_ruby.expr ->
  Ast_ruby.expr list -> Ast_ruby.expr option -> Ast_ruby.tok -> Ast_ruby.expr
val unfold_dot :
  Ast_ruby.expr -> Ast_ruby.expr -> Ast_ruby.tok -> Ast_ruby.expr
val check_for_dot : Ast_ruby.expr -> Ast_ruby.expr
val scope : Ast_ruby.expr -> Ast_ruby.expr -> Ast_ruby.expr
val tuple : Ast_ruby.expr list -> Ast_ruby.expr
val command_codeblock : Ast_ruby.expr -> Ast_ruby.expr -> Ast_ruby.expr
val fix_broken_neq :
  Ast_ruby.expr ->
  Ast_ruby.binary_op -> 'a -> Ast_ruby.expr * Ast_ruby.binary_op * 'a
val fix_broken_assoc :
  Ast_ruby.expr ->
  Ast_ruby.binary_op -> 'a -> Ast_ruby.expr * Ast_ruby.binary_op * 'a
val expr_priority : Ast_ruby.expr -> int
val binop_priority : Ast_ruby.expr -> int
val prune_uop :
  Ast_ruby.unary_op -> Ast_ruby.expr -> Ast_ruby.tok -> Ast_ruby.expr
val prune_right_assoc :
  Ast_ruby.expr -> Ast_ruby.binary_op -> Ast_ruby.expr -> Ast_ruby.expr
val prune_left_assoc :
  Ast_ruby.expr -> Ast_ruby.binary_op -> Ast_ruby.expr -> Ast_ruby.expr
val prune_tern :
  Ast_ruby.expr ->
  Ast_ruby.expr -> Ast_ruby.expr -> Ast_ruby.tok -> Ast_ruby.expr
val do_fail : string -> 'a list -> ('a -> string) -> ('a -> Ocaml.v) -> unit
val wrap : ('a * 'b * 'c) list -> ('a list -> 'd) -> 'd * 'b * 'c
val rhs_do_codeblock : Ast_ruby.expr -> bool
val resolve_block_delim :
  Ast_ruby.expr -> Ast_ruby.expr -> Ast_ruby.expr list
val merge_binop :
  (Ast_ruby.expr * 'a * 'b) list -> Ast_ruby.expr list * 'a * 'b
val merge_topcall :
  (Ast_ruby.expr * 'a * 'b) list -> Ast_ruby.expr list * 'a * 'b
val merge_stmt :
  (Ast_ruby.expr * 'a * 'b) list -> Ast_ruby.expr list * 'a * 'b
val merge_expr :
  string -> (Ast_ruby.expr * 'a * 'b) list -> Ast_ruby.expr list * 'a * 'b
val merge_expr_list :
  string -> (Ast_ruby.ast * 'a * 'b) list -> Ast_ruby.ast list * 'a * 'b
val merge_formal_list :
  string ->
  (Ast_ruby.formal_param list * 'a * 'b) list ->
  Ast_ruby.formal_param list list * 'a * 'b
val merge_rest : string -> ('a * 'b * 'c) list -> 'a list * 'b * 'c
val merge_rescue :
  string ->
  ((Ast_ruby.expr * Ast_ruby.expr) * 'a * 'b) list ->
  (Ast_ruby.expr * Ast_ruby.expr) list * 'a * 'b
