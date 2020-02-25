open Ast_ruby

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
