
val refactor_ast : ?env:Utils.StrSet.t -> Ast.expr list -> Cfg.stmt
  (** Convert an Ast into a Cfg *)

val re_init : unit -> unit
  (** Re-initialize the global state of the module that is used to
      provide unique identifier names.  Should only be necessary for
      testing. *)

val reparse : ?env:Utils.StrSet.t -> ?filename:string -> ?lineno:int -> 
  (Cfg.stmt,'a) Cfg_printer.CodeUnparser.fmt -> 'a
  (** This function takes an unparsing descript as described in
      Cfg_printer and converts the unparsed string back into a
      Cfg.stmt, allowing for easy tree rewriting as describedy by
      Demaille, Levillain, and Sigoure's SAC'09 paper.  For example,

      open Cfg_printer.CodeUnparser
      reparse (s"x = " ++ expr) my_expr

      returns the statement that assigns [my_expr] into the variable
      [x].
  *) 

val kreparse : ?env:Utils.StrSet.t -> ?filename:string -> ?lineno:int -> 
  (Cfg.stmt -> 'a) -> ('a,'b) Cfg_printer.CodeUnparser.fmt -> 'b
  (** Similar to reparse, but passes the final stmt to the supplied
      continuation *)

val freparse : ?env:Utils.StrSet.t -> ?filename:string -> ?lineno:int -> 
  ('a, Format.formatter, unit, Cfg.stmt) format4 -> 'a
  (** Similar to reparse, but uses a format string instead *)

val kfreparse : ?env:Utils.StrSet.t -> ?filename:string -> ?lineno:int -> 
  (Cfg.stmt -> 'b) -> ('a, Format.formatter, unit, 'b) format4 -> 'a
  (** Similar to kreparse, but uses a format string instead *)
