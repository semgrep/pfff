(*
   Public entry point for parsing regexps.
*)

(* Parse a file *)
val file : ?conf:Conf.t -> string -> AST.t

(* Parse a string *)
val string : ?conf:Conf.t -> string -> AST.t

(* Alias for 'file' *)
val parse : ?conf:Conf.t -> string -> AST.t
