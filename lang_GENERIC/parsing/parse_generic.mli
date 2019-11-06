
val parse_with_lang: Lang.t -> Common.filename -> Ast.program

(* infer the language from the filename *)
val parse_program: Common.filename -> Ast.program

(* for sgrep *)
val parse_pattern: Lang.t -> string -> Ast_generic.any
