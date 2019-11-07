
(* mostly for fuzzy sgrep/spatch *)
val parse_with_lang: Lang_fuzzy.t -> Common.filename -> Ast_fuzzy.trees

val parse: Common.filename -> Ast_fuzzy.trees


