
val visit_program:
  tag_hook: (Parse_info.info -> Highlight_code.category -> unit) ->
  Highlight_code.highlighter_preferences ->
  Ast_skip.program option * Parser_skip.token list ->
  unit
