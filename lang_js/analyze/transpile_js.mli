
val xhp: 
  (Cst_js.expr -> Ast_js.expr) ->
  Cst_js.xhp_html -> Ast_js.expr

val var_pattern:
  ((Cst_js.expr -> Ast_js.expr) * 
   (Cst_js.name -> Ast_js.name) *
   (Cst_js.property_name -> Ast_js.property_name)
  ) ->
  Cst_js.variable_declaration_pattern -> Ast_js.var list