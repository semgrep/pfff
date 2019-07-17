
val xhp: 
  (Cst_js.expr -> Ast_js.expr) ->
  Cst_js.xhp_html -> Ast_js.expr

val var_pattern:
  ((Cst_js.expr -> Ast_js.expr) * 
   (Cst_js.name -> Ast_js.name) *
   (Cst_js.property_name -> Ast_js.property_name)
  ) ->
  Cst_js.variable_declaration_pattern -> Ast_js.var list

val forof:
  (Cst_js.lhs_or_var * Cst_js.tok * Cst_js.expr * Cst_js.stmt) ->
   ((Cst_js.expr -> Ast_js.expr) *
    (Cst_js.stmt -> Ast_js.stmt list)) ->
   Ast_js.stmt list
