
exception ObsoleteConstruct of Cst_php.info
exception TodoConstruct of string * Cst_php.info

val store_position: bool ref

val program: 
  Cst_php.program -> Ast_php_simple.program
val program_with_position_information: 
  Cst_php.program -> Ast_php_simple.program

