
val store_position: bool ref

(* this may raise Parse_info.Ast_builder_error *)
val program: 
  Cst_php.program -> Ast_php.program
val program_with_position_information: 
  Cst_php.program -> Ast_php.program

