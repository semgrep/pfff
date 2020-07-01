open Ast_ruby

(*****************************************************************************)
(* xxx_of_string *)
(*****************************************************************************)

let binary_op_of_string = function
  | "="    -> Op_ASSIGN   
  | "+"    -> Op_PLUS     
  | "-"    -> Op_MINUS    
  | "*"    -> Op_TIMES    
  | "/"    -> Op_DIV      
  | "%"    -> Op_REM      
  | "<=>"  -> Op_CMP    
  | "=="   -> Op_EQ     
  | "==="  -> Op_EQQ    
  | "!="   -> Op_NEQ    
  | ">="   -> Op_GEQ    
  | "<="   -> Op_LEQ    
  | "<"    -> Op_LT     
  | ">"    -> Op_GT     
  | "&&"   -> Op_AND      
  | "||"   -> Op_OR 
  | "&"    -> Op_BAND     
  | "|"    -> Op_BOR      
  | "=~"   -> Op_MATCH    
  | "!~"   -> Op_NMATCH   
  | "^"    -> Op_XOR      
  | "**"   -> Op_POW      
  | "and"  -> Op_kAND     
  | "or"   -> Op_kOR      
  | "=>"   -> Op_ASSOC    
  | "[]"   -> Op_AREF     
  | "[]="  -> Op_ASET     
  | "<<"   -> Op_LSHIFT   
  | ">>"   -> Op_RSHIFT   
  | ".."   -> Op_DOT2     
  | "..."  -> Op_DOT3     
  | s -> failwith ("binary_op_of_string: " ^ s)
(*
  | Op_Custom s -> Printf.sprintf "Custom_op(%s)" s
  | Op_OP_ASGN op -> (str_binop op) ^ "="
*)

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)
(*

let id_kind s _pos = match s.[0] with
  | 'a'..'z' | '_' -> ID_Lowercase
  | 'A'..'Z' -> ID_Uppercase
  | '@' -> 
      if s.[1] == '@' then ID_Class
      else ID_Instance
  | '$' -> begin match s.[1] with
      | 'a'..'z' | 'A'..'Z' | '_'  -> ID_Global
      | _ -> ID_Global (* ID_Builtin *)
    end
  | _ -> failwith (*(Log.of_loc pos)*) 
    (spf "unknown id_kind in cfg_refactor: %s" s)

let msg_of_str a pos = match a with
  | "+" -> Operator(Op_PLUS,pos)
  | "-" -> Operator(Op_MINUS,pos)
  | "*" -> Operator(Op_TIMES,pos)
  | "/" -> Operator(Op_DIV,pos)
  | "%" -> Operator(Op_REM,pos)
  | "<=>" -> Operator(Op_CMP,pos)
  | "==" -> Operator(Op_EQ,pos)
  | "===" -> Operator(Op_EQQ,pos)
  | ">=" -> Operator(Op_GEQ,pos)
  | "<=" -> Operator(Op_LEQ,pos)
  | "<" -> Operator(Op_LT,pos)
  | ">" -> Operator(Op_GT,pos)
  | "&" -> Operator(Op_BAND,pos)
  | "|" -> Operator(Op_BOR,pos)
  | "=~" -> Operator(Op_MATCH,pos)
  | "^" -> Operator(Op_XOR,pos)
  | "**" -> Operator(Op_POW,pos)
  | "[]" -> Operator(Op_AREF,pos)
  | "[]=" -> Operator(Op_ASET,pos)
  | "<<" -> Operator(Op_LSHIFT,pos)
  | ">>" -> Operator(Op_RSHIFT,pos)

  | "-@" -> UOperator(Op_UMinus,pos)
  | "+@" -> UOperator(Op_UPlus,pos)
  | "~@" | "~" -> UOperator(Op_UTilde,pos)
  | s -> Id((s, pos), id_kind s pos)
*)