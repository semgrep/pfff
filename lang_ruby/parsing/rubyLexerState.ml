
type expr_state =     
  | Expr_Beg (* beginning of line *)
  | Expr_Mid (* after command, before args *)
  | Expr_End (* end of expr / literal *) 
  | Expr_Def (* after a 'def' or '.' token *)
  | Expr_Local (* after a local variable *)

type t = { 
  mutable expr_state : expr_state;
  lexer_stack : cps_lexer Stack.t;
}

and cps_lexer = t -> Lexing.lexbuf -> NewParser.token

let beg_state t = t.expr_state <- Expr_Beg
let mid_state t = t.expr_state <- Expr_Mid
let end_state t = t.expr_state <- Expr_End
let def_state t = t.expr_state <- Expr_Def
let local_state t = t.expr_state <- Expr_Local

let create entry = 
  let stk = Stack.create () in
    Stack.push entry stk;
    {expr_state = Expr_Beg;
     lexer_stack = stk;
    }


let to_string = function
  | Expr_Beg -> "Expr_Beg"
  | Expr_Mid -> "Expr_Mid"
  | Expr_End -> "Expr_End"
  | Expr_Def -> "Expr_Def"
  | Expr_Local -> "Expr_Local"
