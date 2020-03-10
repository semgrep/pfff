type state =     
  | Bol (* beginning of line *)
  | AfterCommand (* after command, before args *)
  | EndOfExpr (* end of expr / literal *) 
  | AfterDef (* after a 'def' or '.' token *)
  | AfterLocal (* after a local variable *)

type t = { 
  mutable state : state;
  lexer_stack : cps_lexer Stack.t;
}

and cps_lexer = t -> Lexing.lexbuf -> Parser_ruby.token

let beg_state t = t.state <- Bol
let mid_state t = t.state <- AfterCommand
let end_state t = t.state <- EndOfExpr
let def_state t = t.state <- AfterDef
let local_state t = t.state <- AfterLocal

let create entry = 
  let stk = Stack.create () in
    Stack.push entry stk;
    {state = Bol;
     lexer_stack = stk;
    }

