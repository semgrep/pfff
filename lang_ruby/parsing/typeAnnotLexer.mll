{ 
  open TypeAnnotParser
}

let space_re = ['\t' ' ']
let cap_re = ['A'-'Z']
let low_re = ['a'-'z']
let num_re = ['0'-'9']
let alphanum_re = cap_re | low_re | num_re

let ident_re = alphanum_re | '_'

let inst_id_re = (cap_re | low_re | '_') ident_re*
let const_id_re = cap_re+ ident_re*
let type_id_re = (low_re|'_')+ ident_re* '\''?

let meth_sym_re = 
  [ '%' '&' '*' '+' '-' '/' '<' '>' '^' '|' '~' ]
  | "**" | "+@" | "-@" | "<<" | "<=" | "<=>" | "==" | ">=" | ">>"
  | "[]" | "===" | "<==>" | "[]=" | "=~"

let meth_name_re = inst_id_re ['?' '!' '=']


rule token = parse
  | space_re+ { token lexbuf }
  | '\\' space_re* '\n' space_re* "##%" { token lexbuf }
  | "##%" { T_BEGIN_LINE (lexbuf.Lexing.lex_curr_p) }
  | "#" ([^ '#' '\n'] [^ '\n']*)? { token lexbuf }

      (* entry tokens *)
  | "%cast%" { K_CAST (lexbuf.Lexing.lex_curr_p) }
  | "%def%" { K_DEF (lexbuf.Lexing.lex_curr_p) }
  | "%class%" { K_CLASS (lexbuf.Lexing.lex_curr_p) }
  | "%module%" { K_MODULE (lexbuf.Lexing.lex_curr_p) }

      (* built in type constructors *)
  | "*" { T_STAR(lexbuf.Lexing.lex_curr_p) }
  | "?" { T_QUESTION(lexbuf.Lexing.lex_curr_p) }
  | "^" { T_CARROT(lexbuf.Lexing.lex_curr_p) }
      
  | "@FIXME" {Log.fatal (Log.of_loc lexbuf.Lexing.lex_curr_p)
                "deprecated @@FIXME, use !FIXME"}

  | "@" inst_id_re {T_INST_VAR(lexbuf.Lexing.lex_curr_p,Lexing.lexeme lexbuf)}
  | "!" { T_BANG(lexbuf.Lexing.lex_curr_p) }
  | ":" { T_COLON (lexbuf.Lexing.lex_curr_p) }
  | "::" { T_DOUBLE_COLON (lexbuf.Lexing.lex_curr_p) }
  | "." { T_DOT (lexbuf.Lexing.lex_curr_p) }
  | "->" { T_RARROW (lexbuf.Lexing.lex_curr_p) }
  | "(" { T_LPAREN (lexbuf.Lexing.lex_curr_p) }
  | ")" { T_RPAREN (lexbuf.Lexing.lex_curr_p) }
  | "[" { T_LBRACKET (lexbuf.Lexing.lex_curr_p) }
  | "]" { T_RBRACKET (lexbuf.Lexing.lex_curr_p) }
  | "," { T_COMMA (lexbuf.Lexing.lex_curr_p) }
  | "{" { T_LBRACE (lexbuf.Lexing.lex_curr_p) }
  | "}" { T_RBRACE (lexbuf.Lexing.lex_curr_p) }
  | "<" { T_LESS (lexbuf.Lexing.lex_curr_p) }
  | ">" { T_GREATER (lexbuf.Lexing.lex_curr_p) }
  | ";" { T_SEMICOLON (lexbuf.Lexing.lex_curr_p)}
  | '\n' { T_NEWLINE (lexbuf.Lexing.lex_curr_p) }

      (* keywords *)
  | "or" { K_OR (lexbuf.Lexing.lex_curr_p) }
  | "self" { K_SELF (lexbuf.Lexing.lex_curr_p) }

  | type_id_re as t { T_TYPE_ID (lexbuf.Lexing.lex_curr_p,t) }
  | const_id_re as t { T_CONST_ID (lexbuf.Lexing.lex_curr_p,t) }
  | '"' ([^'"']+ as s) '"' { T_METHOD_NAME (lexbuf.Lexing.lex_curr_p, s) }
  | meth_name_re as s { T_METHOD_NAME (lexbuf.Lexing.lex_curr_p, s) }

  | eof { T_EOF }

  | "<=" { T_SUBTYPE (lexbuf.Lexing.lex_curr_p) }
