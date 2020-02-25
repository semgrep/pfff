{
  open NewParser
  open RubyLexerState
  open Lexing
  open Printf

  let pop_lexer state = 
    let (_:cps_lexer) = Stack.pop state.lexer_stack in
      ()

  let fail_eof f state lexbuf = 
    failwith "parse error: premature end of file"

  (* emit the token [tok] and then proceed with the continuation [k] *)
  let emit_extra tok k state lexbuf = 
    let once state _ = 
      pop_lexer state;
      k state lexbuf
    in
      Stack.push once state.lexer_stack;
      tok

  let update_pos str pos = 
    let chars = String.length str in
    let line_counter acc = function '\n' -> acc+1 | _ -> acc in
    let lines = Utils.string_fold_left line_counter 0 str in
      {pos with
         pos_cnum = pos.pos_cnum - chars;
         pos_lnum = pos.pos_lnum - lines;
      }

  let push_back str lexbuf = 
    let pre_str = String.sub lexbuf.lex_buffer 0 lexbuf.lex_curr_pos in
    let post_str = 
      String.sub lexbuf.lex_buffer lexbuf.lex_curr_pos
        (lexbuf.lex_buffer_len-lexbuf.lex_curr_pos)
    in
      lexbuf.lex_buffer <- pre_str ^ str ^ post_str;
      lexbuf.lex_buffer_len <- lexbuf.lex_buffer_len + (String.length str);
      lexbuf.lex_curr_p <- update_pos str lexbuf.lex_curr_p;
      assert ((String.length lexbuf.lex_buffer) == lexbuf.lex_buffer_len)
      
  let contents_of_str s = [Ast.StrChars s]

  (* returns true if string following the modifier m (e.g., %m{...})
     should be parsed as a single quoted or double quoted (interpreted)
     string *)
  let modifier_is_single = function
    | "r" -> false
    | "w" -> true
    | "W" -> false
    | "q" -> true
    | "Q" -> false
    | "x" -> false
    | "" -> false
    | m ->  failwith (Printf.sprintf "unhandled string modifier: %s" m)

  (* Ruby numerics can include _ to separate arbitrary digits.  This
     returns a new string representing a numeric with all _s removed *)
  let remove_underscores str = 
    let len = String.length str in
    let buf = Buffer.create len in
      String.iter
        (function
           | '.'
           | '-' (* needed for 2.3e-4 *)
           | '+' (* needed for 2.3e+4 *)
           | 'e'
           | ('a'..'f') | ('A'..'F') | 'x'
           | ('0'..'9') as i -> Buffer.add_char buf i
           | '_' -> ()
           | _ -> failwith "non number and non-'_' in fixnum"
        ) str;
      Buffer.contents buf

  let to_bignum str = Big_int.big_int_of_string (remove_underscores str)

  (* increment the line count and start of line counters for the lexbuf *)
  let incr_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
      pos_lnum = pos.pos_lnum + 1;
      pos_bol = pos.pos_cnum;
    }

  (* base 16 -> base 10 helper *)
  let num_of_hex_digit = function
    | '0' -> 0
    | '1' -> 1
    | '2' -> 2
    | '3' -> 3
    | '4' -> 4
    | '5' -> 5
    | '6' -> 6
    | '7' -> 7
    | '8' -> 8
    | '9' -> 9
    | 'a' | 'A' -> 10
    | 'b' | 'B' -> 11
    | 'c' | 'C' -> 12
    | 'd' | 'D' -> 13
    | 'e' | 'E' -> 14
    | 'f' | 'F' -> 15
    | c -> failwith (Printf.sprintf "num_of_hex_digit: %c" c)

  (* converts the number stored in [str] initially represented in base
     [base] into a base 10 token (fixnum or bignum) *)
  let convert_to_base10 ~base str pos = 
    let str = remove_underscores str in
    let base10 x exp = (* compute x * (base ** exp) *)
      Big_int.mult_big_int x 
        (Big_int.power_int_positive_int base exp)
    in
    let rec helper acc idx exp = 
      if idx < 0 then acc
      else
        let digit = num_of_hex_digit str.[idx] in
        let b10 = base10 (Big_int.big_int_of_int digit) exp in
        let acc' = Big_int.add_big_int b10 acc in
          helper acc' (idx - 1) (exp + 1)
    in
    let len = String.length str in
    let num = helper Big_int.zero_big_int (len-1) 0 in
      if Big_int.is_int_big_int num
      then T_FIXNUM(Big_int.int_of_big_int num, pos)
      else T_BIGNUM(num, pos)

  let negate_numeric = function
    | T_FIXNUM(n,p) -> T_FIXNUM(-n,p)
    | T_FLOAT(s,n,p) -> T_FLOAT("-"^s,-.n,p)
    | T_BIGNUM(n,p) -> T_BIGNUM(Big_int.minus_big_int n,p)
    | _ -> assert false

  let close_delim = function
    | '{' -> '}'
    | '<' -> '>'
    | '(' -> ')'
    | '[' -> ']'
    | _ -> assert false

  (* chooses the T_LID or T_UID token based on the first character of [id] *)
  let choose_capital_for_id id pos = 
    assert (String.length id > 0);
    match (id).[0] with
      | 'a'..'z' | '_' -> T_LID(id, pos)
      | 'A'..'Z' -> T_UID(id, pos)
      | _ -> failwith "unknown prefix char for ID: this shouldn't happen"

  (* David: the reason for this is because method name can be
     def, class, module; to distinguish K_DEF from def *)
  let insert_delimiters s = "%" ^ s  ^ "%"

  (* helper for transitioning between states *)
  let beg_choose want yes no state lexbuf = 
    if state.expr_state == want 
    then yes state lexbuf else no state lexbuf
      
  type chooser = cps_lexer -> cps_lexer -> cps_lexer

  (* state transition functions *)
  let on_beg : chooser = beg_choose Expr_Beg
  let on_mid : chooser = beg_choose Expr_Mid 
  let on_end : chooser = beg_choose Expr_End 
  let on_def : chooser = beg_choose Expr_Def 
  let on_local : chooser = beg_choose Expr_Local 

  (* CPS terminators *)
  let t_uminus s lb = beg_state s; T_UMINUS lb.lex_curr_p
  let t_minus  s lb = beg_state s; T_MINUS lb.lex_curr_p
  let t_uplus  s lb = beg_state s; T_UPLUS lb.lex_curr_p
  let t_plus   s lb = beg_state s; T_PLUS lb.lex_curr_p
  let t_ustar  s lb = beg_state s; T_USTAR lb.lex_curr_p
  let t_star   s lb = beg_state s; T_STAR lb.lex_curr_p
  let t_uamper s lb = beg_state s; T_UAMPER lb.lex_curr_p
  let t_amper  s lb = beg_state s; T_AMPER lb.lex_curr_p
  let t_slash  s lb = beg_state s; T_SLASH lb.lex_curr_p
  let t_quest  s lb = beg_state s; T_QUESTION lb.lex_curr_p
  let t_tilde  s lb = beg_state s; T_TILDE lb.lex_curr_p
  let t_scope  s lb = beg_state s; T_SCOPE lb.lex_curr_p
  let t_uscope s lb = beg_state s; T_USCOPE lb.lex_curr_p
  let t_lbrack s lb = beg_state s; T_LBRACK lb.lex_curr_p
  let t_lbrack_arg s lb = beg_state s; T_LBRACK_ARG lb.lex_curr_p
  let t_lparen s lb = beg_state s; T_LPAREN lb.lex_curr_p
  let t_lparen_arg s lb = beg_state s; T_LPAREN_ARG lb.lex_curr_p
  let t_lbrace s lb = beg_state s; T_LBRACE lb.lex_curr_p
  let t_lbrace_arg s lb = beg_state s; T_LBRACE_ARG lb.lex_curr_p
  let t_percent s lb = beg_state s; T_PERCENT lb.lex_curr_p
  let t_lshft s lb = beg_state s; T_LSHFT lb.lex_curr_p
  let t_colon s lb = beg_state s; T_COLON lb.lex_curr_p
  let t_eol s lb = beg_state s; T_EOL

  (* return a fresh Buffer.t that is preloaded with the contents of [str] *)
  let buf_of_string str = 
    let b = Buffer.create 31 in
      Buffer.add_string b str;
      b

  (* transitions to End unless in Def, in which case does nothing (stays in Def) *)
  let def_end_state state = match state.expr_state with
    | Expr_Def -> ()
    | _ -> end_state state
}

let ws_re = ['\t' ' '] 
let nl_re = '\n' | "\r\n"
let alpha_re = ['a'-'z''A'-'Z']
let num_re = ['0'-'9']
let alphanum_re = alpha_re | num_re
let rubynum_re = num_re ("_" | num_re)* 
let fixnum_re = rubynum_re
let post_fixnum_re = ("_" | num_re)* 
let rubyfloat_re = rubynum_re ('.' (rubynum_re))? ("e" ("-"|"+")? rubynum_re+)?
let post_rubyfloat_re = 
  post_fixnum_re ('.' (rubynum_re))? ("e" ("-"|"+")? rubynum_re+)?
let id_start = '_' | alpha_re
let id_body = '_' | alphanum_re
let id_suffix = '?'| '!' (*('!' [^'=']?)*)
let id_re = id_start id_body* id_suffix?
let string_single_delim = [
  '!' '@' '#' '$' '%' '^' '&' '*'
  ',' '.' '?' '`' '~' '|' '+' '_'
  '-' '\\' '/' ':' '"' '\'']

let type_annot_re = "##%" [^ '\n']* nl_re
let opt_type_annot_re = "#O#%" [^ '\n']* nl_re
let e = "" (* epsilon *)

rule token state = parse
  | e {if begin_override() then beg_state state;
       (Stack.top state.lexer_stack) state lexbuf}

and top_lexer state = parse
    (* need the ws here to force a longest match preference over the
       rules below *)
  | ws_re* ((['+' '-' '*' '&' '|' '%' '^'] | "||" | "&&" | "<<" | ">>") as op) '='
      {beg_state state; T_OP_ASGN(op, lexbuf.lex_curr_p)}

  (* /= can be either regexp or op_asgn *)
  | ws_re* "/=" 
      {match state.expr_state with 
         | Expr_Beg -> regexp_string (buf_of_string "=") state lexbuf
         | _ -> beg_state state; T_OP_ASGN("/", lexbuf.lex_curr_p)
      }

  (* need precedence over single form *)
  | ws_re* "**" {beg_state state;T_POW lexbuf.lex_curr_p }
  | ws_re* "&&" {beg_state state;T_ANDOP lexbuf.lex_curr_p }

  (* the following lexemes may represent various tokens depending on
     the expression state and surrounding spaces.  Space before and
     after is typically the binop form, while a space before but not
     after is uop.  *)
  | ws_re+ '-' 
      {let binop = on_def (postfix_at t_uminus t_minus) t_minus
       in space_uop uop_minus_lit t_uminus binop state lexbuf}
  | ws_re+ '+' 
      {let binop = on_def (postfix_at t_uplus t_plus) t_plus
       in space_uop uop_plus_lit t_uplus binop state lexbuf}
  | ws_re+ '*' {space_uop t_ustar t_ustar t_star state lexbuf}
  | ws_re+ '&' {space_uop t_uamper t_uamper t_amper state lexbuf}
  | ws_re+ '[' 
      {let binop = on_local t_lbrack_arg t_lbrack in 
         space_uop t_lbrack t_lbrack binop state lexbuf}
  | ws_re+ '(' 
      {let binop = on_local t_lparen_arg t_lparen in 
         space_uop t_lparen t_lparen binop state lexbuf}
  | ws_re+ '%' {space_tok percent t_percent state lexbuf}
  | ws_re+ '?' {space_tok char_code t_quest state lexbuf}
      
  (* no space is usually a binop, but is parsed as a uop at expr_beg
     or if there is a trailing @ in the def state *)
  | '-' {on_def (postfix_at t_uminus t_minus)
           (on_beg uop_minus_lit t_minus) state lexbuf}
  | '+' {on_def (postfix_at t_uplus t_plus)
           (on_beg uop_plus_lit t_plus) state lexbuf}
  | '*' {on_beg t_ustar t_star state lexbuf }
  | '&' {on_beg t_uamper t_amper state lexbuf }
  | '[' {on_beg t_lbrack t_lbrack_arg state lexbuf}
  | '(' {on_beg t_lparen t_lparen_arg state lexbuf}
  | '{' {on_beg t_lbrace t_lbrace_arg state lexbuf}
  | '%' {on_beg percent t_percent state lexbuf}
  | '?' {on_beg char_code t_quest state lexbuf}

  (* need to explicitly separate out cases for / since spaces can
     be significant if they occur inside of a regexp *)
  | ws_re+ '/' (ws_re+ as spc)
      {on_beg (regexp_string (buf_of_string spc)) t_slash state lexbuf}
  | ws_re+ '/' (nl_re as nl)
      {incr_line lexbuf;
       on_beg (regexp_string (buf_of_string nl)) t_slash state lexbuf}
  | ws_re+ '/' {space_tok regexp t_slash state lexbuf}
  | '/' {on_beg regexp t_slash state lexbuf}

  (* heredoc vs shift tokens *)
  | "<<-" {heredoc_header heredoc_string_lead state lexbuf}
  | "<<" ws_re {beg_state state; t_lshft state lexbuf}
  | "<<" nl_re {beg_state state; incr_line lexbuf;t_lshft state lexbuf}
  | "<<" {match state.expr_state with
            | Expr_End | Expr_Local | Expr_Def -> 
                beg_state state;T_LSHFT lexbuf.lex_curr_p
            | Expr_Mid | Expr_Beg -> 
                heredoc_header heredoc_string state lexbuf}

  (* now all of the 'normal' tokens which are otherwise well behaved *)
  | '.'   {def_state state;T_DOT lexbuf.lex_curr_p }
  | ','   {beg_state state;T_COMMA lexbuf.lex_curr_p }
  | '!'   {beg_state state;T_BANG lexbuf.lex_curr_p }
  | '~'   {match state.expr_state with
               (* when defining the ~ method, allow an optional @ postfix *)
             | Expr_Def -> postfix_at t_tilde t_tilde state lexbuf
             | _ -> t_tilde state lexbuf }
  | "<=>" {beg_state state;T_CMP lexbuf.lex_curr_p }
  | "="   {beg_state state;T_ASSIGN lexbuf.lex_curr_p }
  | "=="  {beg_state state;T_EQ lexbuf.lex_curr_p }
  | "===" {beg_state state;T_EQQ lexbuf.lex_curr_p }
  | "!="  {beg_state state;T_NEQ lexbuf.lex_curr_p }
  | ">="  {beg_state state;T_GEQ lexbuf.lex_curr_p }
  | "<="  {beg_state state;T_LEQ lexbuf.lex_curr_p }
  | "<"   {beg_state state;T_LT lexbuf.lex_curr_p }
  | ">"   {beg_state state;T_GT lexbuf.lex_curr_p }
  | "||"  {beg_state state;T_OROP lexbuf.lex_curr_p }
  | "=~"  {beg_state state;T_MATCH lexbuf.lex_curr_p }
  | "!~"  {beg_state state;T_NMATCH lexbuf.lex_curr_p }
  | ">>"  {beg_state state;T_RSHFT lexbuf.lex_curr_p}
  | "=>"  {beg_state state;T_ASSOC lexbuf.lex_curr_p}
  | ')'   {end_state state;T_RPAREN lexbuf.lex_curr_p}
  | ']'   {end_state state;T_RBRACK lexbuf.lex_curr_p}
  | '}'   {end_state state;T_RBRACE lexbuf.lex_curr_p}
  | "..." {beg_state state;T_DOT3 lexbuf.lex_curr_p}
  | ".."  {beg_state state;T_DOT2 lexbuf.lex_curr_p}
  | '#'   {comment state lexbuf}
  | '`'   {tick_string state lexbuf}
  | '^'   {beg_state state;T_CARROT lexbuf.lex_curr_p}
  | '|'   {beg_state state;T_VBAR lexbuf.lex_curr_p}
  | ';'   {beg_state state;T_SEMICOLON lexbuf.lex_curr_p}
  | '\''  {end_state state; 
           non_interp_string '\'' (Buffer.create 31) state lexbuf}
  | '\"'  {double_string state lexbuf}
  | "=begin" [^'\n']* '\n' {beg_state state; incr_line lexbuf; 
                            delim_comment state lexbuf}

  | num_re {postfix_numeric Utils.id (lexeme lexbuf) state lexbuf}
  | "\\\n" {incr_line lexbuf; top_lexer state lexbuf}
  | nl_re {incr_line lexbuf; t_eol state lexbuf}
  | eof   {T_EOF}

  (* ws_re* '(' ([^')']+ as typ) ')' ws_re* (id_re as e) ws_re**)
  | "###%" ws_re* (id_re as e) ws_re* ':' ws_re* ([^'\n']*  as annot)
      {let annot_str = (insert_delimiters "cast") ^ " " ^ annot in
         beg_state state; T_CAST(annot_str,e,lexbuf.lex_curr_p)}

  | opt_type_annot_re as line
  | type_annot_re as line
      {incr_line lexbuf;
       type_annot state (buf_of_string line) lexbuf
      }

  | "class" as cls    
      {def_state state; K_CLASS ((insert_delimiters cls), lexbuf.lex_curr_p) }
  | "def" as def
      {def_state state; K_DEF ((insert_delimiters def), lexbuf.lex_curr_p) }
  | "module" as m   
      {def_state state;K_MODULE ((insert_delimiters m), lexbuf.lex_curr_p)}

  | "alias"    {def_state state;K_ALIAS lexbuf.lex_curr_p}
  | "undef"    {def_state state;K_UNDEF lexbuf.lex_curr_p}
  | "and"      {beg_state state;K_AND lexbuf.lex_curr_p}
  | "begin"    {beg_state state;K_lBEGIN lexbuf.lex_curr_p}
  | "BEGIN"    {beg_state state;K_BEGIN lexbuf.lex_curr_p}
  | "case"     {beg_state state;K_CASE lexbuf.lex_curr_p}
  | "do"       {beg_state state;K_DO lexbuf.lex_curr_p}
  | "else"     {beg_state state;K_ELSE lexbuf.lex_curr_p}
  | "elsif"    {beg_state state;K_ELSIF lexbuf.lex_curr_p}
  | "END"      {beg_state state;K_END lexbuf.lex_curr_p}
  | "end"      {end_state state;K_lEND lexbuf.lex_curr_p}
  | "ensure"   {beg_state state;K_ENSURE lexbuf.lex_curr_p}
  | "for"      {beg_state state;K_FOR lexbuf.lex_curr_p}
  | "if"       {beg_state state;K_IF lexbuf.lex_curr_p}
  | "in"       {beg_state state;K_IN lexbuf.lex_curr_p}
  | "not"      {beg_state state;K_NOT lexbuf.lex_curr_p}
  | "or"       {beg_state state;K_OR lexbuf.lex_curr_p}
  | "rescue"   {beg_state state;K_RESCUE lexbuf.lex_curr_p}
  | "return"   {beg_state state;K_RETURN lexbuf.lex_curr_p}
  | "then"     {beg_state state;K_THEN lexbuf.lex_curr_p}
  | "unless"   {beg_state state;K_UNLESS lexbuf.lex_curr_p}
  | "until"    {beg_state state;K_UNTIL lexbuf.lex_curr_p}
  | "when"     {beg_state state;K_WHEN lexbuf.lex_curr_p}
  | "while"    {beg_state state;K_WHILE lexbuf.lex_curr_p}
  | "yield"    {mid_state state;K_YIELD lexbuf.lex_curr_p}
  | "nil"      {end_state state;K_NIL lexbuf.lex_curr_p}
  | "self"     {end_state state;K_SELF lexbuf.lex_curr_p}
  | "true"     {end_state state;K_TRUE lexbuf.lex_curr_p}
  | "false"    {end_state state;K_FALSE lexbuf.lex_curr_p}
(* No longer lex separately
  | "defined?" {mid_state state;K_DEFINED}
  | "super"    {mid_state state;K_SUPER}
  | "break"    {beg_state state;K_BREAK}
  | "redo"     {beg_state state;K_REDO}
  | "retry"    {beg_state state;K_RETRY}
  | "next"     {beg_state state;K_NEXT}
*)

  | "__END__" {end_lexbuf lexbuf}

  | ":" {on_end t_colon atom state lexbuf}

  | ws_re+ "::" { T_USCOPE lexbuf.lex_curr_p }
  | "::" { on_beg t_uscope t_scope state lexbuf }

  | ("@@" id_re) as id {end_state state; T_CLASS_VAR(id, lexbuf.lex_curr_p)}
  | ('@' id_re) as id {end_state state; T_INST_VAR(id, lexbuf.lex_curr_p)}

  | id_re as id { 
      let tok = choose_capital_for_id id lexbuf.lex_curr_p in
        begin match state.expr_state, tok with
          | Expr_Def, _ -> mid_state state
          | _, T_LID(id, pos) ->
              if NewParser.assigned_id id 
              then local_state state
              else mid_state state
          | _ -> mid_state state
        end;
        tok
    }

  | '$' {dollar state lexbuf}
  | ws_re+  {top_lexer state lexbuf}

and dollar state = parse
  | id_re as id {end_state state;T_GLOBAL_VAR("$"^id, lexbuf.lex_curr_p) }
  | ("-" alphanum_re) as v 
      {end_state state;T_BUILTIN_VAR("$"^v, lexbuf.lex_curr_p) }
  | ( ['0'-'9']+) as v 
      {end_state state;T_BUILTIN_VAR("$" ^ v, lexbuf.lex_curr_p)}
  | ( [^'a'-'z''A'-'Z''#'])
      {end_state state;T_BUILTIN_VAR("$"^(lexeme lexbuf), lexbuf.lex_curr_p)}

and space_tok tok binop state = parse
  | ws_re+ {binop state lexbuf}
  | nl_re  {incr_line lexbuf; binop state lexbuf}
  | e {on_def binop (on_local binop tok) state lexbuf}

and space_uop uop spc_uop binop state = parse
    (* space before and after is binop (unless at expr_beg) *)
  | ws_re+ {on_beg spc_uop binop state lexbuf}
  | nl_re  {incr_line lexbuf; on_beg spc_uop binop state lexbuf}

  | e 
      {match state.expr_state with
         | Expr_Def
         | Expr_Local
         | Expr_End -> binop state lexbuf
         | Expr_Beg | Expr_Mid -> uop state lexbuf
      }

and postfix_numeric cont start state = parse
  | "b" (['0''1''_']+ as num)
      { end_state state; 
        cont (convert_to_base10 ~base:2 num lexbuf.lex_curr_p)}

  | ('o'|'O') ((num_re|'_')+ as num)
      { end_state state; 
        cont (convert_to_base10 ~base:8 num lexbuf.lex_curr_p)}

  | "x" ((num_re|['a'-'f''A'-'F''_'])+ as num)
      { end_state state; 
        cont (convert_to_base10 ~base:16 num lexbuf.lex_curr_p)}

  | (num_re|'_')* as num
      { end_state state;
        if start = "0" 
        then cont (convert_to_base10 ~base:8 num lexbuf.lex_curr_p)
        else 
          let str = (start ^ num) in
          let num = to_bignum str in
          let tok = 
            if Big_int.is_int_big_int num
            then T_FIXNUM(Big_int.int_of_big_int num, lexbuf.lex_curr_p)
            else T_BIGNUM(num, lexbuf.lex_curr_p)
          in cont tok
      }

  | post_rubyfloat_re
      { end_state state;
        let str = (start ^ (lexeme lexbuf)) in
        let str = remove_underscores str in
        let tok = T_FLOAT(str, float_of_string (str), lexbuf.lex_curr_p) in
          cont tok
      }

and uop_minus_lit state = parse
  | num_re { postfix_numeric negate_numeric (lexeme lexbuf) state lexbuf}
  | e { t_uminus state lexbuf}

and uop_plus_lit state = parse
  | num_re { postfix_numeric Utils.id (lexeme lexbuf) state lexbuf}
  | e { t_uplus state lexbuf}

and postfix_at uop binop state = parse
  | '@' {uop state lexbuf}
  | e  {binop state lexbuf}

and atom state = parse
  | ['+' '-' '*' '/' '!' '~' '<' '>' '=' '&' '|' '%' '^' '@']+ 
  | '[' ']' ('='?) (* these can only appear together like this (I think) *)
  | '`' {def_end_state state; 
         T_ATOM((contents_of_str (lexeme lexbuf)), lexbuf.lex_curr_p) }

  | '$'
      { let str = match dollar state lexbuf with
          | T_GLOBAL_VAR(s,p) -> s
          | T_BUILTIN_VAR(s,p) -> s
          | _ -> assert false
        in def_end_state state; 
          T_ATOM(contents_of_str str, lexbuf.lex_curr_p) }

  | ('@'* id_re) '='?
      {def_end_state state; 
       T_ATOM((contents_of_str (lexeme lexbuf)), lexbuf.lex_curr_p) }
  | '"'  {def_end_state state;
          emit_extra (T_ATOM_BEG lexbuf.lex_curr_p)
            (interp_string_lexer '"') state lexbuf}

  | '\''  {def_end_state state;
           emit_extra (T_ATOM_BEG lexbuf.lex_curr_p)
             (non_interp_string '\'' (Buffer.create 31)) state lexbuf}

  | nl_re  {incr_line lexbuf; t_colon state lexbuf}

  | ws_re
  | e     {t_colon state lexbuf}

and char_code state = parse
  | e {beg_state state; char_code_work lexbuf}

and char_code_work = parse
  | [^'\n''\t'] as c {T_FIXNUM(Char.code c, lexbuf.lex_curr_p)}
  | "\\C-" (['a'-'z''A'-'Z'] as c)
      { T_FIXNUM((Char.code (Char.uppercase c)) - 64, lexbuf.lex_curr_p)}
  | "\\M-" (['a'-'z''A'-'Z'] as c)
      { T_FIXNUM((Char.code c) + 128, lexbuf.lex_curr_p)}
  | "\\M-\\C-" (['a'-'z''A'-'Z'] as c)
  | "\\C-\\M-" (['a'-'z''A'-'Z'] as c)
      { T_FIXNUM((Char.code (Char.uppercase c)) + 64, lexbuf.lex_curr_p)}

  | "\\\\" {T_FIXNUM(Char.code '\\', lexbuf.lex_curr_p)}
  | "\\s" {T_FIXNUM(Char.code ' ', lexbuf.lex_curr_p)}
  | "\\n" {T_FIXNUM(Char.code '\n', lexbuf.lex_curr_p)}
  | "\\t" {T_FIXNUM(Char.code '\t', lexbuf.lex_curr_p)}
  | "\\r" {T_FIXNUM(Char.code '\r', lexbuf.lex_curr_p)}
  | "\\(" {T_FIXNUM(Char.code '(', lexbuf.lex_curr_p)}
  | "\\)" {T_FIXNUM(Char.code ')', lexbuf.lex_curr_p)}

and type_annot state buf = parse
  | type_annot_re as line
       {incr_line lexbuf; 
        Buffer.add_string buf line;
        type_annot state buf lexbuf
       }

  | [' ''\t']+  {type_annot state buf lexbuf}

  | "class" as c
      { def_state state; 
        let marker = insert_delimiters c in
        let annot = marker ^ Buffer.contents buf in
          K_CLASS (annot, lexbuf.lex_curr_p) 
      }
  | "def" as def
      { def_state state;
        let marker = insert_delimiters def in
        let annot = marker ^ Buffer.contents buf in
          K_DEF (annot, lexbuf.lex_curr_p) }
  | "module" as m   
      {def_state state;
        let marker = insert_delimiters m in
        let annot = marker ^ Buffer.contents buf in
          K_MODULE (annot, lexbuf.lex_curr_p)
      }

and comment state = parse
  | [^'\n']* '\n' { incr_line lexbuf;t_eol state lexbuf }

and delim_comment state = parse
  | "=end" [^'\n']* '\n' { incr_line lexbuf;top_lexer state lexbuf}
  | [^'\n']* '\n' { incr_line lexbuf; delim_comment state lexbuf}

and non_interp_string delim buf state = parse
  | eof   {failwith "eof in string"}

  | '\\'? '\n' {incr_line lexbuf; 
                Buffer.add_string buf (lexeme lexbuf);
                non_interp_string delim buf state lexbuf}
  | "\\" _ {Buffer.add_string buf (lexeme lexbuf);
            non_interp_string delim buf state lexbuf}
  | _ as c
      {if c == delim 
       then T_SINGLE_STRING(Buffer.contents buf,lexbuf.lex_curr_p)
       else (Buffer.add_char buf c; non_interp_string delim buf state lexbuf)
      }

and double_string state = parse 
  | e {end_state state;
       emit_extra (T_DOUBLE_BEG lexbuf.lex_curr_p)
         (interp_string_lexer '"') state lexbuf}

and tick_string state = parse 
  | e {end_state state;
       emit_extra (T_TICK_BEG lexbuf.lex_curr_p)
         (interp_string_lexer '`') state lexbuf}

and regexp_string buf state = parse
  | e {regexp_delim ((==)'/') ((==)'/') buf state lexbuf}

and regexp_delim delim_f escape_f buf state = parse 
  | e {
      let k state lexbuf = 
        pop_lexer state;
        regexp_modifier state lexbuf
      in
        Stack.push k state.lexer_stack;
        end_state state;
        emit_extra (T_REGEXP_BEG lexbuf.lex_curr_p)
          (interp_lexer fail_eof delim_f escape_f buf) state lexbuf
    }

and regexp_modifier state = parse
  | (alpha_re* as modifiers) {T_REGEXP_MOD modifiers}

and percent state = parse
  | (alpha_re? as modifier)(string_single_delim as d)
      {end_state state; 
       let f = 
         if modifier_is_single modifier
         then non_interp_string d (Buffer.create 31)
         else interp_string_lexer d
       in
         if modifier = "r"
         then regexp_delim ((==)d) ((==)d) (Buffer.create 31) state lexbuf
         else emit_extra (T_USER_BEG(modifier, lexbuf.lex_curr_p)) f state lexbuf
      }

  | (alpha_re? as modifier) (['{' '<' '(' '['] as d_start)
      {end_state state; 
       let d_end = close_delim d_start in
       let level = ref 0 in
       let at_end d = 
         if d = d_end then
           if !level = 0 then true
           else (decr level; false)
         else
           if d = d_start
           then (incr level; false)
           else false
       in
       let chk d = d == d_start || d == d_end in
       let f = 
         if modifier_is_single modifier
         then non_interp_string d_end (Buffer.create 31)
         else interp_lexer fail_eof at_end chk (Buffer.create 31)
       in
         if modifier = "r"
         then regexp_delim at_end chk (Buffer.create 31) state lexbuf
         else emit_extra (T_USER_BEG(modifier, lexbuf.lex_curr_p)) f state lexbuf
      }

  | e {t_percent state lexbuf}

and regexp state = parse
  | e {regexp_string (Buffer.create 31) state lexbuf}

and heredoc_header lead_f state = parse
  | '\''([^'\'']+ as delim)'\'' ([^'\n']* nl_re as rest)
      {incr_line lexbuf;
       let cont str state lexbuf = 
         end_state state;
         push_back rest lexbuf;
         T_SINGLE_STRING(str, lexbuf.lex_curr_p)
       in
          lead_f cont (Buffer.create 31) delim state lexbuf
      }

  | '"'([^'"']+ as delim)'"' ([^'\n']* nl_re as rest)
  | (id_body+ as delim) ([^'\n']* nl_re as rest)
      {incr_line lexbuf;
       let pos = lexbuf.lex_curr_p in
       let cont str state future_lexbuf = 
         let str_lexbuf = Lexing.from_string str in
           str_lexbuf.lex_curr_p <- pos;
           push_back rest future_lexbuf;
           interp_heredoc_lexer state str_lexbuf
       in
         emit_extra 
           (T_DOUBLE_BEG lexbuf.lex_curr_p)
            (lead_f cont (Buffer.create 31) delim) state lexbuf
      }
      
  | e {beg_state state; T_LSHFT lexbuf.lex_curr_p}

and heredoc_string cont buf delim state = parse (* for <<EOF *)
  | eof {fail_eof (heredoc_string cont buf delim) state lexbuf}
  | ([^'\n']* as tok) ('\n'|eof)
      { incr_line lexbuf;
        if tok = delim
        then cont (Buffer.contents buf) state lexbuf
        else begin 
          Buffer.add_string buf (lexeme lexbuf);
          heredoc_string cont buf delim state lexbuf
        end}

and heredoc_string_lead cont buf delim state = parse (* for <<-EOF *)
  | eof {fail_eof (heredoc_string_lead cont buf delim) state lexbuf}
  | [' ''\t']* ([^'\n']* as tok) ('\n'|eof)
      { incr_line lexbuf;
        if tok = delim
        then cont (Buffer.contents buf) state lexbuf
        else begin 
          Buffer.add_string buf (lexeme lexbuf);
          heredoc_string_lead cont buf delim state lexbuf
        end}

and interp_heredoc_lexer state = parse
  | e {let f buf state lexbuf = 
         end_state state;
         T_INTERP_END(Buffer.contents buf,lexbuf.lex_curr_p)
       in 
       let nope _  = false in
         interp_lexer f nope nope (Buffer.create 31) state lexbuf
      }

and interp_string_lexer delim state = parse
  | e {let chk x = x == delim in
         interp_lexer fail_eof chk chk (Buffer.create 31) state lexbuf}

(* tokenize a interpreted string into string / code *)
and interp_lexer do_eof delim_f escape_f buf state = parse
  | eof {do_eof buf state lexbuf}

  | "\\\n" {incr_line lexbuf; interp_lexer do_eof delim_f escape_f buf state lexbuf}
  | '\n' {incr_line lexbuf; Buffer.add_char buf '\n';
          interp_lexer do_eof delim_f escape_f buf state lexbuf}
      
  | "\\" (_ as c)
      {if escape_f c
       then Buffer.add_char buf c
       else Buffer.add_string buf (lexeme lexbuf);
       interp_lexer do_eof delim_f escape_f buf state lexbuf}

  | "#{" {beg_state state;
          let tok = T_INTERP_STR(Buffer.contents buf,lexbuf.lex_curr_p) in
          let k state lexbuf =
            interp_lexer do_eof delim_f escape_f (Buffer.create 31) state lexbuf
          in
            interp_code tok k state lexbuf
         }

  | _ as c
      {if delim_f c 
       then T_INTERP_END(Buffer.contents buf, lexbuf.lex_curr_p)
       else begin
         Buffer.add_char buf c; 
         interp_lexer do_eof delim_f escape_f buf state lexbuf
       end
      }

and interp_code start cont state = parse
  | e {beg_state state;
       let level = ref 0 in
         (* a continuation to read in Ruby tokens until we see an
            unbalanced '}', at which point we abort that continuation
            and restart the [cont] function *)
       let k state future_lexbuf = 
         match top_lexer state lexbuf with
           | T_LBRACE _ | T_LBRACE_ARG _ as tok -> 
               incr level; tok
                 
           | T_RBRACE _ as tok -> 
               if !level == 0 then begin
                 pop_lexer state; (* abort k *)
                 cont state lexbuf
               end else begin
                 decr level;
                 tok
               end
           | tok -> tok
       in
         Stack.push k state.lexer_stack;
         start
      }

and end_lexbuf = parse
  | eof   {T_EOF}
  | _ {end_lexbuf lexbuf}

