open Ast_ruby
module H = Ast_ruby_helpers
module Utils = Utils_ruby
open Format

let format_binop ppf op = pp_print_string ppf (H.str_binop op)

let rec format_id ppf kind s = match kind with
  | ID_Lowercase -> fprintf ppf "lvar(%s)" s
  | ID_Instance -> fprintf ppf "ivar(%s)" s
  | ID_Class -> fprintf ppf "cvar(%s)" s
  | ID_Global -> fprintf ppf "gvar(%s)" s
  | ID_Uppercase -> fprintf ppf "uvar(%s)" s
  | ID_Builtin -> fprintf ppf "bvar(%s)" s
  | ID_Assign k -> format_id ppf k (s ^ "=")

let rec format_string_contents ppf = function
  | StrChars s -> pp_print_string ppf s
  | StrExpr  e -> fprintf ppf "#{%a}" format_expr e

and format_interp_string ppf sc = 
  List.iter (fprintf ppf "i(%a)" format_string_contents) sc

and format_string_kind ppf = function
  | Single s -> fprintf ppf "'%s'" s
  | Double sc -> fprintf ppf "\"%a\"" format_interp_string sc
  | Tick sc -> fprintf ppf "`%a`" format_interp_string sc

and format_lit_kind ppf kind = match kind with
  | Num i -> fprintf ppf "%s" i
  | Float(s) -> fprintf ppf "%s" s
  | String (skind) -> format_string_kind ppf skind
  | Atom sc -> fprintf ppf ":%a" format_interp_string sc
  | Regexp (str,m) -> fprintf ppf "Regexp(@[%a@],%s)" format_interp_string str m
  | Nil -> pp_print_string ppf "nil"
  | Self -> pp_print_string ppf "self"
  | True -> pp_print_string ppf "true"
  | False -> pp_print_string ppf "false"
    
and format_expr ppf expr = match expr with
  | S Empty -> fprintf ppf "<empty>"
  | Literal(kind,_) -> format_lit_kind ppf kind
  | D Alias (_, e1,e2) -> 
      fprintf ppf "alias %a %a"
	format_expr e1
	format_expr e2

  | D Undef (_, e1) -> 
      fprintf ppf "undef %a"
	(Utils.format_comma_list format_expr) e1

  | Id((string,_p), id_kind) -> format_id ppf id_kind string

  | Unary((uop,_), expr) ->
      fprintf ppf "uop(%s,%a)"
	(H.str_uop uop) format_expr expr

  | Binop(expr1, (binary_op,_), expr2) ->
      begin match binary_op with
	| Op_SCOPE
	| Op_DOT -> fprintf ppf "%a%a%a" 
	| _o -> match expr1,expr2 with
	    | Binop(_,_,_),_
	    | _, Binop(_,_,_) -> fprintf ppf "(@[%a %a@ %a@])"
	    | _ ->fprintf ppf "(%a %a %a)"
      end
	format_expr expr1
	format_binop binary_op

	format_expr expr2
  | Operator(bop,_) -> format_binop ppf bop
  | UOperator(uop,_) -> pp_print_string ppf (H.str_uop uop)

  | Hash(_,(_, el,_)) -> fprintf ppf "{@[%a@]}" format_expr_comma_list el
  | Array(_, el,_) -> fprintf ppf "[@[%a@]]" format_expr_comma_list el

  | Tuple(el,_) -> fprintf ppf "Tup(@[%a@])" format_expr_comma_list el

  | S Return(_, el) -> fprintf ppf "return(@[%a@])" format_expr_comma_list el
  | S Yield(_, el) -> fprintf ppf "yield(@[%a@])" format_expr_comma_list el

  | S Block(el,_) -> fprintf ppf "@[<v 2>block(%a)@]" format_expr_break_list el

  | Call (expr1, expr_list, eo,_) ->
      begin match eo with
	| None -> 
	    fprintf ppf "MC(%a(@[%a@]))"
	      format_expr expr1
	      format_expr_comma_list expr_list
	| Some (CodeBlock(b,_,_,_) as cb) -> 
	    let fmt = 
	      if b 
	      then fprintf ppf "@[<v 0>@[<v 2>MC(%a(@[%a@]) { @,%a@]@,})@]"
	      else fprintf ppf "@[<v 0>@[<v 2>MC(%a(@[%a@]) do @,%a@]@,end)@]"
	    in 
	      fmt format_expr expr1
		format_expr_comma_list expr_list
		format_expr cb
	| Some _ -> assert false
      end
	
  | Ternary (expr1, _, expr2, _, expr3) -> 
      fprintf ppf "@[(%a ? %a : %a)@]"
	format_expr expr1
	format_expr expr2
	format_expr expr3

  | S While (_, b, expr, body) -> format_loop ppf "while" b expr body
  | S Until (_, b, expr, body) -> format_loop ppf "until" b expr body

  | S For ( _, formals, expr, body) -> 
      fprintf ppf "@[<v 0>@[<v 2>for %a in %a@,%a@]@;end@]"
	format_formals formals
	format_expr expr
	format_expr_break_list body

  | D ModuleDef (_, name,body) -> 
      fprintf ppf "@[<v 0>@[<v 2>module %a@,%a@]@,"
	format_expr name
	format_expr_break_list body.body_exprs;
      format_rescues ppf body.rescue_exprs;
      format_ensure ppf body.ensure_expr;
      format_else ppf body.else_expr;
      fprintf ppf "end@]"

  | D MethodDef (_, mname,formals,body) ->
      fprintf ppf "@[<v 0>@[<v 2>def %a(@[%a@])@,%a@]@,"
	format_expr mname
	format_formals formals
	format_expr_break_list body.body_exprs;
      format_rescues ppf body.rescue_exprs;
      format_ensure ppf body.ensure_expr;
      format_else ppf body.else_expr;
      fprintf ppf "end@]"      
  | CodeBlock(_,formals,exps,_) -> 
      begin match formals with
        | None 
	| Some [] -> fprintf ppf "%a" format_expr_break_list exps
	| Some lst-> fprintf ppf "|%a|@ %a" format_formals lst 
	    format_expr_break_list exps
      end 

  | D BeginBlock(_, el) -> 
      fprintf ppf "@[<v 0>@[<v 2>BEGIN {@,%a@]@,}@]"
	format_expr_break_list el
  | D EndBlock(_, el) -> 
      fprintf ppf "@[<v 0>@[<v 2>END {@,%a@],}@]"
	format_expr_break_list el

  | S Case(c,_) -> format_case ppf c

  | D ClassDef(_, name,inh,body) -> 
      fprintf ppf "@[<v 0>@[<v 2>class %a %a@,%a@]@,"
	format_expr name
	format_inheritance inh
	format_expr_break_list body.body_exprs;
      format_rescues ppf body.rescue_exprs;
      format_ensure ppf body.ensure_expr;
      format_else ppf body.else_expr;
      fprintf ppf "end@]"      
	
  | S ExnBlock (body,_) -> 
      fprintf ppf "@[<v 0>@[<v 2>begin@,%a@]@,"
	format_expr_break_list body.body_exprs;
      format_rescues ppf body.rescue_exprs;
      format_else ppf body.else_expr;
      format_ensure ppf body.ensure_expr;
      fprintf ppf "end@]"      
	
  | S Unless(_, guard,then_e,else_e) ->
      fprintf ppf "@[<v 0>@[<v 2>unless (%a) then@,%a@]@,"
	format_expr guard
	format_expr_break_list then_e;
      if else_e <> [] then
	fprintf ppf "@[<v 2>else@,%a@]@," format_expr_break_list else_e;
      fprintf ppf "end@]"

  | S If(_, guard,then_e,else_e) ->
      fprintf ppf "@[<v 0>@[<v 2>if (%a) then@,%a@]@,"
	format_expr guard
	format_expr_break_list then_e;
      if else_e <> [] then
	fprintf ppf "@[<v 2>else@,%a@]@," format_expr_break_list else_e;
      fprintf ppf "end@]"

and format_expr_comma_list ppf = function
  | [] -> ()
  | hd::[] -> format_expr ppf hd
  | hd::tl -> format_expr ppf hd;
      List.iter (fprintf ppf ",@ %a" format_expr) tl

and format_expr_break_list ppf = function
  | [] -> ()
  | hd::[] -> format_expr ppf hd
  | hd::tl -> format_expr ppf hd;
      List.iter (fprintf ppf "@,@[%a@]" format_expr) tl

and format_loop ppf lname is_do expr body = 
  if is_do then begin match body with
    | [S ExnBlock _ as b] ->
	fprintf ppf "@[<v 0>%a @[<v 0>%s %a@]@]"
	  format_expr b
	  lname
	  format_expr expr
    | _ -> assert false
  end
  else
    fprintf ppf "@[<v 0>@[<v 2>%s %a@,%a@]@,end@]"
      lname
      format_expr expr
      format_expr_break_list body

and format_formals ppf = function
  | [] -> ()
  | hd::[] -> format_formal ppf hd
  | l -> List.iter (fprintf ppf "@[%a@],@," format_formal) l

and format_rescues ppf = function
  | [] -> ()
  | lst -> 
      List.iter
	(fun (r,b) -> 
	   fprintf ppf "@[<v 2>rescue %a@,%a@]@," format_expr r format_expr b
	) lst

and format_ensure ppf = function
  | [] -> ()
  | e -> fprintf ppf "@[<v 2>ensure@,%a@]@," format_expr_break_list e

and format_else ppf = function
  | [] -> ()
  | e -> fprintf ppf "@[<v 2>else@,%a@]@," format_expr_break_list e

and format_inheritance ppf inh = match inh with
  | None -> ()
  | Some e -> match e with
      | Class_Inherit e -> fprintf ppf "@[ < %a @]" format_expr e
      | Inst_Inherit e -> fprintf ppf "@[ << %a @]" format_expr e

and format_formal ppf = function
  | Formal_id e -> format_expr ppf e
  | Formal_amp (_, (s,_)) -> fprintf ppf "&%s" s
  | Formal_star (_, (s, _)) -> fprintf ppf "*%s" s
  | Formal_rest _ -> fprintf ppf "*"
  | Formal_tuple t -> fprintf ppf "(@[%a@])" format_formals t
  | Formal_default((f, _),_, e) -> 
      fprintf ppf "@[%s = @[%a@]@]"
	f format_expr e

and format_case ppf c = 
  fprintf ppf "@[<v 0>case %a@,%a"
    format_expr c.case_guard
    format_whens c.case_whens;
  if c.case_else <> []
  then fprintf ppf "@[<v 2>else@,%a@]@," format_expr_break_list c.case_else;
  fprintf ppf "end@]"

and format_whens ppf whens = 
  List.iter
    (fun (guards,body) -> 
       fprintf ppf "@[<hv 2>when @[%a@] then@, %a@]@,"
	 format_expr_comma_list guards
	 format_expr_break_list body
    ) whens

let format_ast ppf ast = 
  fprintf ppf "@[<v 0>%a@]" format_expr_break_list ast



let string_of_expr e = 
  Utils.format_to_string format_expr e

let string_of_ast ast = 
  Utils.format_to_string format_ast ast

