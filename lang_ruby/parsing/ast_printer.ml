
open Ast
open Format
open Utils

let format_uop ppf op = pp_print_string ppf (str_uop op)

let format_binop ppf op = pp_print_string ppf (str_binop op)
(*
let format_string_kind ppf = function
  | String_Single _ -> pp_print_string ppf "Single"
  | String_Double _ -> pp_print_string ppf "Double"
(*  | String_User -> pp_print_string ppf "User"*)
  | String_Tick _ -> pp_print_string ppf "Tick"
  | String_Heredoc _ -> pp_print_string ppf "Heredoc"
*)

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
  | String_Single s -> fprintf ppf "'%s'" s
  | String_Double sc -> fprintf ppf "\"%a\"" format_interp_string sc
  | String_Tick sc -> fprintf ppf "`%a`" format_interp_string sc

and format_lit_kind ppf kind = match kind with
  | Lit_FixNum i -> fprintf ppf "%d" i
  | Lit_BigNum big -> 
      fprintf ppf "%s" (Big_int.string_of_big_int big)
  | Lit_Float(s,f) -> fprintf ppf "%s" s
  | Lit_String (skind) -> format_string_kind ppf skind
  | Lit_Atom sc -> fprintf ppf ":%a" format_interp_string sc
  | Lit_Regexp (str,m) -> fprintf ppf "Regexp(@[%a@],%s)" format_interp_string str m
  | Lit_Nil -> pp_print_string ppf "nil"
  | Lit_Self -> pp_print_string ppf "self"
  | Lit_True -> pp_print_string ppf "true"
  | Lit_False -> pp_print_string ppf "false"
    
and format_expr ppf expr = match expr with
  | E_Empty -> fprintf ppf "<empty>"
  | E_Literal(kind,_) -> format_lit_kind ppf kind
  | E_Alias (e1,e2,_) -> 
      fprintf ppf "alias %a %a"
	format_expr e1
	format_expr e2

  | E_Undef (e1,_) -> 
      fprintf ppf "undef %a"
	(format_comma_list format_expr) e1

  | E_Identifier(id_kind,string,p) -> format_id ppf id_kind string

  | E_Unary(uop, expr,_) ->
      fprintf ppf "uop(%s,%a)"
	(str_uop uop) format_expr expr

  | E_Binop(expr1, binary_op, expr2,_) ->
      begin match binary_op with
	| Op_SCOPE
	| Op_DOT -> fprintf ppf "%a%a%a" 
	| o -> match expr1,expr2 with
	    | E_Binop(_,_,_,_),_
	    | _, E_Binop(_,_,_,_) -> fprintf ppf "(@[%a %a@ %a@])"
	    | _ ->fprintf ppf "(%a %a %a)"
      end
	format_expr expr1
	format_binop binary_op

	format_expr expr2
  | E_Operator(bop,_) -> format_binop ppf bop
  | E_UOperator(uop,_) -> pp_print_string ppf (str_uop uop)

  | E_Hash(_,el,_) -> fprintf ppf "{@[%a@]}" format_expr_comma_list el
  | E_Array(el,_) -> fprintf ppf "[@[%a@]]" format_expr_comma_list el

  | E_Tuple(el,_) -> fprintf ppf "Tup(@[%a@])" format_expr_comma_list el

  | E_Return(el,_) -> fprintf ppf "return(@[%a@])" format_expr_comma_list el
  | E_Yield(el,_) -> fprintf ppf "yield(@[%a@])" format_expr_comma_list el

  | E_Block(el,_) -> fprintf ppf "@[<v 2>block(%a)@]" format_expr_break_list el

  | E_MethodCall (expr1, expr_list, eo,_) ->
      begin match eo with
	| None -> 
	    fprintf ppf "MC(%a(@[%a@]))"
	      format_expr expr1
	      format_expr_comma_list expr_list
	| Some (E_CodeBlock(b,_,_,_) as cb) -> 
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
	
  | E_Ternary (expr1, expr2, expr3,_) -> 
      fprintf ppf "@[(%a ? %a : %a)@]"
	format_expr expr1
	format_expr expr2
	format_expr expr3

  | E_While (b, expr, body,_) -> format_loop ppf "while" b expr body
  | E_Until (b, expr, body,_) -> format_loop ppf "until" b expr body

  | E_For ( formals, expr, body,_) -> 
      fprintf ppf "@[<v 0>@[<v 2>for %a in %a@,%a@]@;end@]"
	format_formals formals
	format_expr expr
	format_expr_break_list body

  | E_ModuleDef (name,body, _) -> 
      fprintf ppf "@[<v 0>@[<v 2>module %a@,%a@]@,"
	format_expr name
	format_expr_break_list body.body_exprs;
      format_rescues ppf body.rescue_exprs;
      format_ensure ppf body.ensure_expr;
      format_else ppf body.else_expr;
      fprintf ppf "end@]"

  | E_MethodDef (mname,formals,body, _) ->
      fprintf ppf "@[<v 0>@[<v 2>def %a(@[%a@])@,%a@]@,"
	format_expr mname
	format_formals formals
	format_expr_break_list body.body_exprs;
      format_rescues ppf body.rescue_exprs;
      format_ensure ppf body.ensure_expr;
      format_else ppf body.else_expr;
      fprintf ppf "end@]"      
  | E_CodeBlock(_,formals,exps,_) -> 
      begin match formals with
        | None 
	| Some [] -> fprintf ppf "%a" format_expr_break_list exps
	| Some lst-> fprintf ppf "|%a|@ %a" format_formals lst 
	    format_expr_break_list exps
      end 

  | E_BeginBlock(el,_) -> 
      fprintf ppf "@[<v 0>@[<v 2>BEGIN {@,%a@]@,}@]"
	format_expr_break_list el
  | E_EndBlock(el,_) -> 
      fprintf ppf "@[<v 0>@[<v 2>END {@,%a@],}@]"
	format_expr_break_list el

  | E_Case(c,_) -> format_case ppf c

  | E_ClassDef(name,inh,body, _) -> 
      fprintf ppf "@[<v 0>@[<v 2>class %a %a@,%a@]@,"
	format_expr name
	format_inheritance inh
	format_expr_break_list body.body_exprs;
      format_rescues ppf body.rescue_exprs;
      format_ensure ppf body.ensure_expr;
      format_else ppf body.else_expr;
      fprintf ppf "end@]"      
	
  | E_ExnBlock (body,_) -> 
      fprintf ppf "@[<v 0>@[<v 2>begin@,%a@]@,"
	format_expr_break_list body.body_exprs;
      format_rescues ppf body.rescue_exprs;
      format_else ppf body.else_expr;
      format_ensure ppf body.ensure_expr;
      fprintf ppf "end@]"      
	
  | E_Unless(guard,then_e,else_e,_) ->
      fprintf ppf "@[<v 0>@[<v 2>unless (%a) then@,%a@]@,"
	format_expr guard
	format_expr_break_list then_e;
      if else_e <> [] then
	fprintf ppf "@[<v 2>else@,%a@]@," format_expr_break_list else_e;
      fprintf ppf "end@]"

  | E_If(guard,then_e,else_e,_) ->
      fprintf ppf "@[<v 0>@[<v 2>if (%a) then@,%a@]@,"
	format_expr guard
	format_expr_break_list then_e;
      if else_e <> [] then
	fprintf ppf "@[<v 2>else@,%a@]@," format_expr_break_list else_e;
      fprintf ppf "end@]"

  | E_Annotate(e,te,pos) -> 
      fprintf ppf "%a@,%a" Annotation.format_annotation te format_expr e

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
    | [E_ExnBlock _ as b] ->
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

and format_string_list ppf list = 
  List.iter (fun s -> fprintf ppf "@[%s@],@," s) list

and format_formal ppf = function
  | Formal_id e -> format_expr ppf e
  | Formal_amp s -> fprintf ppf "&%s" s
  | Formal_star s -> fprintf ppf "*%s" s
  | Formal_rest -> fprintf ppf "*"
  | Formal_tuple t -> fprintf ppf "(@[%a@])" format_formals t
  | Formal_default(f,e) -> 
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

let print_ast ast = 
  format_ast std_formatter ast;
  pp_print_newline std_formatter ()

let string_of_expr e = 
  format_to_string format_expr e

let string_of_ast ast = 
  format_to_string format_ast ast

