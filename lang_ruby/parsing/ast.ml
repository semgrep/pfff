
type pos = Lexing.position

type string_contents = 
  | StrChars of string
  | StrExpr of expr

and interp_string = string_contents list

and formal_param = 
  | Formal_id of expr
  | Formal_amp of string
  | Formal_star of string (* as in *x *)
  | Formal_rest (* just '*' *)
  | Formal_tuple of formal_param list
  | Formal_default of string * expr

and string_kind = 
  | String_Single of string
  | String_Double of interp_string
  | String_Tick of interp_string

and inheritance_kind = 
  | Class_Inherit of expr
  | Inst_Inherit of expr

and body_exn = {
  body_exprs: expr list;
  rescue_exprs: (expr*expr) list;
  ensure_expr: expr list;
  else_expr: expr list;
}

and case_block = {
  case_guard : expr;
  case_whens: (expr list * expr list) list;
  case_else: expr list;
}
and expr = 
  | E_Empty
  | E_Literal of lit_kind * pos
  | E_Alias of expr * expr* pos
  | E_Undef of expr list * pos
  | E_Identifier of id_kind * string * pos
  | E_Unary of unary_op * expr * pos
  | E_Binop of expr * binary_op * expr * pos
  | E_Ternary of expr * expr * expr * pos
  | E_Hash of bool * expr list * pos
  | E_Array of expr list * pos
  | E_Tuple of expr list * pos
  | E_MethodCall of expr * expr list * expr option * pos
  | E_While of bool * expr * expr list * pos
  | E_Until of bool * expr * expr list * pos
  | E_Unless of expr * expr list * expr list * pos
  | E_For of formal_param list * expr * expr list * pos
  | E_If of expr * expr list * expr list * pos
  | E_ModuleDef of expr * body_exn * pos
  | E_MethodDef of expr * formal_param list * body_exn * pos
  | E_CodeBlock of bool * formal_param list option * expr list * pos
  | E_ClassDef of expr * inheritance_kind option * body_exn * pos
  | E_BeginBlock of expr list * pos
  | E_EndBlock of expr list * pos
  | E_ExnBlock of body_exn * pos
  | E_Case of case_block * pos
  | E_Operator of binary_op * pos
  | E_UOperator of unary_op * pos
  | E_Return of expr list * pos
  | E_Yield of expr list * pos
  | E_Block of expr list * pos
  | E_Annotate of expr * Annotation.t * pos

and lit_kind = 
  | Lit_FixNum of int
  | Lit_BigNum of Big_int.big_int
  | Lit_Float of string * float
  | Lit_String of string_kind
  | Lit_Atom of interp_string
  | Lit_Regexp of interp_string * string
  | Lit_Nil
  | Lit_Self
  | Lit_True
  | Lit_False

and id_kind = 
  | ID_Lowercase (* prefixed by [a-z] or _ *)
  | ID_Instance  (* prefixed by @ *)
  | ID_Class     (* prefixed by @@ *)
  | ID_Global    (* prefixed by $ *)
  | ID_Uppercase (* prefixed by [A-Z] *)
  | ID_Builtin   (* prefixed by $, followed by non-alpha *)
  | ID_Assign of id_kind (* postfixed by = *)

and unary_op = 
  | Op_UMinus    (* -x *)
  | Op_UPlus     (* +x *)
  | Op_UBang     (* !x *)
  | Op_UTilde    (* ~x *)
  | Op_UNot      (* not x *)
(*  | Op_UDefined*)  (* defined? x *)
  | Op_UAmper    (* & *)
  | Op_UStar     (* * *)
  | Op_UScope    (* ::x *)


and binary_op = 
  | Op_ASSIGN   (* = *)
  | Op_PLUS     (* + *)
  | Op_MINUS    (* - *)
  | Op_TIMES    (* * *)
  | Op_REM      (* % *)
  | Op_DIV      (* / *)
  | Op_CMP  	(* <=> *)
  | Op_EQ  	(* == *)
  | Op_EQQ  	(* === *)
  | Op_NEQ  	(* != *)
  | Op_GEQ  	(* >= *)
  | Op_LEQ  	(* <= *)
  | Op_LT       (* < *)
  | Op_GT       (* > *)
  | Op_AND      (* && *)
  | Op_OR	(* || *)
  | Op_BAND     (* & *)
  | Op_BOR      (* | *)
  | Op_MATCH    (* =~ *)
  | Op_NMATCH   (* !~ *)
  | Op_XOR      (* ^ *)
  | Op_POW      (* ** *)
  | Op_kAND     (* and *)
  | Op_kOR      (* or *)
  | Op_ASSOC    (* => *)
  | Op_DOT      (* . *)
  | Op_SCOPE    (* :: *)
  | Op_AREF     (* [] *)
  | Op_ASET     (* []= *)
  | Op_LSHIFT   (* < < *)
  | Op_RSHIFT   (* > > *)
(*  | Op_Custom of string*)   (* ?? *)
  | Op_OP_ASGN of binary_op  (* +=, -=, ... *)
  | Op_DOT2     (* .. *)
  | Op_DOT3     (* ... *)


type ast = expr list

module Abbr = struct
  
  let fixnum i pos = E_Literal(Lit_FixNum i, pos)
  let float f pos = E_Literal(Lit_Float(string_of_float f, f), pos)

  let ltrue pos = E_Literal(Lit_True,pos)
  let lfalse pos = E_Literal(Lit_False,pos)
  let lself pos = E_Literal(Lit_Self,pos)
  let lnil pos = E_Literal(Lit_Nil,pos)

  let ident x pos =
    let len = String.length x in
      if len = 0 then
        E_Identifier(ID_Uppercase, "", pos)
      else
        let kind = match x.[0] with
	  | 'a'..'z' | '_' -> ID_Lowercase
	  | '@' -> if x.[1] = '@' then ID_Class else ID_Instance
	  | '$' -> ID_Global
	  | 'A'..'Z' -> ID_Uppercase
	  | _ -> raise (Invalid_argument "ast_id")
        in
	  if x.[len-1] = '=' then
	    E_Identifier(ID_Assign kind, String.sub x 0 (len-1), pos)
	  else      
	    E_Identifier(kind, x, pos)

  let str kind pos = E_Literal((Lit_String kind),pos)
  let single_str s = str (String_Single s)
  let double_str s = str (String_Double [StrChars s])
  let tick_str s = str (String_Tick [StrChars s])
  let regexp s m pos = E_Literal(Lit_Regexp([StrChars s],m),pos)
  let atom s pos = E_Literal(Lit_Atom([StrChars s]),pos)

  let scoped_ident lst pos = 
    let rec work = function
      | [] -> assert false
      | [x] -> ident x pos
      | x::(_::_ as rest) -> 
          E_Binop(work rest,Op_SCOPE,ident x pos,pos)
    in work (List.rev lst)

  let mcall targ args ?cb pos = E_MethodCall(targ,args,cb,pos)

  let cb ?args body pos = E_CodeBlock(true,args,body,pos)


  let dp = Lexing.dummy_pos
end

let pcompare = Pervasives.compare

let cmp2 = Utils.cmp2
let cmp_list = Utils.cmp_list

let cmp_opt f x y = match x,y with
  | None, None -> 0
  | Some _, None -> -1
  | None, Some _ -> 1
  | Some e1, Some e2 -> f e1 e2

let rec cmp_expr e1 e2 = match e1,e2 with
  | E_ModuleDef(e1,body1, _), E_ModuleDef(e2,body2, _) -> 
      cmp2 (cmp_expr e1 e2) cmp_body_exn body1 body2

  | E_Identifier(k1,s1,_), E_Identifier(k2,s2,_) -> 
      cmp2 (pcompare k1 k2) pcompare s1 s2

  | E_Empty, E_Empty -> 0
  | E_Literal(k1,_), E_Literal(k2,_) -> cmp_lit k1 k2

  | E_Alias(e11,e12,_), E_Alias(e21,e22,_) -> 
      cmp2 (cmp_expr e11 e21) cmp_expr e12 e22

  | E_Undef(e1,__), E_Undef(e2,_) -> cmp_expr_list e1 e2

  | E_Unary(u1,e1,_), E_Unary(u2,e2,_) ->
      cmp2 (pcompare u1 u2) cmp_expr e1 e2

  | E_Binop(e11,o1,e12,_), E_Binop(e21,o2,e22,_) ->
      cmp2 (cmp2 (pcompare o1 o2) cmp_expr e11 e21) cmp_expr e12 e22

  | E_UOperator(u1,_), E_UOperator(u2,_) -> pcompare u1 u2
  | E_Operator(b1,_), E_Operator(b2,_) -> pcompare b1 b2

  | E_Hash (b1,el1,_), E_Hash (b2,el2,_) ->
      cmp2 (pcompare b1 b2) cmp_expr_list el1 el2

  | E_Return(el1,_), E_Return(el2,_)
  | E_Yield(el1,_), E_Yield (el2,_)
  | E_Block(el1,_), E_Block (el2,_)
  | E_Array (el1,_), E_Array (el2,_)
  | E_Tuple (el1,_), E_Tuple (el2,_)
  | E_BeginBlock (el1,_), E_BeginBlock (el2,_)
  | E_EndBlock (el1,_), E_EndBlock (el2,_) -> cmp_expr_list el1 el2

  | E_ExnBlock(body1,_), E_ExnBlock(body2,_) ->
      cmp_body_exn body1 body2

  | E_Ternary(e11,e12,e13,_), E_Ternary(e21,e22,e23,_) ->
      cmp2 (cmp2 (cmp_expr e11 e21) cmp_expr e12 e22) cmp_expr e13 e23

  | E_For(fl1,e1,el1,_),E_For(fl2,e2,el2,_) ->
      cmp2 (cmp2 (cmp_list cmp_formal fl1 fl2) cmp_expr e1 e2) cmp_expr_list el1 el2

  | E_While(b1,e1,el1,_), E_While(b2,e2,el2,_) ->
      cmp2 (cmp2 (cmp_expr e1 e2) cmp_expr_list el1 el2) pcompare b1 b2
  | E_Until(b1,e1,el1,_), E_Until(b2,e2,el2,_) ->
      cmp2 (cmp2 (cmp_expr e1 e2) cmp_expr_list el1 el2) pcompare b1 b2

  | E_MethodCall (e1,el1,eo1,_), E_MethodCall(e2,el2,eo2,_) ->
      begin match cmp2 (cmp_expr e1 e2) cmp_expr_list el1 el2 with
	| 0 -> begin
	    match eo1,eo2 with
	      | None, None -> 0
	      | None, Some _ -> -1
	      | Some _, None -> 1
	      | Some x1, Some x2 -> cmp_expr x1 x2
	  end
	| c -> c
      end

  | E_Unless(e1,el11, el12,_), E_Unless(e2,el21, el22,_)
  | E_If(e1,el11, el12,_), E_If(e2,el21,el22,_) ->
      cmp2 (cmp2 (cmp_expr e1 e2) cmp_expr_list el11 el21) 
	cmp_expr_list el12 el22

  | E_MethodDef(e1, fl1, body1, _), E_MethodDef(e2, fl2, body2, _) ->
      cmp2 (cmp2 (cmp_expr e1 e2) (cmp_list cmp_formal) fl1 fl2) cmp_body_exn body1 body2

  | E_CodeBlock(b1,sl1,el1,_), E_CodeBlock(b2,sl2,el2,_) ->
      cmp2 (pcompare b1 b2)
	(cmp2 (cmp_opt (cmp_list cmp_formal) sl1 sl2) cmp_expr_list) el1 el2

  | E_ClassDef(e1,iho1, body1, _), E_ClassDef(e2,iho2,body2, _) ->
      begin match (cmp2 (cmp_expr e1 e2) cmp_body_exn body1 body2) with
	| 0 -> cmp_expr_opt cmp_inh iho1 iho2
	| c -> c
      end

  | E_Case(c1,_), E_Case(c2,_) ->
      let c (l11,l12) (l21,l22) =
	cmp2 (cmp_expr_list l11 l21) cmp_expr_list l12 l22
      in
	cmp2 (cmp2 (cmp_expr c1.case_guard c2.case_guard)
		 (cmp_list c) c1.case_whens c2.case_whens)
	  cmp_expr_list c1.case_else c2.case_else

  | _ -> Utils.cmp_ctors e1 e2

and cmp_lit c1 c2 = match c1,c2 with
  | Lit_FixNum i1, Lit_FixNum i2 -> pcompare i1 i2
  | Lit_BigNum b1, Lit_BigNum b2 -> Big_int.compare_big_int b1 b2
  | Lit_Float(s1,f1), Lit_Float (s2,f2) -> 
      let c1 = pcompare (float_of_string s1) (float_of_string s2) in
        cmp2 c1 pcompare f1 f2
  | Lit_String(k1), Lit_String(k2) -> cmp_string k1 k2
  | Lit_Atom s1, Lit_Atom s2 -> cmp_list cmp_interp s1 s2
  | Lit_Regexp (s1,m1), Lit_Regexp (s2,m2) -> 
      cmp2 (cmp_list cmp_interp s1 s2) pcompare m1 m2

  | Lit_Nil, Lit_Nil
  | Lit_Self, Lit_Self 
  | Lit_True, Lit_True
  | Lit_False, Lit_False -> 0

  | _ -> pcompare c1 c2

and cmp_string sk1 sk2 = match sk1,sk2 with
  | String_Single s1, String_Single s2 -> String.compare s1 s2
  | String_Single s1, _ -> -1
  | _, String_Single s2 -> 1
  | String_Tick i1, String_Tick i2
  | String_Double i1, String_Double i2 ->
      cmp_list cmp_interp i1 i2
  | String_Double _, String_Tick _ -> -1
  | String_Tick _, String_Double _ -> 1

and cmp_interp i1 i2 = match i1, i2 with
  | StrChars s1, StrChars s2 -> String.compare s1 s2
  | StrExpr e1, StrExpr e2 -> cmp_expr e1 e2

  | StrChars _, _ -> -1
  | _, StrChars _ -> 1
    
and cmp_inh ih1 ih2 = match ih1,ih2 with
  | Class_Inherit e1, Class_Inherit e2
  | Inst_Inherit e1, Inst_Inherit e2 -> cmp_expr e1 e2
  | Class_Inherit _, Inst_Inherit _ -> -1
  | Inst_Inherit _, Class_Inherit _ -> 1

and cmp_expr_opt f eo1 eo2 = match eo1,eo2 with
  | None, None -> 0
  | None, Some _ -> -1
  | Some _, None -> 1
  | Some e1, Some e2 -> f e1 e2

and cmp_formal f1 f2 = match f1,f2 with
  | Formal_id e1, Formal_id e2 -> cmp_expr e1 e2
  | Formal_amp s1, Formal_amp s2 -> String.compare s1 s2
  | Formal_star s1, Formal_star s2 -> String.compare s1 s2
  | Formal_rest, Formal_rest -> 0
  | Formal_tuple l1, Formal_tuple l2 -> cmp_list cmp_formal l1 l2
  | Formal_default(s1,e1),Formal_default(s2,e2) ->
      cmp2 (String.compare s1 s2) cmp_expr e1 e2
  | _ -> Utils.cmp_ctors f1 f2

and cmp_expr_pair (e11,e12) (e21,e22) =
  cmp2 (cmp_expr e11 e21) cmp_expr e12 e22

and cmp_body_exn b1 b2 = 
  cmp2 
    (cmp2
	(cmp2 (cmp_expr_list b1.body_exprs b2.body_exprs)
	    (cmp_list cmp_expr_pair) b1.rescue_exprs b2.rescue_exprs
	)
	cmp_expr_list b1.ensure_expr b2.ensure_expr
    ) cmp_expr_list b1.else_expr b2.else_expr
    
and cmp_expr_list el1 el2 = cmp_list cmp_expr el1 el2

and cmp_hash hl1 hl2 = 
  cmp_list 
    (fun (e1,eo1) (e2,eo2) ->
      match cmp_expr e1 e2 with
	| 0 -> begin match eo1, eo2 with
	    | None, None ->  0
	    | None, Some _ -> -1
	    | Some _, None -> 1
	    | Some e1', Some e2' -> cmp_expr e1' e2'
	  end
	| c -> c
    ) hl1 hl2

let compare_expr = cmp_expr

let equal_expr e1 e2 = compare_expr e1 e2 = 0

let compare_ast ast1 ast2 = cmp_list cmp_expr ast1 ast2

let equal_ast a1 a2 = (compare_ast a1 a2) == 0

let pos_of = function
  | E_Empty -> Lexing.dummy_pos
  | E_Literal (_, pos)
  | E_Alias (_,_, pos)
  | E_Undef (_,pos)
  | E_Identifier (_, _, pos)
  | E_Unary ( _ , _ , pos)
  | E_Binop ( _ , _ , _ , pos)
  | E_Ternary ( _ , _ , _ , pos)
  | E_Hash ( _,_,pos)
  | E_Array ( _  , pos)
  | E_Tuple ( _  , pos)
  | E_MethodCall ( _ , _  , _ , pos)
  | E_While (_, _ , _  , pos)
  | E_Until (_, _ , _  , pos)
  | E_Unless ( _ , _  , _  , pos)
  | E_For ( _  , _ , _  , pos)
  | E_If ( _ , _  , _  , pos)
  | E_ModuleDef ( _ , _ , pos)
  | E_MethodDef ( _ , _  , _,  pos)
  | E_CodeBlock (_,  _  , _  , pos)
  | E_ClassDef ( _ , _ , _, pos)
  | E_BeginBlock ( _  , pos)
  | E_EndBlock ( _  , pos)
  | E_ExnBlock ( _ , pos)
  | E_Case ( _ , pos)
  | E_Operator ( _ , pos)
  | E_UOperator ( _ , pos)
  | E_Return ( _  , pos)
  | E_Yield ( _  , pos)
  | E_Block ( _  , pos)
  | E_Annotate (_,_,pos) -> pos

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
  | "."    -> Op_DOT      
  | "::"   -> Op_SCOPE    
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

let rec mod_expr f expr = 
  let processed_expr =
    match expr with
      | E_Alias(expr1, expr2, pos) ->
          E_Alias(
            (mod_expr f expr1),
            (mod_expr f expr2),
            pos
          )
      | E_Undef(expr', pos) -> 
          E_Undef(List.map (mod_expr f) expr', pos)
      | E_Unary(uop, expr, pos) -> 
          E_Unary(uop, (mod_expr f expr), pos)
      | E_Binop(expr1, binary_op, expr2, pos) ->
          E_Binop(
            (mod_expr f expr1),
            binary_op,
            (mod_expr f expr2),
            pos
          )
      | E_Ternary(expr1, expr2, expr3, pos) ->
          E_Ternary(
            (mod_expr f expr1),
            (mod_expr f expr2),
            (mod_expr f expr3),
            pos
          )
      | E_Hash(b,el, pos) ->
          E_Hash(b,List.map (mod_expr f) el, pos)
      | E_Array(el, pos) ->
          E_Array((List.map (mod_expr f) el), pos)
      | E_Tuple(el, pos) ->
          E_Tuple((List.map (mod_expr f) el), pos)
      | E_MethodCall(expr1, el, eo, pos) ->
          E_MethodCall(
            mod_expr f expr1, 
            List.map (mod_expr f) el,
            (
              match eo with
                | None -> None
                | Some(e) -> Some(mod_expr f e)
            ),
            pos
          )
      | E_While(b, expr, el, pos) ->
          E_While(
            b, (mod_expr f expr), (List.map (mod_expr f) el), pos
          )
      | E_Until(b, expr, el, pos) ->
          E_Until(
            b, (mod_expr f expr), (List.map (mod_expr f) el), pos
          )
      | E_Unless(expr, el1, el2, pos) ->
          E_Unless(
            (mod_expr f expr),
            (List.map (mod_expr f) el1),
            (List.map (mod_expr f) el2),
            pos
          )
      | E_For(formals, expr, el, pos) ->
          E_For(
            formals, 
            (mod_expr f expr),
            (List.map (mod_expr f) el),
            pos
          )
      | E_If(expr, el1, el2, pos) ->
          E_If(
            (mod_expr f expr),
            (List.map (mod_expr f) el1),
            (List.map (mod_expr f) el2),
            pos
          )
      | E_ModuleDef(expr, body, pos) ->
          E_ModuleDef(
            (mod_expr f expr),
            (mod_body_exn f body),
            pos
          )
      | E_MethodDef(expr, formals, body, pos) ->
          E_MethodDef(
            (mod_expr f expr), 
            formals, 
            (mod_body_exn f body), 
            pos
          ) 
      | E_CodeBlock(b, formals, el, pos) ->
          E_CodeBlock(b, formals, (List.map (mod_expr f) el), pos)
      | E_ClassDef(expr, i_kind, body, pos) ->
          E_ClassDef(
            (mod_expr f expr), 
            i_kind, 
            (mod_body_exn f body),
            pos
          )
      | E_BeginBlock(el, pos) -> E_BeginBlock(List.map (mod_expr f) el, pos)
      | E_EndBlock(el, pos) -> E_EndBlock(List.map (mod_expr f) el, pos)
      | E_ExnBlock(body, pos) -> E_ExnBlock(mod_body_exn f body, pos)
      | E_Case(block, pos) -> E_Case(mod_case_block f block, pos)
      | E_Return(el, pos) -> E_Return((List.map (mod_expr f) el), pos)
      | E_Yield(el, pos) -> E_Yield((List.map (mod_expr f) el), pos)
      | E_Block(el, pos) -> E_Block((List.map (mod_expr f) el), pos)
      | E_Annotate(e,annot,pos) -> E_Annotate(mod_expr_annot f e, annot,pos)

      | E_UOperator _ | E_Operator _ | E_Identifier _ | E_Literal _ | E_Empty -> 
          expr
  in
    f processed_expr

and mod_expr_annot f expr_annot = 
  Log.warn "mod_expr_annot";
  expr_annot 

and mod_case_block f block =
  {
    case_guard = mod_expr f block.case_guard;
    case_whens = (
      List.map (
        fun((el1, el2)) ->
          ((List.map (mod_expr f) el1), (List.map (mod_expr f) el2))
      )
      block.case_whens
    );
    case_else = List.map (mod_expr f) block.case_else
  }

and mod_body_exn f body =
  {
    body_exprs = (List.map (mod_expr f) body.body_exprs);
    rescue_exprs = (
      List.map 
      (fun((expr1, expr2)) -> (mod_expr f expr1, mod_expr f expr2))
      body.rescue_exprs
    );
    ensure_expr = (List.map (mod_expr f) body.ensure_expr);
    else_expr = (List.map (mod_expr f) body.else_expr)
  }

let mod_ast f ast = List.map (mod_expr f) ast

(** sets the position of the expression; use it with mod_ast **)
let set_pos pos = function
  | E_Literal(lit_kind, _) -> E_Literal(lit_kind, pos) 
  | E_Alias(e1, e2, _) -> E_Alias(e1, e2, pos)
  | E_Undef(elist, _) -> E_Undef(elist, pos)
  | E_Identifier(id_kind, str, _) -> E_Identifier(id_kind, str, pos)
  | E_Unary(unary_op, e, _) -> E_Unary(unary_op, e, pos)
  | E_Binop(expr1, binary_op, expr2, _) -> E_Binop(expr1, binary_op, expr2, pos)
  | E_Ternary(expr1, expr2, expr3, _) -> E_Ternary(expr1, expr2, expr3, pos)
  | E_Hash(b,el, _) -> E_Hash(b,el, pos)
  | E_Array(el, _) -> E_Array(el, pos)
  | E_Tuple(el, _) -> E_Tuple(el, pos)
  | E_MethodCall(expr1, el, eo, _) -> E_MethodCall(expr1, el, eo, pos)
  | E_While(b, expr, el, _) -> E_While(b, expr, el, pos)
  | E_Until(b, expr, el, _) -> E_Until(b, expr, el, pos)
  | E_Unless(expr, el1, el2, _) -> E_Unless(expr, el1, el2, pos)
  | E_For(formals, expr, el, _) -> E_For(formals, expr, el, pos)
  | E_If(expr, el1, el2, _) -> E_If(expr, el1, el2, pos)
  | E_ModuleDef(expr, body, _) -> E_ModuleDef(expr, body, pos)
  | E_MethodDef(expr, formals, body, _) -> 
      E_MethodDef(expr, formals, body, pos)
  | E_CodeBlock(b, formals, el, _) -> E_CodeBlock(b, formals, el, pos)
  | E_ClassDef(expr, i_kind, body, _) ->
      E_ClassDef(expr, i_kind, body, pos)
  | E_BeginBlock(el, _) -> E_BeginBlock(el, pos)
  | E_EndBlock(el, _) -> E_EndBlock(el, pos)
  | E_ExnBlock(body, _) -> E_ExnBlock(body, pos)
  | E_Case(block, _) -> E_Case(block, pos)
  | E_Return(el, _) -> E_Return(el, pos)
  | E_Yield(el, _) -> E_Yield(el, pos)
  | E_Block(el, _) -> E_Block(el, pos)
  | _ as expr -> expr

let id_kind s pos = match s.[0] with
  | 'a'..'z' | '_' -> ID_Lowercase
  | 'A'..'Z' -> ID_Uppercase
  | '@' -> 
      if s.[1] == '@' then ID_Class
      else ID_Instance
  | '$' -> begin match s.[1] with
      | 'a'..'z' | 'A'..'Z' | '_' -> ID_Global
      | _ -> ID_Builtin
    end
  | _ -> Log.fatal (Log.of_loc pos) "unknown id_kind in cfg_refactor: %s" s

let msg_of_str a pos = match a with
  | "+" -> E_Operator(Op_PLUS,pos)
  | "-" -> E_Operator(Op_MINUS,pos)
  | "*" -> E_Operator(Op_TIMES,pos)
  | "/" -> E_Operator(Op_DIV,pos)
  | "%" -> E_Operator(Op_REM,pos)
  | "<=>" -> E_Operator(Op_CMP,pos)
  | "==" -> E_Operator(Op_EQ,pos)
  | "===" -> E_Operator(Op_EQQ,pos)
  | ">=" -> E_Operator(Op_GEQ,pos)
  | "<=" -> E_Operator(Op_LEQ,pos)
  | "<" -> E_Operator(Op_LT,pos)
  | ">" -> E_Operator(Op_GT,pos)
  | "&" -> E_Operator(Op_BAND,pos)
  | "|" -> E_Operator(Op_BOR,pos)
  | "=~" -> E_Operator(Op_MATCH,pos)
  | "^" -> E_Operator(Op_XOR,pos)
  | "**" -> E_Operator(Op_POW,pos)
  | "[]" -> E_Operator(Op_AREF,pos)
  | "[]=" -> E_Operator(Op_ASET,pos)
  | "<<" -> E_Operator(Op_LSHIFT,pos)
  | ">>" -> E_Operator(Op_RSHIFT,pos)

  | "-@" -> E_UOperator(Op_UMinus,pos)
  | "+@" -> E_UOperator(Op_UPlus,pos)
  | "~@" | "~" -> E_UOperator(Op_UTilde,pos)
  | s -> E_Identifier(id_kind s pos, s, pos)


let str_uop = function
  | Op_UMinus   -> "-"
  | Op_UPlus    -> "+"
  | Op_UBang    -> "!"
  | Op_UTilde   -> "~"
  | Op_UNot     -> "not "
  | Op_UStar    -> "*"
  | Op_UAmper   -> "&"
  | Op_UScope   -> "::"

let rec str_binop = function
  | Op_ASSIGN   -> "="
  | Op_PLUS     -> "+"
  | Op_MINUS    -> "-"
  | Op_TIMES    -> "*"
  | Op_DIV      -> "/"
  | Op_REM      -> "%"
  | Op_CMP  	-> "<=>"
  | Op_EQ  	-> "=="
  | Op_EQQ  	-> "==="
  | Op_NEQ  	-> "!="
  | Op_GEQ  	-> ">="
  | Op_LEQ  	-> "<="
  | Op_LT  	-> "<"
  | Op_GT  	-> ">"
  | Op_AND      -> "&&"
  | Op_OR	-> "||"
  | Op_BAND     -> "&"
  | Op_BOR      -> "|"
  | Op_MATCH    -> "=~"
  | Op_NMATCH   -> "!~"
  | Op_XOR      -> "^"
  | Op_POW      -> "**"
  | Op_kAND     -> "and"
  | Op_kOR      -> "or"
  | Op_ASSOC    -> "=>"
  | Op_DOT      -> "."
  | Op_SCOPE    -> "::"
  | Op_AREF     -> "[]"
  | Op_ASET     -> "[]="
  | Op_LSHIFT   -> "<<"
  | Op_RSHIFT   -> ">>"
  | Op_OP_ASGN op -> (str_binop op) ^ "="
  | Op_DOT2     -> ".."
  | Op_DOT3     -> "..."

open Annotation

let name_of_annot_id = function
  | TIdent_Relative s
  | TIdent_Absolute s
  | TIdent_Scoped(_,s) -> s

let rec names_of_expr_id pos = function
  | E_Identifier(ID_Assign _,s,_) -> [s ^ "="]
  | E_Identifier(_,s,_) -> [s]
  | E_Binop(_,(Op_SCOPE|Op_DOT),r,_) -> names_of_expr_id pos r
  | E_Unary(Op_UScope,r,_) -> names_of_expr_id pos r
  | E_Operator(op,_) -> [str_binop op]
  | E_UOperator(uop,_) -> 
      (* some unary ops can be specified without their @ postfix, so
         we allow both forms *)
      let s = str_uop uop in
        [s; s ^ "@"]
  | _ -> Log.fatal (Log.of_loc pos) "expr id"

let verify_annotation_name e annot pos = match annot,e with
  | ClassType(aname,_,_), E_ClassDef(class_id,_,_,_)
  | ClassType(aname,_,_), E_ModuleDef(class_id,_,_) ->
      let names = names_of_expr_id pos class_id in
      if not (List.mem aname names)
      then Log.fatal (Log.of_loc pos)
        "annotation for %s does not match ruby name %s" aname (List.hd names)

  | ClassType _, _ ->
      Log.fatal (Log.of_loc pos) "class annotation on non-class expr"

  | MethodType [], _ -> assert false
  | MethodType ((annot_id,_,_)::_), E_MethodDef(meth_id,_,_,_) ->
      let names = names_of_expr_id pos meth_id in
      let aname = name_of_annot_id annot_id in
      if not (List.mem aname names)
      then Log.fatal (Log.of_loc pos)
        "annotation for %s does not match ruby method name %s" aname (List.hd names)

  | MethodType _, _ ->
      Log.fatal (Log.of_loc pos) "method annotation on non-method expr"

  | ExprType _, _ -> ()
      
