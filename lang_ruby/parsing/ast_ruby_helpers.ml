open Common
open Ast_ruby
module Utils = Utils_ruby

let pcompare = Pervasives.compare

let cmp2 = Utils.cmp2
let cmp_list = Utils.cmp_list

let cmp_opt f x y = match x,y with
  | None, None -> 0
  | Some _, None -> -1
  | None, Some _ -> 1
  | Some e1, Some e2 -> f e1 e2

let rec cmp_expr e1 e2 = match e1,e2 with
  | D ModuleDef(e1,body1, _), D ModuleDef(e2,body2, _) -> 
      cmp2 (cmp_expr e1 e2) cmp_body_exn body1 body2

  | Id(k1,s1,_), Id(k2,s2,_) -> 
      cmp2 (pcompare k1 k2) pcompare s1 s2

  | S Empty, S Empty -> 0
  | Literal(k1,_), Literal(k2,_) -> cmp_lit k1 k2

  | D Alias(e11,e12,_), D Alias(e21,e22,_) -> 
      cmp2 (cmp_expr e11 e21) cmp_expr e12 e22

  | D Undef(e1,__), D Undef(e2,_) -> cmp_expr_list e1 e2

  | Unary(u1,e1,_), Unary(u2,e2,_) ->
      cmp2 (pcompare u1 u2) cmp_expr e1 e2

  | Binop(e11,o1,e12,_), Binop(e21,o2,e22,_) ->
      cmp2 (cmp2 (pcompare o1 o2) cmp_expr e11 e21) cmp_expr e12 e22

  | UOperator(u1,_), UOperator(u2,_) -> pcompare u1 u2
  | Operator(b1,_), Operator(b2,_) -> pcompare b1 b2

  | Hash (b1,el1,_), Hash (b2,el2,_) ->
      cmp2 (pcompare b1 b2) cmp_expr_list el1 el2

  | S Return(el1,_), S Return(el2,_)
  | S Yield(el1,_), S Yield (el2,_)
  | S Block(el1,_), S Block (el2,_)
  | Array (el1,_), Array (el2,_)
  | Tuple (el1,_), Tuple (el2,_)
  | D BeginBlock (el1,_), D BeginBlock (el2,_)
  | D EndBlock (el1,_), D EndBlock (el2,_) -> cmp_expr_list el1 el2

  | S ExnBlock(body1,_), S ExnBlock(body2,_) ->
      cmp_body_exn body1 body2

  | Ternary(e11,e12,e13,_), Ternary(e21,e22,e23,_) ->
      cmp2 (cmp2 (cmp_expr e11 e21) cmp_expr e12 e22) cmp_expr e13 e23

  | S For(fl1,e1,el1,_),S For(fl2,e2,el2,_) ->
      cmp2 (cmp2 (cmp_list cmp_formal fl1 fl2) cmp_expr e1 e2) cmp_expr_list el1 el2

  | S While(b1,e1,el1,_), S While(b2,e2,el2,_) ->
      cmp2 (cmp2 (cmp_expr e1 e2) cmp_expr_list el1 el2) pcompare b1 b2
  | S Until(b1,e1,el1,_), S Until(b2,e2,el2,_) ->
      cmp2 (cmp2 (cmp_expr e1 e2) cmp_expr_list el1 el2) pcompare b1 b2

  | Call (e1,el1,eo1,_), Call(e2,el2,eo2,_) ->
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

  | S Unless(e1,el11, el12,_), S Unless(e2,el21, el22,_)
  | S If(e1,el11, el12,_), S If(e2,el21,el22,_) ->
      cmp2 (cmp2 (cmp_expr e1 e2) cmp_expr_list el11 el21) 
    cmp_expr_list el12 el22

  | D MethodDef(e1, fl1, body1, _), D MethodDef(e2, fl2, body2, _) ->
      cmp2 (cmp2 (cmp_expr e1 e2) (cmp_list cmp_formal) fl1 fl2) cmp_body_exn body1 body2

  | CodeBlock(b1,sl1,el1,_), CodeBlock(b2,sl2,el2,_) ->
      cmp2 (pcompare b1 b2)
    (cmp2 (cmp_opt (cmp_list cmp_formal) sl1 sl2) cmp_expr_list) el1 el2

  | D ClassDef(e1,iho1, body1, _), D ClassDef(e2,iho2,body2, _) ->
      begin match (cmp2 (cmp_expr e1 e2) cmp_body_exn body1 body2) with
    | 0 -> cmp_expr_opt cmp_inh iho1 iho2
    | c -> c
      end

  | S Case(c1,_), S Case(c2,_) ->
      let c (l11,l12) (l21,l22) =
    cmp2 (cmp_expr_list l11 l21) cmp_expr_list l12 l22
      in
    cmp2 (cmp2 (cmp_expr c1.case_guard c2.case_guard)
         (cmp_list c) c1.case_whens c2.case_whens)
      cmp_expr_list c1.case_else c2.case_else

  | _ -> Utils.cmp_ctors e1 e2

and cmp_lit c1 c2 = match c1,c2 with
  | FixNum i1, FixNum i2 -> pcompare i1 i2
  | BigNum b1, BigNum b2 -> Big_int.compare_big_int b1 b2
  | Float(s1,f1), Float (s2,f2) -> 
      let c1 = pcompare (float_of_string s1) (float_of_string s2) in
        cmp2 c1 pcompare f1 f2
  | String(k1), String(k2) -> cmp_string k1 k2
  | Atom s1, Atom s2 -> cmp_list cmp_interp s1 s2
  | Regexp (s1,m1), Regexp (s2,m2) -> 
      cmp2 (cmp_list cmp_interp s1 s2) pcompare m1 m2

  | Nil, Nil
  | Self, Self 
  | True, True
  | False, False -> 0

  | _ -> pcompare c1 c2

and cmp_string sk1 sk2 = match sk1,sk2 with
  | Single s1, Single s2 -> String.compare s1 s2
  | Single _s1, _ -> -1
  | _, Single _s2 -> 1
  | Tick i1, Tick i2
  | Double i1, Double i2 ->
      cmp_list cmp_interp i1 i2
  | Double _, Tick _ -> -1
  | Tick _, Double _ -> 1

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


let compare_expr = cmp_expr

let equal_expr e1 e2 = compare_expr e1 e2 = 0

let compare_ast ast1 ast2 = cmp_list cmp_expr ast1 ast2

let equal_ast a1 a2 = (compare_ast a1 a2) == 0

let tok_of = function
  | S Empty -> Parse_info.fake_info "Empty"

  | Literal (_, pos)
  | D Alias (_,_, pos)
  | D Undef (_,pos)
  | Id (_, _, pos)
  | Unary ( _ , _ , pos)
  | Binop ( _ , _ , _ , pos)
  | Ternary ( _ , _ , _ , pos)
  | Hash ( _,_,pos)
  | Array ( _  , pos)
  | Tuple ( _  , pos)
  | Call ( _ , _  , _ , pos)
  | S While (_, _ , _  , pos)
  | S Until (_, _ , _  , pos)
  | S Unless ( _ , _  , _  , pos)
  | S For ( _  , _ , _  , pos)
  | S If ( _ , _  , _  , pos)
  | D ModuleDef ( _ , _ , pos)
  | D MethodDef ( _ , _  , _,  pos)
  | CodeBlock (_,  _  , _  , pos)
  | D ClassDef ( _ , _ , _, pos)
  | D BeginBlock ( _  , pos)
  | D EndBlock ( _  , pos)
  | S ExnBlock ( _ , pos)
  | S Case ( _ , pos)
  | Operator ( _ , pos)
  | UOperator ( _ , pos)
  | S Return ( _  , pos)
  | S Yield ( _  , pos)
  | S Block ( _  , pos)
   -> pos

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
      | D Alias(expr1, expr2, pos) ->
          D (Alias(
            (mod_expr f expr1),
            (mod_expr f expr2),
            pos
          ))
      | D Undef(expr', pos) -> 
          D (Undef(List.map (mod_expr f) expr', pos))
      | Unary(uop, expr, pos) -> 
          Unary(uop, (mod_expr f expr), pos)
      | Binop(expr1, binary_op, expr2, pos) ->
          Binop(
            (mod_expr f expr1),
            binary_op,
            (mod_expr f expr2),
            pos
          )
      | Ternary(expr1, expr2, expr3, pos) ->
          Ternary(
            (mod_expr f expr1),
            (mod_expr f expr2),
            (mod_expr f expr3),
            pos
          )
      | Hash(b,el, pos) ->
          Hash(b,List.map (mod_expr f) el, pos)
      | Array(el, pos) ->
          Array((List.map (mod_expr f) el), pos)
      | Tuple(el, pos) ->
          Tuple((List.map (mod_expr f) el), pos)
      | Call(expr1, el, eo, pos) ->
          Call(
            mod_expr f expr1, 
            List.map (mod_expr f) el,
            (
              match eo with
                | None -> None
                | Some(e) -> Some(mod_expr f e)
            ),
            pos
          )
      | S While(b, expr, el, pos) ->
          S (While(
            b, (mod_expr f expr), (List.map (mod_expr f) el), pos
          ))
      | S Until(b, expr, el, pos) ->
          S (Until(
            b, (mod_expr f expr), (List.map (mod_expr f) el), pos
          ))
      | S Unless(expr, el1, el2, pos) ->
          S (Unless(
            (mod_expr f expr),
            (List.map (mod_expr f) el1),
            (List.map (mod_expr f) el2),
            pos
          ))
      | S For(formals, expr, el, pos) ->
          S (For(
            formals, 
            (mod_expr f expr),
            (List.map (mod_expr f) el),
            pos
          ))
      | S If(expr, el1, el2, pos) ->
          S (If(
            (mod_expr f expr),
            (List.map (mod_expr f) el1),
            (List.map (mod_expr f) el2),
            pos
          ))
      | D ModuleDef(expr, body, pos) ->
          D (ModuleDef(
            (mod_expr f expr),
            (mod_body_exn f body),
            pos
          ))
      | D MethodDef(expr, formals, body, pos) ->
          D (MethodDef(
            (mod_expr f expr), 
            formals, 
            (mod_body_exn f body), 
            pos
          ))
      | CodeBlock(b, formals, el, pos) ->
          CodeBlock(b, formals, (List.map (mod_expr f) el), pos)
      | D ClassDef(expr, i_kind, body, pos) ->
          D (ClassDef(
            (mod_expr f expr), 
            i_kind, 
            (mod_body_exn f body),
            pos
          ))
      | D BeginBlock(el, pos) -> D (BeginBlock(List.map (mod_expr f) el, pos))
      | D EndBlock(el, pos) -> D (EndBlock(List.map (mod_expr f) el, pos))
      | S ExnBlock(body, pos) -> S (ExnBlock(mod_body_exn f body, pos))
      | S Case(block, pos) -> S (Case(mod_case_block f block, pos))
      | S Return(el, pos) -> S (Return((List.map (mod_expr f) el), pos))
      | S Yield(el, pos) -> S (Yield((List.map (mod_expr f) el), pos))
      | S Block(el, pos) -> S (Block((List.map (mod_expr f) el), pos))

      | UOperator _ | Operator _ | Id _ | Literal _ | S Empty -> 
          expr
  in
    f processed_expr

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
let set_tok pos = function
  | Literal(lit_kind, _) -> Literal(lit_kind, pos) 
  | D Alias(e1, e2, _) -> D (Alias(e1, e2, pos))
  | D Undef(elist, _) -> D (Undef(elist, pos))
  | Id(id_kind, str, _) -> Id(id_kind, str, pos)
  | Unary(unary_op, e, _) -> Unary(unary_op, e, pos)
  | Binop(expr1, binary_op, expr2, _) -> Binop(expr1, binary_op, expr2, pos)
  | Ternary(expr1, expr2, expr3, _) -> Ternary(expr1, expr2, expr3, pos)
  | Hash(b,el, _) -> Hash(b,el, pos)
  | Array(el, _) -> Array(el, pos)
  | Tuple(el, _) -> Tuple(el, pos)
  | Call(expr1, el, eo, _) -> Call(expr1, el, eo, pos)
  | S While(b, expr, el, _) -> S (While(b, expr, el, pos))
  | S Until(b, expr, el, _) -> S (Until(b, expr, el, pos))
  | S Unless(expr, el1, el2, _) -> S (Unless(expr, el1, el2, pos))
  | S For(formals, expr, el, _) -> S (For(formals, expr, el, pos))
  | S If(expr, el1, el2, _) -> S (If(expr, el1, el2, pos))
  | D ModuleDef(expr, body, _) -> D (ModuleDef(expr, body, pos))
  | D MethodDef(expr, formals, body, _) -> 
      D (MethodDef(expr, formals, body, pos))
  | CodeBlock(b, formals, el, _) -> CodeBlock(b, formals, el, pos)
  | D ClassDef(expr, i_kind, body, _) ->
      D (ClassDef(expr, i_kind, body, pos))
  | D BeginBlock(el, _) -> D (BeginBlock(el, pos))
  | D EndBlock(el, _) -> D (EndBlock(el, pos))
  | S ExnBlock(body, _) -> S (ExnBlock(body, pos))
  | S Case(block, _) -> S (Case(block, pos))
  | S Return(el, _) -> S (Return(el, pos))
  | S Yield(el, _) -> S (Yield(el, pos))
  | S Block(el, _) -> S (Block(el, pos))
  | _ as expr -> expr

let id_kind s _pos = match s.[0] with
  | 'a'..'z' | '_' -> ID_Lowercase
  | 'A'..'Z' -> ID_Uppercase
  | '@' -> 
      if s.[1] == '@' then ID_Class
      else ID_Instance
  | '$' -> begin match s.[1] with
      | 'a'..'z' | 'A'..'Z' | '_' -> ID_Global
      | _ -> ID_Builtin
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
  | s -> Id(id_kind s pos, s, pos)


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
  | Op_CMP      -> "<=>"
  | Op_EQ   -> "=="
  | Op_EQQ      -> "==="
  | Op_NEQ      -> "!="
  | Op_GEQ      -> ">="
  | Op_LEQ      -> "<="
  | Op_LT   -> "<"
  | Op_GT   -> ">"
  | Op_AND      -> "&&"
  | Op_OR   -> "||"
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


