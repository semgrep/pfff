open Common
open Ast_ruby
module Utils = Utils_ruby

(*****************************************************************************)
(* Comparators *)
(*****************************************************************************)

let pcompare = Pervasives.compare

let cmp2 = Utils.cmp2
let cmp_list = Utils.cmp_list

let cmp_opt f x y = match x,y with
  | None, None -> 0
  | Some _, None -> -1
  | None, Some _ -> 1
  | Some e1, Some e2 -> f e1 e2

let rec cmp_expr e1 e2 = match e1,e2 with
  | D ModuleDef(_, e1,body1), D ModuleDef(_, e2,body2) -> 
      cmp2 (cmp_expr e1 e2) cmp_body_exn body1 body2

  | Id((s1,_),k1), Id((s2,_),k2) -> 
      cmp2 (pcompare k1 k2) pcompare s1 s2

  | S Empty, S Empty -> 0
  | Literal(k1), Literal(k2) -> cmp_lit k1 k2

  | D Alias(_, e11,e12), D Alias(_, e21,e22) -> 
      cmp2 (cmp_expr e11 e21) cmp_expr e12 e22

  | D Undef(_, e1), D Undef(_, e2) -> cmp_expr_list e1 e2

  | Unary((u1,_),e1), Unary((u2,_),e2) ->
      cmp2 (pcompare u1 u2) cmp_expr e1 e2

  | Binop(e11,(o1,_),e12), Binop(e21,(o2,_),e22) ->
      cmp2 (cmp2 (pcompare o1 o2) cmp_expr e11 e21) cmp_expr e12 e22

  | UOperator(u1,_), UOperator(u2,_) -> pcompare u1 u2
  | Operator(b1,_), Operator(b2,_) -> pcompare b1 b2

  | Hash (b1,(_, el1,_)), Hash (b2,(_, el2,_)) ->
      cmp2 (pcompare b1 b2) cmp_expr_list el1 el2

  | S Return(_, el1), S Return(_, el2)
  | S Yield(_, el1), S Yield (_, el2)
  | S Block(el1,_), S Block (el2,_)
  | Array (_, el1,_), Array (_, el2,_)
  | Tuple (el1,_), Tuple (el2,_)
  | D BeginBlock (_, (_, el1, _)), D BeginBlock (_, (_, el2, _))
  | D EndBlock (_, (_, el1, _)), D EndBlock (_, (_, el2, _)) ->
      cmp_expr_list el1 el2

  | S ExnBlock(body1,_), S ExnBlock(body2,_) ->
      cmp_body_exn body1 body2

  | Ternary(e11,_, e12,_, e13), Ternary(e21,_, e22,_, e23) ->
      cmp2 (cmp2 (cmp_expr e11 e21) cmp_expr e12 e22) cmp_expr e13 e23

  | S For(_, fl1,e1,el1),S For(_, fl2,e2,el2) ->
      cmp2 (cmp2 (cmp_list cmp_formal fl1 fl2) cmp_expr e1 e2) cmp_expr_list el1 el2

  | S While(_, b1,e1,el1), S While(_, b2,e2,el2) ->
      cmp2 (cmp2 (cmp_expr e1 e2) cmp_expr_list el1 el2) pcompare b1 b2
  | S Until(_, b1,e1,el1), S Until(_, b2,e2,el2) ->
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

  | S Unless(_, e1,el11, el12), S Unless(_, e2,el21, el22)
  | S If(_, e1,el11, el12), S If(_, e2,el21,el22) ->
      cmp2 (cmp2 (cmp_expr e1 e2) cmp_expr_list el11 el21) 
    cmp_expr_list el12 el22

  | D MethodDef(_, e1, fl1, body1), D MethodDef(_, e2, fl2, body2) ->
      cmp2 (cmp2 (cmp_expr e1 e2) (cmp_list cmp_formal) fl1 fl2) cmp_body_exn body1 body2

  | CodeBlock(b1,sl1,el1,_), CodeBlock(b2,sl2,el2,_) ->
      cmp2 (pcompare b1 b2)
    (cmp2 (cmp_opt (cmp_list cmp_formal) sl1 sl2) cmp_expr_list) el1 el2

  | D ClassDef(_, e1,iho1, body1), D ClassDef(_, e2,iho2,body2) ->
      begin match (cmp2 (cmp_expr e1 e2) cmp_body_exn body1 body2) with
    | 0 -> cmp_expr_opt cmp_inh iho1 iho2
    | c -> c
      end

  | S Case(_, c1), S Case(_, c2) ->
      let c (l11,l12) (l21,l22) =
    cmp2 (cmp_expr_list l11 l21) cmp_expr_list l12 l22
      in
    cmp2 (cmp2 (cmp_expr c1.case_guard c2.case_guard)
         (cmp_list c) c1.case_whens c2.case_whens)
      cmp_expr_list c1.case_else c2.case_else

  | _ -> Utils.cmp_ctors e1 e2

and cmp_lit c1 c2 = match c1,c2 with
  | Num (i1,_), Num (i2,_) -> pcompare i1 i2
  | Float(s1,_), Float (s2,_) -> pcompare s1 s2
  | String(k1,_), String(k2,_) -> cmp_string k1 k2
  | Atom (s1,_), Atom (s2,_) -> cmp_list cmp_interp s1 s2
  | Regexp ((s1,m1),_), Regexp ((s2,m2),_) -> 
      cmp2 (cmp_list cmp_interp s1 s2) pcompare m1 m2

  | Nil _, Nil _
  | Self _, Self _
  | Bool (true, _), Bool (true, _)
  | Bool (false,_), Bool (false,_) -> 0

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
  | Formal_amp (_,(s1,_)), Formal_amp (_, (s2, _)) -> String.compare s1 s2
  | Formal_star (_, (s1,_)), Formal_star (_, (s2, _)) -> String.compare s1 s2
  | Formal_rest _, Formal_rest _ -> 0
  | Formal_tuple (_, l1, _), Formal_tuple (_, l2, _) -> 
      cmp_list cmp_formal l1 l2
  | Formal_default((s1,_),_, e1),Formal_default((s2,_),_,e2) ->
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

(*****************************************************************************)
(* tok_of *)
(*****************************************************************************)

let tok_of_literal = function
  | Num (_, pos)
  | Float (_, pos)
  | String (_, pos)
  | Regexp (_, pos)
  | Atom (_, pos)
  | Nil pos | Self pos 
  | Bool (_, pos) 
   -> pos

let tok_of = function
  | S Empty -> Parse_info.fake_info "Empty"

  | D Alias (pos, _,_)
  | D Undef (pos, _)
  | Id ((_, pos), _)
  | Unary ( (_, pos) , _ )
  | Binop ( _ , (_, pos) , _)
  | Ternary ( _ , pos, _ , _ , _)
  | Hash ( _,(pos, _,_))
  | Array (pos, _  , _)
  | Tuple ( _  , pos)
  | Call ( _ , _  , _ , pos)
  | S While (pos, _, _ , _)
  | S Until (pos, _, _ , _  )
  | S Unless (pos, _ , _  , _)
  | S For (pos, _  , _ , _ )
  | S If (pos, _ , _  , _ )
  | D ModuleDef (pos, _ , _ )
  | D MethodDef (pos, _ , _  , _)
  | CodeBlock (_,  _  , _  , pos)
  | D ClassDef (pos, _ , _ , _)
  | D BeginBlock (pos, _)
  | D EndBlock (pos, _)
  | S ExnBlock ( _ , pos)
  | S Case (pos, _)
  | Operator ( _ , pos)
  | UOperator ( _ , pos)
  | S Return (pos, _)
  | S Yield (pos, _)
  | S Block ( _  , pos)
   -> pos

  | Literal x -> tok_of_literal x

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

(*****************************************************************************)
(* Mappers *)
(*****************************************************************************)

let map_bracket f (t1, x, t2) = (t1, f x, t2)

let rec mod_expr f expr = 
  let processed_expr =
    match expr with
      | D Alias(pos, expr1, expr2) ->
          D (Alias(pos, (mod_expr f expr1),(mod_expr f expr2)))
      | D Undef(pos, expr') -> 
          D (Undef(pos, List.map (mod_expr f) expr'))
      | Unary((uop,pos), expr) -> 
          Unary((uop,pos), (mod_expr f expr))
      | Binop(expr1, (binary_op,pos), expr2) ->
          Binop(
            (mod_expr f expr1),
            (binary_op, pos),
            (mod_expr f expr2)
          )
      | Ternary(expr1, pos1, expr2, pos2, expr3) ->
          Ternary(
            (mod_expr f expr1), pos1,
            (mod_expr f expr2), pos2,
            (mod_expr f expr3)
          )
      | Hash(b,(pos1, el, pos2)) ->
          Hash(b, (pos1, List.map (mod_expr f) el, pos2))
      | Array(pos1, el, pos2) ->
          Array(pos1, List.map (mod_expr f) el, pos2)
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
      | S While(pos, b, expr, el) ->
          S (While(pos,
            b, (mod_expr f expr), (List.map (mod_expr f) el)
          ))
      | S Until(pos, b, expr, el) ->
          S (Until(pos,
            b, (mod_expr f expr), (List.map (mod_expr f) el)
          ))
      | S Unless(pos, expr, el1, el2) ->
          S (Unless(pos,
            (mod_expr f expr),
            (List.map (mod_expr f) el1),
            (List.map (mod_expr f) el2)
          ))
      | S For(pos, formals, expr, el) ->
          S (For(pos,
            formals, 
            (mod_expr f expr),
            (List.map (mod_expr f) el)
          ))
      | S If(pos, expr, el1, el2) ->
          S (If(pos,
            (mod_expr f expr),
            (List.map (mod_expr f) el1),
            (List.map (mod_expr f) el2)
          ))
      | D ModuleDef(pos, expr, body) ->
          D (ModuleDef(pos, (mod_expr f expr),(mod_body_exn f body)))
      | D MethodDef(pos, expr, formals, body) ->
          D (MethodDef(pos, (mod_expr f expr), formals, (mod_body_exn f body)))
      | CodeBlock(b, formals, el, pos) ->
          CodeBlock(b, formals, (List.map (mod_expr f) el), pos)
      | D ClassDef(pos, expr, i_kind, body) ->
          D (ClassDef(pos, (mod_expr f expr), i_kind, (mod_body_exn f body)))
      | D BeginBlock(pos, el) -> 
        D (BeginBlock(pos, map_bracket (List.map (mod_expr f)) el))
      | D EndBlock(pos, el) -> 
        D (EndBlock(pos, map_bracket (List.map (mod_expr f)) el))
      | S ExnBlock(body, pos) -> S (ExnBlock(mod_body_exn f body, pos))
      | S Case(pos, block) -> S (Case(pos, mod_case_block f block))
      | S Return(pos, el) -> S (Return(pos, (List.map (mod_expr f) el)))
      | S Yield(pos, el) -> S (Yield(pos, (List.map (mod_expr f) el)))
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

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)

let set_tok_literal pos = function
  | Num (x, _) -> Num (x, pos)
  | Float (x, _) -> Float (x, pos)
  | String (x,_) -> String (x, pos)
  | Regexp (x, _) -> Regexp (x, pos)
  | Atom (x,_) -> Atom (x, pos)
  | Nil _ -> Nil pos
  | Self _ -> Self pos
  | Bool (x, _) -> Bool (x, pos)

(** sets the position of the expression; use it with mod_ast **)
let set_tok pos = function
  | Literal(lit_kind) -> Literal(set_tok_literal pos lit_kind) 
  | D Alias(_, e1, e2) -> D (Alias(pos, e1, e2))
  | D Undef(_, elist) -> D (Undef(pos, elist))
  | Id((str, _), id_kind) -> Id((str, pos), id_kind)
  | Unary((unary_op,_), e) -> Unary((unary_op,pos), e)
  | Binop(expr1, (binary_op,_), expr2) -> Binop(expr1, (binary_op,pos), expr2)
  | Ternary(expr1, _, expr2, pos2, expr3) -> 
      Ternary(expr1, pos, expr2, pos2, expr3)
  | Hash(b,(_, el, pos2)) -> Hash(b,(pos, el, pos2))
  | Array(_, el, pos2) -> Array(pos, el, pos2)
  | Tuple(el, _) -> Tuple(el, pos)
  | Call(expr1, el, eo, _) -> Call(expr1, el, eo, pos)
  | S While(_, b, expr, el) -> S (While(pos, b, expr, el))
  | S Until(_, b, expr, el) -> S (Until(pos, b, expr, el))
  | S Unless(_, expr, el1, el2) -> S (Unless(pos, expr, el1, el2))
  | S For(_, formals, expr, el) -> S (For(pos, formals, expr, el))
  | S If(_, expr, el1, el2) -> S (If(pos, expr, el1, el2))
  | D ModuleDef(_, expr, body) -> D (ModuleDef(pos, expr, body))
  | D MethodDef(_, expr, formals, body) -> 
      D (MethodDef(pos, expr, formals, body))
  | CodeBlock(b, formals, el, _) -> CodeBlock(b, formals, el, pos)
  | D ClassDef(_, expr, i_kind, body) ->
      D (ClassDef(pos, expr, i_kind, body))
  | D BeginBlock(_, el) -> D (BeginBlock(pos, el))
  | D EndBlock(_, el) -> D (EndBlock(pos, el))
  | S ExnBlock(body, _) -> S (ExnBlock(body, pos))
  | S Case(_, block) -> S (Case(pos, block))
  | S Return(_, el) -> S (Return(pos, el))
  | S Yield(_, el) -> S (Yield(pos, el))
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
  | s -> Id((s, pos), id_kind s pos)


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
