open Common
open Ast_ruby

(*****************************************************************************)
(* Comparators *)
(*****************************************************************************)

let compare_expr = Ast_ruby.compare_expr
let compare_ast = Ast_ruby.compare_stmts

let equal_expr = Ast_ruby.equal_expr
let equal_ast = Ast_ruby.equal_stmts
let equal_any = Ast_ruby.equal_any

(*****************************************************************************)
(* tok_of *)
(*****************************************************************************)

let tok_of_literal = function
  | Num (_, pos)
  | Float (_, pos)
  | String (_, pos)
  | Regexp (_, pos)
  | Atom (_, pos)
  | Nil pos | Self pos | Super pos
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
  | S For2 (pos, _  , _ , _, _ )
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
  | S Break (pos, _) | S Next (pos, _)
  | S Redo (pos, _) | S Retry (pos, _)

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
      | S For2(pos,es, in_, expr, el) ->
          S (For2(pos,
            ((mod_expr f) es), 
            in_,
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

      | S Break(pos, el) -> S (Break(pos, (List.map (mod_expr f) el)))
      | S Next(pos, el) -> S (Next(pos, (List.map (mod_expr f) el)))
      | S Redo(pos, el) -> S (Redo(pos, (List.map (mod_expr f) el)))
      | S Retry(pos, el) -> S (Retry(pos, (List.map (mod_expr f) el)))

      | S Block(el, pos) -> S (Block((List.map (mod_expr f) el), pos))

      | UOperator _ | Operator _ | Id _ | Literal _ | S Empty -> 
          expr
  in
    f processed_expr

and mod_case_block f block =
  {
    case_guard = Common.map_opt (mod_expr f) block.case_guard;
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
  | Super _ -> Super pos
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
