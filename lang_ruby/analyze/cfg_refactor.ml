
open Printf
open Cfg
open Cfg_printer
open Utils

module C = Cfg.Abbr

type 'a acc = {
  q : 'a DQueue.t;
  seen : StrSet.t;
  super_args : (star_expr list * expr option) option;
}

let acc_empty old = 
  {q = DQueue.empty; seen = StrSet.empty;
   super_args = old.super_args}

let acc_enqueue stmt acc = 
  let q = DQueue.enqueue stmt acc.q in
    {acc with q = q}

let acc_emptyq acc = {acc with q=DQueue.empty}

let acc_seen acc1 acc2 = 
  {acc1 with seen = StrSet.union acc1.seen acc2.seen}

let add_seen x acc = {acc with seen = StrSet.add x acc.seen}

let acc_append acc1 acc2 = 
  {q = DQueue.append acc1.q acc2.q;
   seen = StrSet.union acc1.seen acc2.seen;
   super_args = (assert(acc1.super_args == acc2.super_args);acc1.super_args);
  }

let rec seen_lhs acc (lhs:lhs) = match lhs with
  | `ID_Var(_,str) | `Star (`ID_Var(_,str)) ->
      {acc with seen = StrSet.add str acc.seen}

  | #identifier | `Star (#identifier) -> acc
  | `Tuple ls -> List.fold_left seen_lhs acc ls

let uniq_counter = ref 0
let uniq () = incr uniq_counter; !uniq_counter

let fresh acc = 
  let fresh_name = "__tmp_" ^ (string_of_int (uniq ())) in
  let id = `ID_Var (`Var_Local, fresh_name) in
    (seen_lhs acc id), id

let fresh_global pos = 
  let name = sprintf "$__druby_global_%d_%d" pos.Lexing.pos_lnum (uniq()) in
    `ID_Var(`Var_Global, name)

let formal_counter = ref 0
let fresh_formal () = incr formal_counter; !formal_counter

let re_init () = uniq_counter := 0

let gen_super_args params =
  let rec work = function
  | `Formal_default(s,_)
  | `Formal_meth_id s -> `ID_Var(`Var_Local,s)
  | `Formal_amp s -> assert false
  | `Formal_star s -> `Star (`ID_Var(`Var_Local,s))
  in
    match List.rev params with
      | (`Formal_amp s)::rest -> 
          let args = List.rev_map work rest in
            Some (args, Some (`ID_Var(`Var_Local,s)))
      | lst  -> 
          Some (List.rev_map work lst, None)

let make_call_expr acc targ msg args cb pos = 
  let acc, lhs = fresh acc in
  let call = C.mcall ~lhs ?targ msg args ?cb pos in
    (acc_enqueue call acc), lhs

(* convert a single quoted string into a double quoated string.  We do
   three transformations:
   - substitute \' for just '
   - substitute \c for \\c where c is not another slash
     This step is broken into several subcases to try and keep an
     ounce of readability in the regexps
   - explicitly esacpe special characters such as # or {
*)
let unescape_single_string s = 
  (* first unescaped any single quotes: \' becomes ' *)
  let s = Str.global_replace (Str.regexp "\\\\'") "'" s in
    (* ^\[^\] *)
  let start_re = Str.regexp "^\\\\\\([^\\\\]\\)" in
  let t = "\\\\\\\\\\1" in
  let s = Str.global_replace start_re t s in
    (* ([^\])\(^[\]) *)
  let no_prefix = Str.regexp "\\([^\\\\]\\)\\\\\\([^\\\\]\\)" in
  let t = "\\1\\\\\\\\\\2" in
  let s = Str.global_replace no_prefix t s in
    (* (\\)+\[^\]*)
  let some_prefix = Str.regexp "\\(\\\\\\\\\\)+\\\\\\([^\\\\]\\)" in
  let t = "\\1\\\\\\\\\\2" in
  let s = Str.global_replace some_prefix t s in
    (* convert to double string by escaping necessary chars *)
    escape_chars s ['#';'{';'}';'[';']';'(';')' ]


(* Bitwise the value of Regexp::<s> onto <fin> updating the code
   accumulator as necessary.
*)
let or_opt acc fin lang once s pos =
  let v = `ID_Scope(`ID_Var(`Var_Constant, "Regexp"),s) in
    match fin with
      | Some e ->
          let acc, lhs = fresh acc in
          let call = C.mcall ~lhs ~targ:e (`ID_Operator(Op_BOr)) [v] pos in
          let acc = acc_enqueue call acc in
            acc, Some lhs, lang, once
      | None ->
          acc, Some v, lang, once

(* Takes a string of regexp options like "ixm", and constructs the
   necessary arguments to Regexp.new for those modifiers.  Returns the
   4-tuple <acc,mod,lang,once> where

   acc - code accumulator
   mod - cfg option representing the bitwise or the corresponding options
   lang - option type containing the string identifier for a language
   once - boolean representing if this regexp should be interpreted once
*)
let parse_regexp_options acc str pos : (stmt acc * expr option * char option * bool) =
  let build_opt (acc,fin,lang,once) = function
    | 'i' -> or_opt acc fin lang once "IGNORECASE" pos
    | 'x' -> or_opt acc fin lang once "EXTENDED" pos
    | 'm' -> or_opt acc fin lang once"MULTILINE" pos
    | 'o' -> (acc,fin,lang,true)
    | 'n' | 'N' | 'e' | 'E' | 's' | 'S' | 'u' | 'U' as c-> 
        begin match lang with
          | None -> acc,fin,Some c,once
          | Some c' ->
              Log.err ~ctx:(Log.of_loc pos)
                "multiple language modifiers for regexp: %c %c??"
                c c';
              acc,fin,Some c',once
        end
    | c -> Log.fatal (Log.of_loc pos) "unknown regexp modifier: %c" c
  in
    string_fold_left build_opt (acc,None,None,false) str

let escape_regexp re = 
  let escaped = ['{'; '}'] in
  let buf = Buffer.create 32 in
  let _ =
    string_fold_left 
      (fun state c -> match state with
         | `NoEsc ->
             Buffer.add_char buf c;
             if c = '\\' then `Esc
             else if c = '[' 
             then `CharClass
             else `NoEsc

         | `Esc -> Buffer.add_char buf c; `NoEsc

         | `CharClass ->
             if List.mem c escaped
             then Buffer.add_char buf '\\';
             Buffer.add_char buf c;
             if c = '\\' then `EscClass
             else if c = ']' 
             then `NoEsc
             else `CharClass

         | `EscClass ->
             Buffer.add_char buf c;
             `CharClass
      ) (`NoEsc) re
  in
  Buffer.contents buf

(** [refactor_list f (acc,res) lst] returns the the pair (acc',es)
    where acc' is the DQueue of hoisted values and es is the list of
    refactored values.  The function 'f' performs the individual
    refactoring (but must return a pair).
*)
let rec refactor_list f (acc,lst) = function
  | [] -> acc, lst
  | hd::tl -> 
      let acc,e = f acc hd in
      let es = DQueue.enqueue e lst in
        refactor_list f (acc,es) tl

let is_literal = function
  | "true"
  | "false" -> true
  | _ -> false

let is_special = function
  | "true" | "false"
  | "__FILE__" | "__LINE__" -> 
      true
  | _ -> false

let special_of_string pos = function
  | "true"  -> `ID_True
  | "false" -> `ID_False
  | "__FILE__" -> `Lit_String (pos.Lexing.pos_fname)
  | "__LINE__" -> `Lit_FixNum (pos.Lexing.pos_lnum)
  | _ -> raise (Invalid_argument "special_of_string")

let rec refactor_id_kind pos : Ast.id_kind -> var_kind = function
  | Ast.ID_Lowercase -> `Var_Local
  | Ast.ID_Instance -> `Var_Instance
  | Ast.ID_Class -> `Var_Class
  | Ast.ID_Global -> `Var_Global
  | Ast.ID_Uppercase -> `Var_Constant
  | Ast.ID_Builtin -> `Var_Builtin
  | Ast.ID_Assign ik -> 
      Log.fatal (Log.of_loc pos)
        "trying to refactor id_assign, but should be handled elsewhere"

let refactor_builtin_or_global pos = function
  | Ast.ID_Builtin -> `Var_Builtin
  | Ast.ID_Global -> `Var_Global
  | _ ->
      Log.fatal (Log.of_loc pos)
        "trying to refactor other kind into builtin or global"

let refactor_uop pos = function
  | Ast.Op_UMinus -> Op_UMinus
  | Ast.Op_UPlus -> Op_UPlus
  | Ast.Op_UTilde -> Op_UTilde
  | Ast.Op_UStar
  | Ast.Op_UBang
  | Ast.Op_UNot
  | Ast.Op_UAmper
  | Ast.Op_UScope -> 
      Log.fatal (Log.of_loc pos) "trying to refactor construct posing as unary op"

let refactor_binop pos : Ast.binary_op -> binary_op = function
  | Ast.Op_PLUS -> Op_Plus
  | Ast.Op_MINUS -> Op_Minus
  | Ast.Op_TIMES -> Op_Times
  | Ast.Op_REM -> Op_Rem
  | Ast.Op_DIV -> Op_Div
  | Ast.Op_CMP -> Op_CMP
  | Ast.Op_EQ -> Op_EQ
  | Ast.Op_EQQ -> Op_EQQ
  | Ast.Op_GEQ -> Op_GEQ
  | Ast.Op_LEQ -> Op_LEQ
  | Ast.Op_LT -> Op_LT
  | Ast.Op_GT -> Op_GT
  | Ast.Op_BAND -> Op_BAnd
  | Ast.Op_BOR -> Op_BOr
  | Ast.Op_MATCH -> Op_Match
  | Ast.Op_XOR -> Op_XOR
  | Ast.Op_POW -> Op_Pow
  | Ast.Op_AREF -> Op_ARef
  | Ast.Op_ASET -> Op_ASet
  | Ast.Op_LSHIFT -> Op_LShift
  | Ast.Op_RSHIFT -> Op_RShift

  | Ast.Op_NEQ
  | Ast.Op_NMATCH
  | Ast.Op_OP_ASGN _
  | Ast.Op_ASSIGN
  | Ast.Op_AND
  | Ast.Op_OR
  | Ast.Op_kAND
  | Ast.Op_kOR
  | Ast.Op_ASSOC
  | Ast.Op_DOT
  | Ast.Op_SCOPE
  | Ast.Op_DOT2
  | Ast.Op_DOT3 as bop -> 
      Log.fatal (Log.of_loc pos)
        "trying to refactor construct posing as binary op: %s"
        (Ast.str_binop bop)

let msg_id_from_string = function
  | "+" -> `ID_Operator Op_Plus
  | "-" -> `ID_Operator Op_Minus    
  | "*" -> `ID_Operator Op_Times    
  | "/" -> `ID_Operator Op_Div      
  | "%" -> `ID_Operator Op_Rem      
  | "<=>" -> `ID_Operator Op_CMP          
  | "==" -> `ID_Operator Op_EQ          
  | "===" -> `ID_Operator Op_EQQ          
  | ">=" -> `ID_Operator Op_GEQ          
  | "<=" -> `ID_Operator Op_LEQ          
  | "<" -> `ID_Operator Op_LT          
  | ">" -> `ID_Operator Op_GT          
  | "&" -> `ID_Operator Op_BAnd     
  | "|" -> `ID_Operator Op_BOr      
  | "=~" -> `ID_Operator Op_Match
  | "^" -> `ID_Operator Op_XOR      
  | "**" -> `ID_Operator Op_Pow
  | "[]" -> `ID_Operator Op_ARef
  | "[]=" -> `ID_Operator Op_ASet
  | "<<" -> `ID_Operator Op_LShift
  | ">>" -> `ID_Operator Op_RShift

  | "-@" -> `ID_UOperator Op_UMinus
  | "+@" -> `ID_UOperator Op_UPlus
  | "~@" | "~" -> `ID_UOperator Op_UTilde
  | s -> `ID_MethodName s

let rec tuple_of_lhs (lhs:lhs) pos : tuple_expr = match lhs with
  | #identifier as id -> id
  | `Tuple(l) -> 
      let l' = List.map (fun x -> tuple_of_lhs x pos) l in
        `Tuple(l')
  | `Star (#identifier as id) -> `Star id

let make_tuple_option : tuple_expr list -> tuple_expr option = function
  | [] -> None
  | [x] -> Some x
  | lst -> Some (`Tuple lst)

let make_assignable_msg (m : msg_id) : msg_id = match m with
  | `ID_MethodName s -> `ID_Assign s

  | `ID_Assign _ ->
      Log.fatal Log.empty "make_assignable_msg: already assignable????"

  | `ID_Operator Op_ARef -> `ID_Operator Op_ASet

  | `ID_Operator _
  | `ID_UOperator _ -> 
      Log.fatal Log.empty "make_assignable_msg: non [] operator????"

  | `ID_Super -> Log.fatal Log.empty "make_assignable_msg: super??"
        

let replace_last f = function
  | [] -> []
  | l -> match List.rev l with
      | [] -> assert false
      | hd::tl -> List.rev ( (f hd):: tl)

let method_formal_of_id : identifier -> method_formal_param = function
  | `ID_Var(`Var_Local,s) -> `Formal_meth_id(s)
  | _ -> Log.fatal Log.empty "method_formal_of_id: non-id"

let block_formal_of_id : identifier -> block_formal_param = function
  | `ID_Var(k,s) -> `Formal_block_id(k,s)
  | _ -> 
      Log.fatal Log.empty "block_formal_of_id: non-id"

(* convert the last value in the statement given by add_f *)
let rec convert_to_return (add_f : tuple_expr -> stmt_node) acc stmt = match stmt.snode with
  | ExnBlock eb -> 
      let q_to_stmt q = C.seq (DQueue.to_list q) stmt.pos in
      let convert_rescue rb = 
        {rb with rescue_body = q_to_stmt (convert_to_return add_f acc rb.rescue_body)}
      in
      (* body doesn't return if there's an else stmt *)
      let body = match eb.exn_else with
	| Some(_) -> eb.exn_body
	| None -> q_to_stmt (convert_to_return add_f acc eb.exn_body)
      in
      let rescue = List.map convert_rescue eb.exn_rescue in
      let ensure = eb.exn_ensure in (* ensure doesn't return a value *)
      let eelse = map_opt q_to_stmt
        (map_opt (convert_to_return add_f acc) eb.exn_else)
      in
      let eblk = C.exnblock body rescue ?ensure ?eelse stmt.pos in
        DQueue.enqueue eblk DQueue.empty

  | Case cb ->
      let whens' = 
        List.map
          (fun (gs,bs) ->
            let q = convert_to_return add_f acc bs in
            let bs' = C.seq (DQueue.to_list q) bs.pos in
              (gs,bs')
          ) cb.case_whens
      in
      let else' = map_opt
        (fun s -> 
           C.seq (DQueue.to_list (convert_to_return add_f acc s)) s.pos
        ) cb.case_else
      in
      let cb' = {cb with case_whens=whens'; case_else = else'} in
        DQueue.enqueue (mkstmt (Case cb') stmt.pos) DQueue.empty

  | For(params,guard,body) ->
      let q = DQueue.enqueue stmt DQueue.empty in
      let ret = mkstmt (add_f (guard :> tuple_expr)) stmt.pos in
        DQueue.enqueue ret q

  | If(g,t,f) ->
      let tq = DQueue.to_list (convert_to_return add_f acc t) in
      let t' = C.seq tq stmt.pos in
      let fq = DQueue.to_list (convert_to_return add_f acc f) in
      let f' = C.seq fq stmt.pos in
      let if'  = mkstmt (If(g,t',f')) stmt.pos in
        DQueue.enqueue if' DQueue.empty

  | Return _ -> DQueue.enqueue stmt DQueue.empty

  | Seq slist -> 
      let q = DQueue.from_list slist in
      let q,last = DQueue.pop_back q in
        DQueue.append q (convert_to_return add_f acc last)

  | Expression e ->
      let ret = mkstmt (add_f (e :> tuple_expr)) stmt.pos in
        DQueue.enqueue ret DQueue.empty
      
  | Yield(None,args) ->
      (* we don't need to track v here since its immediately dead
         after the return *)
      let _, v = fresh (acc_emptyq acc) in
      let yield = C.yield ~lhs:v ~args stmt.pos in
      let ret = mkstmt (add_f v) stmt.pos in
      let q = DQueue.enqueue yield DQueue.empty in
        DQueue.enqueue ret q

  | MethodCall(None,mc) ->
      (* we don't need to track v here since its immediately dead
         after the return *)
      let _, v = fresh (acc_emptyq acc) in
      let meth = mkstmt (MethodCall(Some v,mc)) stmt.pos in
      let ret = mkstmt (add_f v) stmt.pos in
      let q = DQueue.enqueue meth DQueue.empty in
        DQueue.enqueue ret q

  | Assign(lhs,_)
  | MethodCall(Some lhs,_)
  | Yield(Some lhs,_) ->
      let ret = mkstmt (add_f (tuple_of_lhs lhs stmt.pos)) stmt.pos in
      let q = DQueue.enqueue stmt DQueue.empty in
        DQueue.enqueue ret q

  | Defined(id,s) ->
      let ret = mkstmt (add_f (id :> tuple_expr)) stmt.pos in
      let q = DQueue.enqueue stmt DQueue.empty in
        DQueue.enqueue ret q

  | Break _ | Redo | Retry | Next _ -> 
      DQueue.enqueue stmt DQueue.empty (* control jumps elsewhere *)

  | Undef _
  | While _
  | Module _
  | Method _
  | Class _
  | Alias _ -> 
      let q = DQueue.enqueue stmt DQueue.empty in
      let ret = mkstmt (add_f `ID_Nil) stmt.pos in
        DQueue.enqueue ret q

  | Begin _ | End _ -> 
      Log.fatal (Log.of_loc stmt.pos)
        "BEGIN or END block can not be used inside a method"

open Visitor
class proc_transformer = object
  inherit default_visitor
  method visit_stmt s = match s.snode with
    | Return args -> ChangeTo (update_stmt s (Next args))

    | Module _ | Method _ | Class _ (* new scope *)
    | While _ | For _ | MethodCall _ (* nested blocks *) ->
        SkipChildren
        
    | _ -> DoChildren

end

let proc_transform block = match block with 
  | CB_Arg _ -> block
  | CB_Block(params,body) ->
      let visitor = new proc_transformer in
      let body' = visit_stmt visitor body in
        CB_Block(params,body')

let add_final_return return_f acc pos =
  if DQueue.is_empty acc.q
  then acc_enqueue (mkstmt (return_f `ID_Nil) pos) acc
  else 
    let q,last = DQueue.pop_back acc.q in
    let last_q = convert_to_return return_f acc last in
      (* adding the final return shouldn't affect the seen set *)
      {acc with q = DQueue.append q last_q}

let refactor_formal_list f acc lst pos = 
  let acc, rlst = List.fold_left
    (fun (acc,lst) formal ->
       let acc,formal' = f acc formal pos in
         acc, formal'::lst
    ) (acc,[]) lst
  in acc, List.rev rlst

let rec refactor_expr (acc:stmt acc) (e : Ast.expr) : stmt acc * Cfg.expr = 
  match e with
    | Ast.E_Binop(_, Ast.Op_OP_ASGN _, _, _) | Ast.E_Annotate _
    | Ast.E_If _ | Ast.E_Yield _ | Ast.E_Return _  
    | Ast.E_Case _ | Ast.E_ExnBlock _ | Ast.E_EndBlock _
    | Ast.E_BeginBlock _  | Ast.E_CodeBlock _ | Ast.E_MethodDef _ 
    | Ast.E_For _ | Ast.E_Unless _ | Ast.E_Until _
    | Ast.E_While _ | Ast.E_Ternary _ | Ast.E_Alias _ | Ast.E_Undef _ as s ->
        (* These cases are where stmts are embedded in expressions,
           like x = (if g then 1 else 2 end) To handle this, we create
           a fresh variable are store the result of each possible
           return of the stmt.  e.g.  
           if g then t = 1 else t = 2 end; x = t
        *)
        let acc' = refactor_stmt acc s in
          (* grab the last statement, think x = (s1();s2()) *)
        let rest,s' = DQueue.pop_back acc'.q in
        let acc = {acc' with q = rest} in
        let acc, v = fresh acc in
          (* move the assignment expression into the statement *)
        let ss = add_last_assign ~do_break:false v s' in
        let acc = acc_enqueue ss acc in
          acc, v

    | Ast.E_ClassDef _
    | Ast.E_ModuleDef _ -> 
        let acc, v = fresh acc in
        let st_acc = refactor_stmt (acc_emptyq acc) e in
        let acc = acc_seen acc st_acc in
        let rest,s = DQueue.pop_back st_acc.q in
        let acc = acc_append acc {acc with q=rest} in
        let s' = match s.snode with
          | Class(None,ck,body) ->
              mkstmt (Class(Some v,ck,body)) s.pos
          | Module(None,name,body) ->
              mkstmt (Module(Some v,name,body)) s.pos
          | _ -> 
              Log.fatal (Log.of_loc (Ast.pos_of e))
                "[BUG] Class/module? xlate error: %a(%a)" 
                CodePrinter.format_stmt s
                CodePrinter.format_identifier v
        in
          acc_enqueue s' acc, v


    | Ast.E_Unary(Ast.Op_UScope,e,pos) ->
        let acc,e' = refactor_id acc e in
        let s = match e' with
          | `ID_Var(`Var_Constant, s) -> s
          | _ -> Log.fatal (Log.of_loc pos) "unknown right hand of uscope: %a"
              CodePrinter.format_identifier e'
        in
          acc, (`ID_UScope s)

    | Ast.E_Unary(Ast.Op_UMinus, Ast.E_Literal(Ast.Lit_FixNum i,_), pos) -> 
        acc, `Lit_FixNum (-i)

    | Ast.E_Unary(Ast.Op_UMinus, Ast.E_Literal(Ast.Lit_BigNum i,_), pos) -> 
        acc, `Lit_BigNum (Big_int.minus_big_int i)

    | Ast.E_Unary(Ast.Op_UMinus, Ast.E_Literal(Ast.Lit_Float(s,f),_), pos) -> 
        assert(s.[0] != '-');
        acc, `Lit_Float("-" ^ s, (~-. f))

    | Ast.E_Unary((Ast.Op_UBang | Ast.Op_UNot),e,pos) ->
        let acc,v = fresh acc in
        let t = C.assign v `ID_True pos in
        let f = C.assign v `ID_False pos in
        let acc,e' = refactor_expr acc e in
        let acc = acc_enqueue (C.if_s e' ~t:f ~f:t pos) acc in
          acc, v

    | Ast.E_Unary(uop,e, pos) -> 
        let acc,e' = refactor_expr acc e in
        let msg = `ID_UOperator (refactor_uop pos uop) in
          make_call_expr acc (Some e') msg [] None pos

    (* A::m is really a method call *)
    | Ast.E_Binop(e1,Ast.Op_SCOPE,(Ast.E_Identifier(Ast.ID_Lowercase,_,_)),pos) -> 
        let acc,v = fresh acc in
        let acc = seen_lhs acc v in
        let e' = Ast.E_MethodCall(e,[], None, pos) in
          (refactor_method_call_assign acc (Some v) e'), v

    | Ast.E_Binop(e1,Ast.Op_SCOPE,e2, pos) -> 
        let acc,e1' = refactor_id acc e1 in
        let acc,e2' = refactor_id acc e2 in
        let s = match e2' with
          | `ID_Var(`Var_Constant, s) -> s
          | _ -> Log.fatal (Log.of_loc pos) "unknown right hand of scope: %a"
              CodePrinter.format_identifier e2'
        in
          acc, `ID_Scope(e1', s)

    | Ast.E_Binop(e1,Ast.Op_NEQ,e2,pos) ->
        let acc, t1 = fresh acc in
        let acc, t2 = fresh acc in
        let acc = refactor_binop_into_mc acc (Some t1) e1 Ast.Op_EQ e2 pos in
        let t = C.assign t2 `ID_True pos in
        let f = C.assign t2 `ID_False pos in
        let acc = acc_enqueue (C.if_s t1 ~t:f ~f:t pos) acc in
          acc, t2

    | Ast.E_Binop(e1,Ast.Op_NMATCH,e2,pos) ->
        let acc, t1 = fresh acc in
        let acc, t2 = fresh acc in
        let acc = refactor_binop_into_mc acc (Some t1) e1 Ast.Op_MATCH e2 pos in
        let t = C.assign t2 `ID_True pos in
        let f = C.assign t2 `ID_False pos in
        let acc = acc_enqueue (C.if_s t1 ~t:f ~f:t pos) acc in
          acc, t2

    | Ast.E_Binop(_,Ast.Op_DOT,_, pos) -> 
        Log.fatal (Log.of_loc pos) "refactor_expr got dot expr??"

    | Ast.E_Binop(e1, (Ast.Op_AND|Ast.Op_kAND), e2, pos) ->
        refactor_and_if acc e1 e2 pos
    | Ast.E_Binop(e1, (Ast.Op_OR|Ast.Op_kOR), e2, pos) ->
        refactor_or_if acc e1 e2 pos

    | Ast.E_Binop(e1,Ast.Op_ASSOC,e2,pos) ->
        let acc,e1' = refactor_expr acc e1 in
        let acc,e2' = refactor_expr acc e2 in
          acc, `Lit_Hash [ e1', e2']

    | Ast.E_Binop(e1,Ast.Op_DOT2,e2,pos) ->
        let acc,e1' = refactor_expr acc e1 in
        let acc,e2' = refactor_expr acc e2 in
          acc, `Lit_Range(false, e1', e2')

    | Ast.E_Binop(e1,Ast.Op_DOT3,e2,pos) ->
        let acc,e1' = refactor_expr acc e1 in
        let acc,e2' = refactor_expr acc e2 in
          acc, `Lit_Range(true, e1', e2')
            
    | Ast.E_Binop(e1,Ast.Op_ASSIGN,e2, pos) -> 
        let acc,e1',after = refactor_lhs acc e1 in
        let acc,e2' = refactor_tuple_expr acc e2 in
        let acc = acc_enqueue (C. assign e1' e2' pos) acc in
        let acc = acc_append acc after in
        let e1_id = match e1' with
          | #identifier as id -> id
          | _ -> Log.fatal Log.empty "nested multi-assign in refactor_expr"
        in
          acc, e1_id
            
    | Ast.E_Binop(e1,bop,e2, pos) -> 
        let acc, v = fresh acc in
        let acc = refactor_binop_into_mc acc (Some v) e1 bop e2 pos in
          acc, v

    | Ast.E_MethodCall(Ast.E_Identifier(Ast.ID_Lowercase,"defined?", p),
                       args, cb, pos) ->
        let _ = map_opt (fun _ -> Log.fatal (Log.of_loc pos) "cb for 'defined?' ??") cb in
        let acc, v = fresh acc in
          refactor_defined acc v args pos, v

    | Ast.E_MethodCall(msg, args, cb, pos) -> 
        let acc,v = fresh acc in
        let acc = seen_lhs acc v in
          (refactor_method_call_assign acc (Some v) e), v

    | Ast.E_Identifier(Ast.ID_Lowercase,s, pos) as e -> 
        if is_special s then acc, (special_of_string pos s)
        else if StrSet.mem s acc.seen
        then acc, `ID_Var(`Var_Local, s) 
        else if s = "super"
        then 
          let acc, lhs = fresh acc in
            (refactor_super acc (Some lhs) pos), lhs
        else refactor_expr acc (Ast.E_MethodCall(e,[],None,pos))

    (* handles $0 *)
    | Ast.E_Identifier(Ast.ID_Global, "0", pos) ->
        acc, `Lit_String Config.conf.Config.ruby_file

    | Ast.E_Identifier(ik,s, pos) -> 
        if is_special s then acc, (special_of_string pos s)
        else acc, `ID_Var(refactor_id_kind pos ik, s) 

    | Ast.E_Literal(Ast.Lit_Self,pos) -> acc, `ID_Self
    | Ast.E_Literal(l,pos) -> refactor_lit acc l pos

    | Ast.E_Tuple(l,pos) 
    | Ast.E_Array(l,pos) ->
        let acc,l' = refactor_list refactor_star_expr (acc,DQueue.empty) l in
          acc, `Lit_Array (DQueue.to_list l')

    | Ast.E_Hash(b,l,pos) ->
        let acc, hl = refactor_hash_list acc l pos in
          acc, `Lit_Hash hl

    | Ast.E_Operator(_,pos)
    | Ast.E_UOperator(_,pos) -> 
        (* these should be handled by the def/methodcall rules *)
        Log.fatal (Log.of_loc pos) "operator / uoperator in refactor_expr"
          

    | Ast.E_Block(l,pos) -> begin match List.rev l with
        | [] -> Log.fatal Log.empty "refactor_expr: empty block???"
        | last::rest ->
            let acc = refactor_stmt_list acc (List.rev rest) in
              refactor_expr acc last
      end
        
    | Ast.E_Empty -> Log.fatal Log.empty "refactor_expr: empty expr??"

and refactor_defined acc lhs args pos = 
  let arg = match args with
    | [x] -> x
    | _ -> Log.fatal (Log.of_loc pos) "multiple args to defined?"
  in
  let inside_acc = refactor_stmt (acc_emptyq acc) arg in
  let acc = acc_seen acc inside_acc in
  let s = C.seq (DQueue.to_list inside_acc.q) pos in
    acc_enqueue (C.defined lhs s pos) acc

and refactor_super acc lhs pos = match acc.super_args with
  | None -> Log.fatal (Log.of_loc pos) "super called outside of method"
  | Some(args,None) ->
      acc_enqueue (C.mcall ?lhs `ID_Super args pos) acc
  | Some(args,Some e) ->
      acc_enqueue (C.mcall ?lhs `ID_Super args ~cb:(CB_Arg e) pos) acc

(* turn /foo#{bar}/mods into a call to Regexp.new *)
and construct_explicit_regexp acc pos re_interp mods = 
  let acc,re_opts,lang,once = parse_regexp_options acc mods pos in
  let build_call acc lhs = 
    let re_interp = List.map
      (function
         | Ast.StrExpr _ as c -> c
         | Ast.StrChars s -> Ast.StrChars (escape_chars s ['\\'])
      ) re_interp 
    in
    let acc,str = refactor_interp_string acc re_interp pos in
    let new_opts = match lang with
      | None -> []
      | Some c -> [`Lit_String (String.make 1 c)]
    in
    let new_opts = match re_opts with
    | None -> str::`Lit_FixNum 0::new_opts
    | Some v -> str::v::new_opts
    in
    let call = C.mcall ~lhs ~targ:(`ID_UScope "Regexp")
      (`ID_MethodName "new") new_opts pos
    in      
      acc_enqueue call acc
  in
    if once then begin
      let glob = fresh_global pos in
      let f_acc = acc_emptyq acc in
      let f_acc = build_call f_acc glob in
      let acc = acc_seen acc f_acc in
      let if_e = 
        C.if_s glob pos
          ~t:(C.expr glob pos)
          ~f:(C.seq (DQueue.to_list f_acc.q) pos)
      in
        acc_enqueue if_e acc, glob
    end else
      let acc, lhs = fresh acc in
        build_call acc lhs, lhs

and refactor_interp_string acc istr pos = 
  let refactor_contents acc : Ast.string_contents -> stmt acc * expr = function
    | Ast.StrChars s -> acc, `Lit_String s
    | Ast.StrExpr ast_e -> 
        let acc, e = refactor_expr acc ast_e in
          make_call_expr acc (Some e) (`ID_MethodName "to_s") [] None pos
  in
  let rec helper acc expr_acc l = match l with
    | [] -> acc, expr_acc
    | hd::tl -> 
        let acc, e = refactor_contents acc hd in
        let acc, expr_acc = 
          make_call_expr acc (Some expr_acc) (`ID_Operator Op_Plus) [e] None pos 
        in
          helper acc expr_acc tl
  in
    (* unfold once to get start of expr_acc *)
    match istr with
      | [] -> acc, `Lit_String ""
      | hd::tl -> 
          let acc, e = refactor_contents acc hd in
            helper acc e tl
              
and refactor_lit acc (l : Ast.lit_kind) pos : stmt acc * expr = match l with
  | Ast.Lit_FixNum i -> acc, `Lit_FixNum i
  | Ast.Lit_BigNum i -> acc, `Lit_BigNum i
  | Ast.Lit_Float(s,f) -> acc, `Lit_Float(s,f)

  | Ast.Lit_String(Ast.String_Single s) -> 
        acc, `Lit_String (unescape_single_string s)

  | Ast.Lit_String(Ast.String_Double s) -> 
      refactor_interp_string acc s pos
  | Ast.Lit_String(Ast.String_Tick s) -> 
      let acc, e = refactor_interp_string acc s pos in
        make_call_expr acc None (`ID_MethodName "__backtick") [e] None pos

  | Ast.Lit_Atom [Ast.StrChars s] -> acc, `Lit_Atom s
  | Ast.Lit_Atom istr -> 
      let acc, str = refactor_interp_string acc istr pos in
        make_call_expr acc (Some str) (`ID_MethodName "to_sym") [] None pos

  | Ast.Lit_Regexp([Ast.StrChars s1],s2) -> 
      let s1' = escape_regexp s1 in
        acc, `Lit_Regexp(s1',s2)

  | Ast.Lit_Regexp(s1,s2) -> construct_explicit_regexp acc pos s1 s2
              
  | Ast.Lit_Nil -> acc, `ID_Nil
  | Ast.Lit_True -> acc, `ID_True
  | Ast.Lit_False -> acc, `ID_False
  | Ast.Lit_Self -> 
      Log.fatal Log.empty
        "trying to convert self to literal, but should be handled elsewhere"

and refactor_star_expr (acc:stmt acc) e : stmt acc * star_expr = match e with
  | Ast.E_Unary(Ast.Op_UStar, e, pos) -> 
      let acc, e' = refactor_expr acc e in
        acc, `Star e'
  | e ->
      let acc, e' = refactor_expr acc e in
        acc, (e' :> star_expr)

and refactor_method_arg_no_cb acc e : stmt acc * star_expr = match e with 
  | Ast.E_Unary(Ast.Op_UAmper, e, pos) -> 
      Log.fatal (Log.of_loc pos) "unexpected & arg in method arguments"
  | e -> refactor_star_expr acc e

and refactor_method_args_and_cb acc arg_list cb = 
  let acc, lst, cb_arg = match List.rev arg_list, cb with
    | Ast.E_Unary(Ast.Op_UAmper, e, pos)::rest, None -> 
        let acc, e' = refactor_expr acc e in
          acc, (List.rev rest), Some (CB_Arg e')
    | (Ast.E_Unary(Ast.Op_UAmper, _, pos)::_), Some _ -> 
        Log.fatal (Log.of_loc pos)
          "method can't have both an &-argument and code block"
    | lst, Some cb_e -> acc, arg_list, Some (refactor_codeblock acc cb_e)
    | lst, None -> acc, arg_list, None
  in
  let acc,final_rev_args = List.fold_left
    (fun (acc,lst) x -> 
       let acc, arg = refactor_method_arg_no_cb acc x in
         acc, arg::lst
    ) (acc,[]) lst
  in
    acc, (List.rev final_rev_args), cb_arg

and refactor_hash_list acc l pos =
  let rec pair_list acc = function
    | [] -> acc
    | x::[] -> 
        Log.fatal (Log.of_loc pos) "odd number of elements in hash: %s" 
          (format_to_string CodePrinter.format_expr x)
    | x::y::tl -> pair_list ((x,y)::acc) tl
  in
    match l with
      | [] -> acc, []
      | (Ast.E_Binop(_,Ast.Op_ASSOC,_,_))::_ ->
          let acc,rlst = 
            List.fold_left
              (fun (acc,lst) -> function
                 | Ast.E_Binop(e1,Ast.Op_ASSOC,e2,pos) ->
                     let acc,e1' = refactor_expr acc e1 in
                     let acc,e2' = refactor_expr acc e2 in
                       acc, (e1', e2')::lst
                 | _ -> Log.fatal (Log.of_loc pos) "non assoc expression in hash list?"
              ) (acc,[]) l
          in
            acc, List.rev rlst

      | _::_ -> 
          let acc,l' = refactor_list refactor_expr (acc,DQueue.empty) l in
          let lst = List.rev (pair_list [] (DQueue.to_list l')) in
            acc, lst

and refactor_binop_into_mc acc res e1 bop e2 pos = 
  let acc,e1' = refactor_expr acc e1 in
  let acc,e2' = refactor_method_arg_no_cb acc e2 in 
  let call = C.mcall ?lhs:res ~targ:e1' (`ID_Operator (refactor_binop pos bop))
    [e2'] pos
  in acc_enqueue call acc

and refactor_and_if (acc:stmt acc) l r pos : stmt acc * expr = 
  let acc,l' = refactor_expr acc l in
    (* we use a separate accumulator since we only want the refactored
       expressions to execute if the initial condition was true.  Thus we
       place all refactoring code inside the true branch of the If *)
  let r_acc = refactor_stmt (acc_emptyq acc) r in
  let acc, v = fresh acc in
  let vl = C.assign v l' pos in
  let v_acc = add_final_return (fun t -> Assign(v,t)) r_acc pos in
  let vr = C.seq (DQueue.to_list v_acc.q) pos in
    (* the && operator returns the value of the last expression evaluated, 
       thus "[not true] && _" returns [not true]
       and  "[not false] && x" returns x
    *)
  let acc = acc_enqueue (C.if_s l' ~t:vr ~f:vl pos) acc in
    (acc_seen acc v_acc), v

and refactor_or_if acc l r pos = 
  let acc,l' = refactor_expr acc l in
    (* Like the and refactoring above, we use a separate accumulator
       since we only want the refactored expressions to execute if the
       initial condition was false.  Thus we place all refactoring code
       inside the true branch of the If *)
  let acc, v = fresh acc in
  let vl = C.assign v l' pos in
  let return_f t = Assign(v,t) in
  let r_acc = refactor_stmt (acc_emptyq acc) r in
  let v_acc = add_final_return return_f r_acc pos in
  let vr = C.seq (DQueue.to_list v_acc.q) pos in
    (* the || operator returns the value of the last expression evaluated, 
       thus "[not true] || x" returns x
       and  "[not false] || x" returns [not false]
    *)
  let acc = acc_enqueue (C.if_s l' ~t:vl ~f:vr pos) acc in
    (acc_seen acc v_acc), v
      
and refactor_tuple_expr (acc:stmt acc) (e : Ast.expr) : stmt acc * Cfg.tuple_expr =
  match e with
    | Ast.E_Tuple(l,pos) -> 
        let acc,l' = refactor_list refactor_tuple_expr (acc,DQueue.empty) l in
          acc, `Tuple((DQueue.to_list l'))

    | Ast.E_Unary(Ast.Op_UStar, e, pos) -> 
        let acc, e' = refactor_tuple_expr acc e in
          begin match e' with
            | (#expr | #tuple) as e' -> acc, `Star e'
            | #star -> 
                Log.fatal Log.empty "refactor_tuple_expr: nested / double star expression"
          end
    | _ -> 
        let acc, expr = refactor_expr acc e in
        let expr = (expr :> tuple_expr) in
          acc, expr

and refactor_lhs acc e : (stmt acc * lhs * stmt acc) = 
  let acc,lhs,after = match e with
    | Ast.E_Tuple(l,pos) -> 
        let rec work (acc,lst,after) = function
          | [] -> acc, lst,after
          | hd::tl -> 
              let acc,e,after' = refactor_lhs acc hd in
              let after = acc_append after after' in
              let es = DQueue.enqueue e lst in
                work (acc,es,after) tl
        in
        let acc,l',after = work (acc,DQueue.empty,acc_emptyq acc) l in
          acc, `Tuple((DQueue.to_list l')), after
            
    | Ast.E_UOperator(Ast.Op_UStar,pos) ->
        let acc, v = fresh acc in
          acc, (`Star v), acc_emptyq acc
          
    | Ast.E_Unary(Ast.Op_UStar, e, pos) -> 
        let acc, e',after = refactor_lhs acc e in
          begin match e' with
            | #identifier as id -> acc, `Star id, after
            | `Tuple _| `Star _ -> 
                Log.fatal (Log.of_loc pos) "refactor_lhs: nested star?"
          end

    | Ast.E_Identifier(Ast.ID_Lowercase,s, pos)  -> 
        if is_literal s then Log.fatal Log.empty "lhs literal?"
        else acc, `ID_Var(`Var_Local, s), acc_emptyq acc

    | Ast.E_Identifier(ik,s, pos) -> 
        if is_literal s then Log.fatal Log.empty "lhs literal?"
        else acc, `ID_Var(refactor_id_kind pos ik, s), acc_emptyq acc
          
    | Ast.E_Binop(e1,Ast.Op_DOT,e2,pos) ->
        Log.fatal Log.empty "dot expression on lhs?"

    | Ast.E_MethodCall(Ast.E_Binop(targ,Ast.Op_DOT,msg,_),args,None,pos) ->
        let after = acc_emptyq acc in
        let after,targ' = refactor_expr after targ in
        let after,msg' = refactor_msg after msg in
        let msg' = make_assignable_msg msg' in
        let after, args', cb = refactor_method_args_and_cb after args None in
        let after, last_arg = fresh after in
        let args' = args' @ [last_arg] in
        let mc_stmt = C.mcall ~targ:targ' msg' args' ?cb pos in
        let after = acc_enqueue mc_stmt after in
          acc, last_arg, after
            
    | Ast.E_MethodCall _ ->
        Log.fatal Log.empty "unhandled methodcall on lhs?"

    | _ -> 
        match refactor_expr acc e with
          | acc, (#identifier as id) -> acc, id, acc_emptyq acc
          | acc, #literal -> Log.fatal Log.empty "lhs literal?"
  in
  let acc = seen_lhs acc lhs in
    acc, lhs,after

and refactor_id (acc:stmt acc) e : stmt acc * identifier = 
  match refactor_expr acc e with 
    | (_,#identifier) as accid -> accid
    | _,(#literal as l) -> 
        Log.fatal (Log.of_loc (Ast.pos_of e)) "lhs_of_expr: literal %a" 
          CodePrinter.format_literal l

and refactor_msg (acc:stmt acc) msg : stmt acc * msg_id = match msg with
  | Ast.E_Operator(bop, pos) ->  acc, `ID_Operator (refactor_binop pos bop)
  | Ast.E_UOperator(uop, pos) -> acc, `ID_UOperator (refactor_uop pos uop)

  | Ast.E_Identifier((Ast.ID_Lowercase | Ast.ID_Uppercase ), s, pos) -> 
      acc, `ID_MethodName s

  | Ast.E_Identifier(Ast.ID_Assign(Ast.ID_Lowercase),s,pos)
  | Ast.E_Identifier(Ast.ID_Assign(Ast.ID_Uppercase),s, pos) -> 
      acc, `ID_Assign s

  | Ast.E_Literal((Ast.Lit_Nil | Ast.Lit_Self | Ast.Lit_True | Ast.Lit_False) as lk
                    ,pos) ->
      let lks = format_to_string Ast_printer.format_lit_kind lk in
        acc, `ID_MethodName lks
  | e ->
      Log.fatal (Log.of_loc (Ast.pos_of e)) "refactor_msg unknown msg: %s\n"
        (Ast_printer.string_of_expr e)

and refactor_symbol_or_msg (acc:stmt acc) sym_msg = match sym_msg with
  | Ast.E_Literal(Ast.Lit_Atom(interp),pos) ->
      let acc, e = refactor_interp_string acc interp pos in
        begin match e with 
          | `Lit_String s -> acc, msg_id_from_string s
          | _ -> Log.fatal Log.empty "alias with symbol interp string?"
        end
  | msg -> refactor_msg acc msg

and refactor_codeblock acc : Ast.expr -> codeblock = function
  | Ast.E_CodeBlock(_,formals,body, pos) ->
      let formals = match formals with
        | None -> [Ast.Formal_rest]
        | Some lst -> lst
      in
      let acc, formals = refactor_block_formal_list acc formals pos in
      let body_acc = refactor_stmt_list (acc_emptyq acc) body in
      let add_next e = Next(Some e) in
      let body_acc = add_final_return add_next body_acc pos in
        CB_Block(formals, C.seq (DQueue.to_list body_acc.q) pos)
  | _ -> 
      Log.fatal Log.empty "refactor_codeblock: non-codeblock"

and map_codeblock acc cb_o = map_opt (refactor_codeblock acc) cb_o

(* assign the last value of the stmt to [id].
   e.g., add_last_assign "x" [if_stmt] becomes
   if (guard) then x = result else x = result end
*)
and add_last_assign ~do_break (id:identifier) (s : stmt) : stmt = 
  let lhs = (id :> lhs) in match s.snode with
    | Seq(lst) -> begin match List.rev lst with
          (* empty evaluates to nil *)
        | [] -> C.assign lhs `ID_Nil s.pos
        | last::rest ->
            let last' = add_last_assign ~do_break id last in
              C.seq (List.rev (last'::rest)) s.pos
      end

    | Expression(e) -> (* x becomes id = x *)
        C.assign lhs e s.pos
          
    | Assign(e1,_) -> (* id2 = e becomes id2 = e; id = id2 *)
        let new_s = C.assign lhs (tuple_of_lhs e1 s.pos) s.pos in
          C.seq [s;new_s] s.pos

    | MethodCall(None, mc) -> (* x.f() becomes id = x.f() *)
        mkstmt (MethodCall(Some lhs, mc)) s.pos
    | Yield(None,es) -> C.yield ~lhs ~args:es s.pos
        
    | MethodCall(Some id2, _) (* id2=x.f() becomes id2=x.(); id=id2 *)
    | Yield(Some id2,_) ->
        let new_s = C.assign lhs (tuple_of_lhs id2 s.pos) s.pos in
          C.seq [s;new_s] s.pos

    | If(g,t,f) ->
        let t' = add_last_assign ~do_break id t in
        let f' = add_last_assign ~do_break id f in
          C.if_s g ~t:t' ~f:f' s.pos
            
    | While(g, body) ->
        let body' = add_last_assign ~do_break:true id body in
          (* the while body may never execute, so put a nil assign first *)
        let nilassgn = C.assign lhs `ID_Nil s.pos in
          C.seq [nilassgn;(C.while_s g body' s.pos)] s.pos

    | For(flist,g,body) -> 
        let new_s = C.assign id g s.pos in
        let body' = add_last_assign ~do_break:true id body in
          C.seq [new_s;C.for_s flist (id:>expr) body' s.pos] s.pos

    | Defined(id2,_) -> 
        let new_s = C.assign id id2 s.pos in
          C.seq [s;new_s] s.pos
            
    | ExnBlock(exn) -> 
        let body = add_last_assign ~do_break id exn.exn_body in
        let rescue = 
          List.map (fun r -> C.rblock r.rescue_guards
                      (add_last_assign ~do_break id r.rescue_body)
                   ) exn.exn_rescue;
        in
        let ensure = exn.exn_ensure in (* ensure does not return a value *)
        let eelse = map_opt (add_last_assign ~do_break id) exn.exn_else in
          C.exnblock body rescue ?ensure ?eelse s.pos
             
    | Case(cb) -> 
        let cb' = {
          case_guard = cb.case_guard;
          case_whens = 
            List.map
              (fun (gs,ss) -> 
                 gs, (add_last_assign ~do_break id ss)
              ) cb.case_whens;
          case_else = match cb.case_else with
            | None -> None 
            | Some x -> Some (add_last_assign ~do_break id x)
        } in mkstmt (Case cb') s.pos


    | Return _ -> s (* no need to assign the result of a return statement *)

    | Break o when do_break ->
        let rhs = default_opt `ID_Nil o in
          C.seq [C.assign id rhs s.pos;C.break s.pos] s.pos

    | Break _ | Redo | Retry | Next _ -> s (* control jumps elsewhere *)

    | Undef _
    | Module _ (* All of these return nil *)
    | Method _
    | Class _ 
    | Alias _
    | Begin _
    | End _ -> 
        let new_s = C.assign lhs `ID_Nil s.pos in
          C.seq [s;new_s] s.pos

and refactor_method_call_assign (acc:stmt acc) (lhs : lhs option) = function
  | Ast.E_MethodCall(Ast.E_Identifier(Ast.ID_Lowercase,"retry",p1), [], None, p2) ->
      acc_enqueue (C.retry p1) acc
  | Ast.E_MethodCall(Ast.E_Identifier(Ast.ID_Lowercase,"redo",p1), [], None, p2) ->
      acc_enqueue (C.redo p1) acc

  | Ast.E_MethodCall(Ast.E_Identifier(Ast.ID_Lowercase,"next",p1), args, None, p2) ->
      let env, es = refactor_list refactor_tuple_expr (acc, DQueue.empty) args in
      let tup = make_tuple_option (DQueue.to_list es) in
        acc_enqueue (C.next ?v:tup p1) acc

  | Ast.E_MethodCall(Ast.E_Identifier(Ast.ID_Lowercase,"break",p1), args, None, p2) ->
      let env, es = refactor_list refactor_tuple_expr (acc, DQueue.empty) args in
      let tup = make_tuple_option (DQueue.to_list es) in
        acc_enqueue (C.break ?v:tup p1) acc


  | Ast.E_MethodCall(Ast.E_Identifier(Ast.ID_Lowercase,"undef",p1), args, None, p2) ->
      Log.fatal (Log.of_loc p1) "undef as method?"

  | Ast.E_MethodCall(Ast.E_Identifier(Ast.ID_Lowercase,"defined?", p), args, cb, pos) ->
      let _ = map_opt (fun _ -> Log.fatal (Log.of_loc pos) "cb for 'defined?' ??") cb in
      let acc, v = match lhs with
        | None -> fresh acc
        | Some (#identifier as id) -> acc, id
        | Some _ -> Log.fatal (Log.of_loc pos) "non-ident = defined?"
      in
        refactor_defined acc v args pos

  | Ast.E_MethodCall(Ast.E_Identifier(Ast.ID_Lowercase,("proc"|"lambda" as m),_),
                     [], Some cb, pos) ->
      let cb' = refactor_codeblock acc cb in
      let cb' = proc_transform cb' in
      let call = C.mcall ?lhs (`ID_MethodName m) [] ~cb:cb' pos in
        acc_enqueue call acc

  | Ast.E_MethodCall(Ast.E_Identifier(Ast.ID_Lowercase,"define_method",_),
                     [Ast.E_Literal(Ast.Lit_Atom [Ast.StrChars mname],atompos)], 
                     Some(Ast.E_CodeBlock(_,params_o,cb_body,_)), pos) ->
      let params = Utils.default_opt [] params_o in
      let name = Ast.msg_of_str mname atompos in
      let body = {Ast.body_exprs = cb_body;rescue_exprs=[];ensure_expr=[];else_expr=[]} in
      let e' = Ast.E_MethodDef(name,params,body, pos) in
        refactor_stmt acc e'

  | Ast.E_MethodCall(Ast.E_Binop(targ,Ast.Op_DOT,msg,pos1), args, cb, pos2)
  | Ast.E_MethodCall(Ast.E_Binop(targ,Ast.Op_SCOPE,msg,pos1), args, cb, pos2) ->
      let acc,targ' = refactor_expr acc targ in
      let acc,msg' = refactor_msg acc msg in
      let acc,args',cb' = refactor_method_args_and_cb acc args cb in
      let call = C.mcall ?lhs ~targ:targ' msg' args' ?cb:cb' pos2 in
        acc_enqueue call acc

  | Ast.E_MethodCall(msg, args, cb, pos) ->
      let () = match msg, args with
        | Ast.E_Identifier(Ast.ID_Lowercase,("proc"|"lambda"),_),
          [Ast.E_Unary(Ast.Op_UAmper,_,_)] ->
            Log.err ~ctx:(Log.of_loc pos) "unsupported proc/lambda with &block param"
        | _ -> ()
      in
      let acc,msg' = match msg with
        | Ast.E_Identifier(Ast.ID_Lowercase, "super", _) -> 
            acc, `ID_Super
        | _ -> refactor_msg acc msg 
      in
      let acc,args',cb' = refactor_method_args_and_cb acc args cb in
      let call = C.mcall ?lhs msg' args' ?cb:cb' pos in
        acc_enqueue call acc

  | _ -> raise (Invalid_argument "CFG:refactor_method_call_assign")

(* return the accumulator and resulting lhs expression for an assignment *)
and refactor_assignment (acc: stmt acc) lhs rhs pos = match lhs,rhs with
  (* x[y] = z is really x.[]=(y,z) *)
  | Ast.E_MethodCall(
      Ast.E_Binop(targ, Ast.Op_DOT,Ast.E_Operator(Ast.Op_AREF,_),_), args,None,_),
    _ ->
      let acc,targ' = refactor_expr acc targ in
      let acc,rhs_arg = refactor_star_expr acc rhs in
      let acc,lhs_args = refactor_list refactor_star_expr (acc,DQueue.empty) args in
      let lhs_list = DQueue.to_list lhs_args in
      (* We need to be careful here because the lhs arguments can
         contain a star expression (x[*y] = z) and x.[]=( *y,z) is not
         a valid method call, thus we contruct a separate array for
         the arguments in this case. *)
      let acc,args = 
        if List.exists (function `Star _ -> true| _ -> false) lhs_list
        then begin
          (* construct tmp = [*lhs] + [rhs] *)
          let lhs_ary = `Lit_Array lhs_list in
          let rhs_ary = `Lit_Array [rhs_arg] in
          let acc, tmp = fresh acc in
          let call = C.mcall ~lhs:tmp ~targ:lhs_ary
            (`ID_Operator Op_Plus) [rhs_ary] pos 
          in
            (acc_enqueue call acc), [`Star tmp]
        end
        else
          (* lhs has no *-exprs, so safe to just concat args *)
          acc, DQueue.to_list (DQueue.enqueue rhs_arg lhs_args)
      in
      let call = C.mcall ~targ:targ' (`ID_Operator Op_ASet) args pos in
        acc_enqueue call acc

  (* handle x.y = e specially to avoid duping the rhs into a temp *)
  | Ast.E_MethodCall(Ast.E_Binop(targ,Ast.Op_DOT,msg,_),args,None,_), _ ->
      if args <> [] then Log.fatal (Log.of_loc pos) "args on lhs of assignable?";
      let acc,targ' = refactor_expr acc targ in
      let acc,msg' = refactor_msg acc msg in
      let msg' = make_assignable_msg msg' in
      let acc,arg = refactor_method_arg_no_cb acc rhs in
        acc_enqueue (C.mcall ~targ:targ' msg' [arg] pos) acc

  | _, Ast.E_Tuple _ | Ast.E_Tuple _, _ ->
      let acc,rhs' = refactor_tuple_expr acc rhs in
      let acc,lhs',after = refactor_lhs acc lhs in
      let acc = acc_enqueue (C.assign lhs' rhs' pos) acc in
        acc_append acc after

  (* optimization to eliminate an extra temporary using method assign *)
  | _, (Ast.E_MethodCall _ as m) ->
      let acc,lhs',after = refactor_lhs acc lhs in
      let acc = refactor_method_call_assign acc (Some lhs') m in
        acc_append acc after

  | e1,        (Ast.E_Identifier(Ast.ID_Lowercase,s, pos') as e)
      when not (StrSet.mem s acc.seen || is_special s) -> 
      refactor_assignment acc e1 (Ast.E_MethodCall(e,[],None,pos')) pos

  | _ -> 
      let acc,lhs',after = refactor_lhs acc lhs in
      let acc,rhs' = refactor_tuple_expr acc rhs in
      let acc = acc_enqueue (C.assign lhs' rhs' pos) acc in
        acc_append acc after

and refactor_stmt (acc: stmt acc) (e:Ast.expr) : stmt acc = match e with
  | Ast.E_Alias(Ast.E_Identifier(Ast.ID_Builtin|Ast.ID_Global as k1,s1, p1), 
                Ast.E_Identifier(Ast.ID_Builtin|Ast.ID_Global as k2,s2, p2) ,p3) ->
      let g1 = `ID_Var(refactor_builtin_or_global p1 k1,s1) in
      let g2 = `ID_Var(refactor_builtin_or_global p2 k2,s2) in
        acc_enqueue (C.alias_g ~link:g1 ~orig:g2 p3) acc

  | Ast.E_Alias(e1,e2, pos) -> 
      let acc,e1' = refactor_symbol_or_msg acc e1 in
      let acc,e2' = refactor_symbol_or_msg acc e2 in
        acc_enqueue (C.alias_m ~link:e1' ~orig:e2' pos) acc

  | Ast.E_Undef(e1, pos) -> 
      let acc,e1' = refactor_list refactor_symbol_or_msg (acc,DQueue.empty) e1 in
        acc_enqueue (C.undef (DQueue.to_list e1') pos) acc

  | Ast.E_Ternary(g,t,f, pos) ->
      let acc,g' = refactor_expr acc g in
      let tacc = refactor_stmt (acc_emptyq acc) t  in
      let ts = C.seq (DQueue.to_list tacc.q) pos in
        (* propagate seen from tacc to facc for below *)
      let facc = refactor_stmt (acc_emptyq tacc) f  in
      let fs = C.seq (DQueue.to_list facc.q) pos in
      let acc = acc_seen acc facc in
        acc_enqueue (mkstmt (If(g',ts,fs)) pos) acc

  | Ast.E_If(g,t,f,pos)
  | Ast.E_Unless(g,f,t, pos) ->
      let acc,g' = refactor_expr acc g in
      let tacc = refactor_stmt_list (acc_emptyq acc) t in
      let ts = C.seq (DQueue.to_list tacc.q) pos in
        (* propagate seen from tacc to facc for below *)
      let facc = refactor_stmt_list (acc_emptyq tacc) f in
      let fs = C.seq (DQueue.to_list facc.q) pos in
      let acc = {acc with seen = StrSet.union acc.seen facc.seen} in
        acc_enqueue (C.if_s g' ~t:ts ~f:fs pos) acc

  | Ast.E_Case(case,pos) -> refactor_case acc case pos

  | Ast.E_Return(args,pos) ->
      let acc,args' = refactor_list refactor_tuple_expr (acc,DQueue.empty) args in
      let tup = make_tuple_option (DQueue.to_list args') in
        acc_enqueue (C.return ?v:tup pos) acc

  | Ast.E_Yield(args,pos) ->
      let acc,args' = refactor_list refactor_method_arg_no_cb (acc,DQueue.empty) args in
        acc_enqueue (C.yield ~args:(DQueue.to_list args') pos) acc

  | Ast.E_Block(el,pos) -> 
      let blk_acc = refactor_stmt_list (acc_emptyq acc) el in
      let acc = {acc with seen = StrSet.union acc.seen blk_acc.seen} in
        acc_enqueue (C.seq (DQueue.to_list blk_acc.q) pos) acc

  | Ast.E_Binop(e1,Ast.Op_ASSIGN,(Ast.E_Yield(args,pos1)), pos2) ->
      let acc,e1',after = refactor_lhs acc e1 in
      let acc,args' = refactor_list refactor_method_arg_no_cb (acc,DQueue.empty) args in
      let yield_s = C.yield ~lhs:e1' ~args:(DQueue.to_list args') pos2 in
      let acc = acc_enqueue yield_s acc in
        acc_append acc after

  | Ast.E_Binop(e1,Ast.Op_ASSIGN,
                Ast.E_Binop(l,
                            ( Ast.Op_PLUS | Ast.Op_MINUS | Ast.Op_TIMES
                            | Ast.Op_REM  | Ast.Op_DIV   | Ast.Op_CMP
                            | Ast.Op_EQ   | Ast.Op_EQQ   (*| Ast.Op_NEQ*)
                            | Ast.Op_GEQ  | Ast.Op_LEQ   | Ast.Op_LT
                            | Ast.Op_GT   | Ast.Op_BAND  | Ast.Op_BOR
                            | Ast.Op_MATCH (*| Ast.Op_NMATCH*) | Ast.Op_XOR
                            | Ast.Op_POW  | Ast.Op_AREF  | Ast.Op_ASET
                            | Ast.Op_LSHIFT | Ast.Op_RSHIFT
                             as op),
                            r,pos1),
                pos) ->
      let acc,e1',after = refactor_lhs acc e1 in
      let acc = refactor_binop_into_mc acc (Some e1') l op r pos in
        acc_append acc after

  | Ast.E_Binop(lhs,Ast.Op_ASSIGN,rhs, pos) ->
      refactor_assignment acc lhs rhs pos
          
  (* A::m is really a method call *)
  | Ast.E_Binop(e1,Ast.Op_SCOPE,(Ast.E_Identifier(Ast.ID_Lowercase,_,_)),pos) -> 
      refactor_method_call_assign acc None (Ast.E_MethodCall(e,[], None, pos))

  | Ast.E_MethodCall _ as m ->
      refactor_method_call_assign acc None m

  (* special case for 'x &&= e' when this is the first assignment to x.
     In this case, x is always nil (the && can't succeed) and the rhs is
     dead code.
  *) 
  | Ast.E_Binop((Ast.E_Identifier(Ast.ID_Lowercase,s, pos'),
                 Ast.Op_OP_ASGN Ast.Op_AND, rhs, pos))
      when not (StrSet.mem s acc.seen || is_special s) ->
      let id' = `ID_Var(`Var_Local, s) in
      let acc = add_seen s acc in
        Log.err ~ctx:(Log.of_loc pos)
          "removing dead code: %a" Ast_printer.format_expr rhs;
        acc_enqueue (C.assign id' `ID_Nil pos) acc

  (* special case for 'x ||= e' when this is the first assignment to x.
     In this case, x is always set to e (the || can never fail) *) 
  | Ast.E_Binop((Ast.E_Identifier(Ast.ID_Lowercase,s, pos') as lhs,
                 Ast.Op_OP_ASGN Ast.Op_OR, rhs, pos))
      when not (StrSet.mem s acc.seen || is_special s) ->
      refactor_assignment acc lhs rhs pos

  | Ast.E_Binop(lhs, Ast.Op_OP_ASGN Ast.Op_AND, rhs, pos) ->
      let acc, lhs_id = refactor_id acc lhs in
      let asgn_acc = refactor_assignment (acc_emptyq acc) lhs rhs pos in
      let t_branch = C.seq (DQueue.to_list asgn_acc.q) pos in
      let f_branch = C.expr lhs_id pos in
      let s = C.if_s lhs_id ~t:t_branch ~f:f_branch pos in
        acc_enqueue s (acc_seen acc asgn_acc)

  | Ast.E_Binop(lhs, Ast.Op_OP_ASGN Ast.Op_OR, rhs, pos) ->
      let acc, lhs_id = refactor_id acc lhs in
      let asgn_acc = refactor_assignment (acc_emptyq acc) lhs rhs pos in
      let t_branch = C.expr lhs_id pos in
      let f_branch = C.seq (DQueue.to_list asgn_acc.q) pos in
      let s = C.if_s lhs_id ~t:t_branch ~f:f_branch pos in
        acc_enqueue s (acc_seen acc asgn_acc)

  | Ast.E_Binop(lhs, Ast.Op_OP_ASGN op, rhs, pos) ->
      refactor_assignment acc lhs (Ast.E_Binop(lhs,op,rhs,pos)) pos

  | Ast.E_Identifier(Ast.ID_Lowercase,s, pos) 
      when not (StrSet.mem s acc.seen || is_special s) -> 
      if s = "super" then refactor_super acc None pos
      else refactor_stmt acc (Ast.E_MethodCall(e,[],None,pos))
        
  | Ast.E_Identifier _
  | Ast.E_Literal _
  | Ast.E_Tuple _
  | Ast.E_Hash _
  | Ast.E_Array _
  | Ast.E_Unary _
  | Ast.E_Binop _ as e ->
      let acc,e' = refactor_expr acc e in
        acc_enqueue (C.expr e' (Ast.pos_of e)) acc

  | Ast.E_While(true,g,body, pos) -> (* do .. while *)
      let gpos = Ast.pos_of g in
      let body_acc = refactor_stmt_list (acc_emptyq acc) body in
      let body_acc,g' = refactor_expr body_acc g in
      let guard = C.if_s g' (C.next gpos) (C.break gpos) gpos in
      let body_acc = acc_enqueue guard body_acc in
      let body' = C.seq (DQueue.to_list body_acc.q) pos in
      let acc = acc_seen acc body_acc in
        acc_enqueue (C.while_s `ID_True body' pos) acc

  | Ast.E_While(false,g,body, pos) -> (* while .. do *)
      let gpos = Ast.pos_of g in
      let while_acc,g' = refactor_expr (acc_emptyq acc) g in
      let body_acc = refactor_stmt_list (acc_emptyq while_acc) body in
        if (DQueue.is_empty while_acc.q) then
          (* preserve guards that are already just expressions *)
          let while_body = C.seq (DQueue.to_list body_acc.q) pos in
            acc_enqueue (C.while_s g' while_body pos) (acc_seen acc body_acc)
        else
          let body' = C.seq (DQueue.to_list body_acc.q) pos in
          let guard = C.if_s g' ~t:body' ~f:(C.break gpos) gpos in
          let while_acc = acc_enqueue guard while_acc in
          let while_body = C.seq (DQueue.to_list while_acc.q) pos in
          let acc = acc_seen acc body_acc in
            acc_enqueue (C.while_s `ID_True while_body pos) acc

  | Ast.E_Until(b,g,body, pos) -> 
      refactor_stmt acc (Ast.E_While(b,Ast.E_Unary(Ast.Op_UNot,g,pos),body,pos))

  | Ast.E_For(formals,guard,body, pos) ->
      let acc, formals = refactor_block_formal_list acc formals pos in
      let acc, g_expr = refactor_expr acc guard in
      let body_acc = refactor_stmt_list (acc_emptyq acc) body in
      let body' = C.seq (DQueue.to_list body_acc.q) pos in
      let acc = acc_seen acc body_acc in
        acc_enqueue (C.for_s formals g_expr body' pos) acc

  | Ast.E_ModuleDef(m,body, pos) ->
      let acc,m' = refactor_id acc m in
      let body_acc = refactor_body (acc_empty acc) body pos in
      let body' = C.seq (DQueue.to_list body_acc.q) pos in
        acc_enqueue (C.module_s m' body' pos) acc

  | Ast.E_MethodDef(meth,params,body, pos) ->
      let acc,mn = refactor_method_name acc meth in
      let in_acc, params' = 
        refactor_method_formal_list (acc_empty acc) params pos 
      in
      let body_acc = {(acc_emptyq in_acc) with 
                        super_args=gen_super_args params'}
      in
      let body_acc = refactor_body body_acc body pos in
      let body_acc = 
        if DQueue.is_empty body_acc.q
        then acc_enqueue (C.expr `ID_Nil pos) body_acc
        (* add an extra nil to the end of the initialization block.
           Otherwise if the method has no body, the result would be the
           last default argument *)
        else body_acc
      in
      let body_acc = {q = DQueue.append in_acc.q body_acc.q;
                      seen = StrSet.union in_acc.seen body_acc.seen;
                      super_args=body_acc.super_args} 
      in 
      let add_return e = Return (Some e) in
      let body_acc = add_final_return add_return body_acc pos in
      let body' = C.seq (DQueue.to_list body_acc.q) pos in
        acc_enqueue (C.meth mn params' body' pos) acc

  | Ast.E_ClassDef(clazz,inh,body, pos) ->
      let body_acc = refactor_body (acc_empty acc) body pos in
        begin match inh with
          | None -> 
              let body' = C.seq (DQueue.to_list body_acc.q) pos in
              let acc,clazz' = refactor_id acc clazz in 
                acc_enqueue (C.nameclass clazz' body' pos) acc

          | Some (Ast.Class_Inherit e) -> 
              let body' = C.seq (DQueue.to_list body_acc.q) pos in
              let acc,clazz' = refactor_id acc clazz in
              let acc,e' = refactor_id acc e in
                acc_enqueue (C.nameclass clazz' ~inh:e' body' pos) acc

          | Some (Ast.Inst_Inherit e) -> 
              assert(clazz = Ast.E_Empty);
              let acc,e' = refactor_id acc e in
              let body_lst = DQueue.to_list body_acc.q in
              let body' = C.seq body_lst pos in
                acc_enqueue (C.metaclass e' body' pos) acc
        end

  | Ast.E_BeginBlock(lst,pos) ->
      let body_acc = refactor_stmt_list (acc_empty acc) lst in
      let body' = C.seq (DQueue.to_list body_acc.q) pos in
        acc_enqueue (mkstmt (Begin body') pos) acc

  | Ast.E_EndBlock(lst,pos) ->
      let body_acc = refactor_stmt_list (acc_empty acc) lst in
      let body' = C.seq (DQueue.to_list body_acc.q) pos in
        acc_enqueue (mkstmt (End body') pos) acc

  | Ast.E_ExnBlock(body,pos) -> refactor_body acc body pos

  | Ast.E_Empty -> acc

  | Ast.E_Annotate(e,(Annotation.ExprType te as annot),pos) ->
      let acc, ident = refactor_id acc e in
      let cast = C.expr ident pos in
      let cast = add_annotation cast annot in
        acc_enqueue cast acc

  | Ast.E_Annotate(expr,annot,pos) ->
      let acc = refactor_stmt acc expr in
      let rest,stmt = DQueue.pop_back acc.q in
      let stmt = add_annotation stmt annot in
        acc_enqueue stmt {acc with q=rest}

  | Ast.E_Operator _
  | Ast.E_UOperator _
  | Ast.E_CodeBlock _ as s -> 
      Log.fatal (Log.of_loc (Ast.pos_of s))
        "refactor_stmt: unknown stmt to refactor: %s\n"
        (Ast_printer.string_of_expr s)
      
and refactor_method_name (acc:stmt acc) e : stmt acc * def_name = match e with
  | Ast.E_Binop(targ,Ast.Op_SCOPE,msg, pos)
  | Ast.E_Binop(targ,Ast.Op_DOT,msg, pos) ->
      let acc,targ' = refactor_id acc targ in
      let acc,msg' = refactor_msg acc msg in
        acc, (Singleton_Method (targ',msg'))

  | Ast.E_Literal(Ast.Lit_Atom [Ast.StrChars s], pos) -> 
      acc, (Instance_Method (`ID_MethodName s))

  | Ast.E_Literal(Ast.Lit_Atom _, pos) -> 
      Log.fatal (Log.of_loc pos) "interpreted atom string in method name?"
  | e -> 
      let acc,id = refactor_msg acc e in
        acc, (Instance_Method id)

and refactor_body (acc:stmt acc) b pos : stmt acc = 
  if b.Ast.rescue_exprs = [] && b.Ast.ensure_expr = [] && b.Ast.else_expr = []
  then refactor_stmt_list acc b.Ast.body_exprs
  else begin
    (* thread the seen set through all parts of the body *)
    let body_acc = refactor_stmt_list (acc_emptyq acc) b.Ast.body_exprs in
    let rescue_acc,resc_rlist = 
      List.fold_left
        (fun (acc,lst) resc ->
           let acc, resc' = refactor_rescue pos acc resc in
             acc, resc'::lst
        ) ((acc_emptyq body_acc),[]) b.Ast.rescue_exprs 
    in
    let rescue_list = List.rev resc_rlist in
    let ensure_acc = 
      refactor_stmt_list (acc_emptyq rescue_acc) b.Ast.ensure_expr 
    in
    let else_acc = 
      refactor_stmt_list (acc_emptyq ensure_acc) b.Ast.else_expr 
    in
    let acc = acc_seen acc else_acc in
    let body = C.seq (DQueue.to_list body_acc.q) pos in
    let eelse = 
      if DQueue.is_empty else_acc.q then None 
      else Some (C.seq (DQueue.to_list else_acc.q) pos);
    in
    let ensure = 
      if DQueue.is_empty ensure_acc.q then None 
      else Some (C.seq (DQueue.to_list ensure_acc.q) pos);
    in
      acc_enqueue (C.exnblock body rescue_list ?eelse ?ensure pos) acc
  end

and refactor_rescue pos acc (gs,rescue_body) : stmt acc * rescue_block =
  let guard_exprs = match gs with
    | Ast.E_Tuple(l,_) -> l
    | Ast.E_Empty -> []
    | x -> [x]
  in
  let acc_just_set, rev_guards = 
    List.fold_left
      (fun (acc,gl) e ->
        let set, g = refactor_rescue_guard acc e in
          {acc with seen=set}, g::gl
      ) (acc_emptyq acc,[]) guard_exprs
  in
  let set = acc_just_set.seen in
  let guards = List.rev rev_guards in
  let body_acc = acc_seen {q=DQueue.empty; seen = set;
                           super_args=acc.super_args} acc in
  let body_acc = refactor_stmt body_acc rescue_body in
  let body = C.seq (DQueue.to_list body_acc.q) pos in
  let resc_blk = {rescue_guards = guards;rescue_body = body} in
    (acc_seen acc body_acc), resc_blk

and refactor_rescue_guard acc (e:Ast.expr) : StrSet.t * rescue_guard = 
  match e with
    | Ast.E_Binop(Ast.E_Empty,Ast.Op_ASSOC,bind_e, pos) -> 
        let obj = Ast.E_Identifier(Ast.ID_Uppercase,"StandardError", pos) in
          refactor_rescue_guard acc (Ast.E_Binop(obj,Ast.Op_ASSOC,bind_e, pos))

    | Ast.E_Binop(exn_e,Ast.Op_ASSOC,bind_e, pos) -> 
        let acc, exn = refactor_tuple_expr acc exn_e in
        let acc, bind_lhs,after = refactor_lhs acc bind_e in
        let () = 
          if not (DQueue.is_empty after.q)
          then Log.fatal (Log.of_loc pos) "methodcall in rescue binder??"
        in
        let bind = match bind_lhs with
          | #identifier as id -> id
          | _ -> Log.fatal (Log.of_loc pos) "non-identifier in rescue binder"
        in
          if not (DQueue.is_empty acc.q)
          then begin
            Log.fatal (Log.of_loc pos) "rescue binding created hoisted expression?"
          end;
          acc.seen, Rescue_Bind(exn,bind)

    | Ast.E_Identifier(Ast.ID_Uppercase,_,_)
    | Ast.E_Binop(_,Ast.Op_SCOPE,_,_)
    | Ast.E_Unary(Ast.Op_UScope,_,_) ->
        let acc, e' = refactor_tuple_expr acc e in
          if not (DQueue.is_empty acc.q)
          then begin
            DQueue.iter
              (fun s ->
                 Printf.eprintf "resc: %s\n" 
                   (CodePrinter.string_of_cfg s)
              ) acc.q;
            Log.fatal (Log.of_loc (Ast.pos_of e))
              "rescue1 guard created hoisted expression?"
          end;
          acc.seen, Rescue_Expr e'

    | e -> 
        Log.fixme ~ctx:(Log.of_loc (Ast.pos_of e))
          "rescue gaurd: %a" Ast_printer.format_expr e;
        let acc, e' = refactor_tuple_expr acc e in
          if (not (DQueue.is_empty acc.q)) || not (StrSet.is_empty acc.seen)
          then Log.fatal (Log.of_loc (Ast.pos_of e))
            "rescue guard created hoisted expression?";
          acc.seen, Rescue_Expr e'
            
and refactor_method_formal (acc:stmt acc) t pos : stmt acc * method_formal_param = 
  match t with
  | Ast.Formal_id Ast.E_Identifier(Ast.ID_Lowercase,str,pos) -> 
      let acc = {acc with seen = StrSet.add str acc.seen} in
        acc, `Formal_meth_id(str)

  | Ast.Formal_id _ -> 
      Log.fatal Log.empty "refactor_method_formal: non-local method formal?"

  | Ast.Formal_amp s ->
      {acc with seen = StrSet.add s acc.seen}, `Formal_amp s

  | Ast.Formal_star(str) -> 
      let acc = {acc with seen = StrSet.add str acc.seen} in
        acc, `Formal_star(str)

  | Ast.Formal_rest -> 
      let acc, id = fresh acc in
      let s = match id with
        | `ID_Var(`Var_Local,s) -> s
        | _ -> assert false
      in
        acc, `Formal_star(s)

  | Ast.Formal_tuple(f_lst) -> Log.fatal Log.empty "refactor_method_formal: formal_tuple?"

  | Ast.Formal_default (f,s) ->
      let pos = Ast.pos_of s in
      let default_acc = refactor_stmt (acc_emptyq acc) s in
      let acc = acc_seen acc default_acc in
      let acc = {acc with seen = StrSet.add f acc.seen} in
      let s' = C.seq (DQueue.to_list default_acc.q) pos
      in
        match s'.snode with
          | Expression e -> acc, `Formal_default(f, (e :> tuple_expr))
          | _ -> 
              let def = `Lit_Atom (sprintf "__rat_default_%d" (fresh_formal())) in
              let eql = `ID_MethodName "eql?" in
              let acc, v = fresh acc in
              let formal_id = `ID_Var(`Var_Local, f) in
              let acc = seen_lhs acc formal_id in
              let s'' = add_last_assign ~do_break:false formal_id s' in
              let blk = [
                C.mcall ~lhs:v ~targ:(C.local f) eql [def] pos;
                C.if_s v ~t:s'' ~f:(C.expr `ID_Nil pos) pos;
                ]
              in
              let pre = C.seq blk pos in
              let acc = acc_enqueue pre acc in
                acc, `Formal_default (f, def)

and refactor_block_formal acc t pos : stmt acc * block_formal_param = match t with
  | Ast.Formal_id Ast.E_Identifier(ik,str,pos) -> 
      (add_seen str acc), `Formal_block_id(refactor_id_kind pos ik,str)
  | Ast.Formal_id _ ->
      Log.fatal (Log.of_loc pos) "refactor_block_formal: non-identifier in formal id"

  | Ast.Formal_star(s) -> (add_seen s acc), `Formal_star(s)
  | Ast.Formal_rest -> 
      let acc, id = fresh acc in
      let s = match id with
        | `ID_Var(`Var_Local,s) -> s
        | _ -> assert false
      in
        acc, `Formal_star(s)

  | Ast.Formal_tuple(f_lst) ->
      let acc, lst = refactor_block_formal_list acc f_lst pos in
        acc, `Formal_tuple lst
  | Ast.Formal_amp s -> Log.fatal (Log.of_loc pos) "refactor_block_formal: & arg?"
  | Ast.Formal_default _ -> Log.fatal (Log.of_loc pos) "refactor_block_formal: default arg?"

and refactor_block_formal_list acc lst pos = 
  refactor_formal_list refactor_block_formal acc lst pos

and refactor_method_formal_list acc lst pos = 
  refactor_formal_list refactor_method_formal acc lst pos

and refactor_case acc case pos = 
  let acc,g' = match case.Ast.case_guard with
    | Ast.E_Empty -> acc, `ID_True
    | e -> refactor_expr acc e
  in
  let acc, whens' = 
    List.fold_left
      (fun (acc,whens) (g,body) -> 
         let acc, glist = 
           refactor_list refactor_tuple_expr (acc,DQueue.empty) g 
         in
         let g' = match DQueue.to_list glist with
           | [] -> assert false
           | [x] -> x
           | lst -> `Tuple lst
         in
         let body_acc = refactor_stmt_list (acc_emptyq acc) body in
         let body' = C.seq (DQueue.to_list body_acc.q) pos in
         let acc = acc_seen acc body_acc in
           acc, acc_enqueue (g',body') whens
      ) (acc,acc_emptyq acc) case.Ast.case_whens
  in
  let else' = refactor_stmt_list (acc_emptyq whens') case.Ast.case_else in
  let default = if DQueue.is_empty else'.q
  then None else Some (C.seq (DQueue.to_list else'.q) pos)
  in
  let case = C.case g' (DQueue.to_list whens'.q) ?default pos in
    acc_enqueue case (acc_seen acc else')
      
and refactor_stmt_list (acc:stmt acc) lst : stmt acc = 
  List.fold_left refactor_stmt acc lst
    

let refactor_ast ?env ast = 
  let seen = default_opt StrSet.empty env in
  let acc = {q = DQueue.empty; seen = seen;super_args=None} in
  let acc = refactor_stmt_list acc ast in
    match DQueue.to_list acc.q with
      | [] -> empty_stmt ()
      | (hd::_) as lst -> C.seq lst (pos_of hd)

let kreparse ?env ?filename ?lineno cont k =
  let module U = Cfg_printer.CodeUnparser in
  let cont str = 
    let ast = Parse_helper.parse_string ?env ?filename ?lineno str in
      cont (refactor_ast ast ?env)
  in
    U.ksformat cont k

let reparse ?env ?filename ?lineno k = 
  kreparse ?env ?filename ?lineno Utils.id k

let kfreparse ?env ?filename ?lineno cont = 
  Log.kfsprintf
    (fun str -> 
       let ast = Parse_helper.parse_string ?env ?filename ?lineno str in
         cont (refactor_ast ast ?env)
    ) 

let freparse ?env ?filename ?lineno = 
  kfreparse ?env ?filename ?lineno Utils.id
