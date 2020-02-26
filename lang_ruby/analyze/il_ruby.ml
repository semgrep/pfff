type pos = Lexing.position

type unary_op =
    Op_UMinus
  | Op_UPlus
  | Op_UTilde

type binary_op =
  | Op_Plus
  | Op_Minus
  | Op_Times
  | Op_Rem
  | Op_Div
  | Op_CMP
  | Op_EQ
  | Op_EQQ
  | Op_GEQ
  | Op_LEQ
  | Op_LT
  | Op_GT
  | Op_BAnd
  | Op_BOr
  | Op_Match
  | Op_XOR
  | Op_Pow
  | Op_ARef
  | Op_ASet
  | Op_LShift
  | Op_RShift

let uniq_counter = ref 0
let uniq () = incr uniq_counter; !uniq_counter

type var_kind = [
    `Var_Local
  | `Var_Instance
  | `Var_Class
  | `Var_Global
  | `Var_Constant
  | `Var_Builtin
]

type identifier = [
  | `ID_Var of var_kind * string
  | `ID_Self
  | `ID_Scope of identifier * string
  | `ID_UScope of string
  | `ID_Nil
  | `ID_True
  | `ID_False
]

type msg_id = [
  | `ID_UOperator of unary_op
  | `ID_Operator of binary_op
  | `ID_MethodName of string
  | `ID_Assign of string
  | `ID_Super
]

(* convenience alias that is a subtype of identifier *)
type builtin_or_global = [`ID_Var of [`Var_Builtin|`Var_Global] * string]

type alias_kind =
  | Alias_Method of msg_id * msg_id
  | Alias_Global of builtin_or_global * builtin_or_global

type ('expr,'star_expr) literal_ = [
    `Lit_FixNum of int
  | `Lit_BigNum of Big_int.big_int
  | `Lit_Float of string * float
  | `Lit_String of string
  | `Lit_Atom of string
  | `Lit_Regexp of string * string
  | `Lit_Array of 'star_expr list
  | `Lit_Hash of ('expr * 'expr) list
  | `Lit_Range of bool * 'expr * 'expr
]

type 'a star = [ `Star of 'a]
type 'a tuple = [`Tuple of 'a list]

type 'a expr_ = [ identifier | ('a expr_,'a) literal_ ]

(* a star_expr is either an expr or a (`Star of expr), i.e., no
   nested Star's are allowed *)
type star_expr = [ star_expr expr_ | (star_expr expr_) star]

type expr = star_expr expr_

type literal = (expr,star_expr) literal_

type tuple_expr = [
  | tuple_expr tuple
  | expr
  | [tuple_expr tuple | expr] star  (* again, no nested stars *)
]

(* lhs is like a tuple expression, but no literals are allowed *)
type lhs = [
  | identifier
  | lhs tuple
  | identifier star
 ]

type def_name = 
  | Instance_Method of msg_id
  | Singleton_Method of identifier * msg_id

type rescue_guard = 
  | Rescue_Expr of tuple_expr
  | Rescue_Bind of tuple_expr * identifier


type class_kind = 
  | MetaClass of identifier
  | NominalClass of identifier * identifier option
      

and method_formal_param = [
    `Formal_meth_id of string
  | `Formal_amp of string
  | `Formal_star of string
  | `Formal_default of string * tuple_expr
]

and block_formal_param = [
    `Formal_block_id of var_kind * string
  | `Formal_star of string
  | `Formal_tuple of block_formal_param list
]
      
type any_formal = [block_formal_param|method_formal_param]

  type stmt = {
    snode : stmt_node;
    pos : pos;
    sid : int;
    mutable lexical_locals : Utils_ruby.StrSet.t;
    mutable preds : stmt Set_.t;
    mutable succs : stmt Set_.t;
  }

  and stmt_node = 
  | Seq of stmt list
  | Alias of alias_kind
  | If of expr * stmt * stmt
  | Case of case_block
  | While of expr * stmt
  | For of block_formal_param list * expr * stmt 
  | MethodCall of lhs option * method_call
  | Assign of lhs * tuple_expr
  | Expression of expr
  | Return of tuple_expr option
  | Yield of lhs option * star_expr list
  | Module  of lhs option * identifier * stmt
  | Method of def_name * method_formal_param list * stmt
  | Class of lhs option * class_kind * stmt
  | ExnBlock of exn_block
  | Begin of stmt 
  | End of stmt 
  | Defined of identifier * stmt
  | Undef of msg_id list
  | Break of tuple_expr option
  | Next of tuple_expr option
  | Redo
  | Retry

  and exn_block = {
    exn_body : stmt;
    exn_rescue : rescue_block list;
    exn_else : stmt option;
    exn_ensure : stmt option;
  }
      
  and rescue_block = {
    rescue_guards : rescue_guard list;
    rescue_body : stmt;
  }
      
  and case_block = {
    case_guard : expr;
    case_whens: (tuple_expr * stmt) list;
    case_else: stmt option;
  }
      
  and method_call = {
    mc_target : expr option;
    mc_msg : msg_id;
    mc_args : star_expr list;
    mc_cb : codeblock option;
  }
      
  and codeblock = 
    | CB_Arg of expr
    | CB_Block of block_formal_param list * stmt



  type t = stmt
  let compare t1 t2 = compare t1.sid t2.sid

  let mkstmt snode pos = 
    {snode = snode;
     pos = pos;
     lexical_locals = Utils_ruby.StrSet.empty;
     preds=Set_.empty;
     succs=Set_.empty;
     sid = uniq()}

  let update_stmt stmt snode = 
    {snode = snode;
     pos = stmt.pos;
     lexical_locals = stmt.lexical_locals;
     preds=Set_.empty;
     succs=Set_.empty;
     sid = uniq()}

  let update_locals stmt locals = stmt.lexical_locals <- locals

  let rec fold_stmt f acc stmt = match stmt.snode with
    | If(_g,ts,fs) -> fold_stmt f (fold_stmt f (f acc stmt) ts) fs

    | Seq(sl) -> List.fold_left (fold_stmt f) (f acc stmt) sl

    | For(_,_,s)
    | Module(_,_,s)
    | Method(_,_,s)
    | Class(_,_,s)
    | Begin(s)
    | End(s) -> fold_stmt f (f acc stmt) s

    | While(_g,body) ->
        fold_stmt f (f acc stmt)  body

    | Case(c) ->
        let acc = f acc stmt in
        let acc = List.fold_left
	  (fun acc (_w,b) ->
	     fold_stmt f acc b 
	  ) acc c.case_whens
        in
	  Utils_ruby.do_opt ~none:acc ~some:(fold_stmt f acc) c.case_else

    | MethodCall(_,{mc_cb = (None|Some (CB_Arg _)); _}) -> f acc stmt
    | MethodCall(_,{mc_cb = Some (CB_Block(_,cb_body)); _}) -> 
        fold_stmt f (f acc stmt) cb_body
	  
    | ExnBlock(b) ->
        let acc = f acc stmt in
        let acc = fold_stmt f acc b.exn_body in
        let acc = List.fold_left
	  (fun acc rb -> 
	     fold_stmt f acc rb.rescue_body
	  ) acc b.exn_rescue 
        in
        let acc = Utils_ruby.do_opt ~none:acc ~some:(fold_stmt f acc) b.exn_ensure in
	  Utils_ruby.do_opt ~none:acc ~some:(fold_stmt f acc) b.exn_else

    | Alias _ | Assign _ | Expression _ | Return _ | Yield _
    | Defined _ | Undef _ | Break _ | Redo | Retry | Next _ 
        -> f acc stmt

  let rec compute_cfg_succ stmt (succs:stmt Set_.t) = match stmt.snode with
    | Seq [] -> stmt.succs <- succs;
    | Seq ((hd::_) as l) -> 
        stmt.succs <- Set_.add hd stmt.succs;
        compute_cfg_succ_list succs l

    | MethodCall _ (* handle CB *)
    | Assign _
    | Expression _
    | Defined _
    | Alias _ -> stmt.succs <- succs

    | Case cb -> 
        List.iter
	  (fun (_guard,body) ->
	     (*
	       stmt.succs <- StmtSet.add guard stmt.succs;
	       compute_cfg_succ guard (StmtSet.singleton body);
	     *)
	     compute_cfg_succ body succs;
	  ) cb.case_whens;
        begin match cb.case_else with
	  | None -> ()
	  | Some else' -> 
	      stmt.succs <- Set_.add else' stmt.succs;
	      compute_cfg_succ else' succs
        end

    | ExnBlock eb -> 
        stmt.succs <- Set_.add eb.exn_body stmt.succs;
        let succs' =  match eb.exn_ensure, eb.exn_else with
	  | None, None -> succs
	  | Some x, None
	  | None, Some x -> 
	      compute_cfg_succ x succs;
	      Set_.add x succs
	  | Some x1, Some x2 ->
	      compute_cfg_succ x1 succs;
	      compute_cfg_succ x2 succs;
	      Set_.add x1 (Set_.add x2 succs)
        in
        let succs' = 
	  List.fold_left
	    (fun acc resc ->
	       compute_cfg_succ resc.rescue_body succs;
	       Set_.add resc.rescue_body acc
	    ) succs' eb.exn_rescue
        in
	  compute_cfg_succ eb.exn_body succs'
	    
    | If(_g,t,f) -> 
        stmt.succs <- Set_.add t (Set_.add f stmt.succs);
        compute_cfg_succ t succs;
        compute_cfg_succ f succs

    | While(_g,body) ->
        stmt.succs <- Set_.add body stmt.succs;
        body.succs <- Set_.add stmt body.succs;
        compute_cfg_succ body succs

    | For(_params,_guard,body) ->
        stmt.succs <- Set_.union (Set_.add body succs) stmt.succs;
        body.succs <- Set_.union succs body.succs;
        compute_cfg_succ body succs

    | Return _ -> stmt.succs <- Set_.empty
    | Yield _ -> stmt.succs <- Set_.union succs stmt.succs

    | Module(_,_,body)
    | Class(_,_,body) -> 
        stmt.succs <- Set_.add body stmt.succs;
        compute_cfg_succ body succs

    | Method(_,_,body) ->
        stmt.succs <- succs;
        compute_cfg_succ body Set_.empty
	  
    | Undef _ | Break _ | Redo | Retry | Next _ -> 
        failwith "handle control op in successor computation"

    (* These can't actually appear in a method *)
    | Begin(body)
    | End(body) -> 
        stmt.succs <- Set_.add body stmt.succs;
        compute_cfg_succ body succs
	  
  and compute_cfg_succ_list last = function
    | [] -> assert false
    | hd::[] -> compute_cfg_succ hd last
    | h1::((h2::_) as tl) -> 
        compute_cfg_succ h1 (Set_.singleton h2);
        compute_cfg_succ_list last tl

  let preds_from_succs stmt = 
    fold_stmt 
      (fun () stmt ->
         Set_.iter
	   (fun succ ->
	      succ.preds <- Set_.add stmt succ.preds
	   ) stmt.succs
      ) () stmt
      
  let compute_cfg stmt = 
    let () = fold_stmt 
      (fun () s -> 
         s.preds <- Set_.empty; 
         s.succs <- Set_.empty
      ) () stmt 
    in
      compute_cfg_succ stmt Set_.empty;
      preds_from_succs stmt
