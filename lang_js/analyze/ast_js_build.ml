(* Yoann Padioleau
 *
 * Copyright (C) 2019 r2c
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open Common

module A = Ast_js
module C = Cst_js
module G = Ast_generic (* for the operators *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Cst_js to Ast_js.
 *
 * See also transpile_js.ml for important helper functions.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
(* used only to tag Id to avoid repetition in the code highlighter 
 * less: factorize code with graph_code_js?
 *)
type env = {
  (* I handle block scope by not using
   * a ref of mutable here! Just build a new list and passed it down.
   *)
  locals: (string * Ast_js.resolved_name (* Local or Param *)) list;
  (* 'var's have a function scope.
   * alt: lift Var up in a ast_js_build.ml transforming phase
   *)
  vars: (string, bool) Hashtbl.t;
}

let empty_env () = {
  locals = [];
  vars = Hashtbl.create 0;
}

exception TodoConstruct of string * Parse_info.t
(* The string is usually "advanced es6" or "Typescript" *)
exception UnhandledConstruct of string * Parse_info.t

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let opt f env x =
  match x with
  | None -> None
  | Some x -> Some (f env x)

let fst3 (x, _, _) = x

let noop = A.Block []
let not_resolved () = ref A.NotResolved

exception Found of Parse_info.t

let first_tok_of_item x =
  let hooks = { Visitor_js.default_visitor with
    Visitor_js.kinfo = (fun (_k, _) i -> raise (Found i));
  } in
  begin
    let vout = Visitor_js.mk_visitor hooks in
    try 
      vout (C.Item x);
      failwith "first_to_of_item: could not find a token";
    with Found tok -> tok
  end

let s_of_n n = 
  Ast_js.str_of_name n

(*
let is_local env n =
  let s = s_of_n n in
  List.mem_assoc s env.locals || Hashtbl.mem env.vars s
*)

(* copy-paste of Graph_code_js.add_locals mostly *)
let add_locals env vs = 
  let locals = vs |> Common.map_filter (fun v ->
    let s = s_of_n v.A.v_name in
    match v.A.v_kind with
    | A.Let | A.Const -> Some (s, A.Local)
    | A.Var ->
        Hashtbl.replace env.vars s true;
        None
     ) in
  { env with locals = locals @ env.locals } 

let add_params env ps = 
  let params = ps |> List.map (fun p ->
    let s = s_of_n p.A.p_name in
    s, A.Param
  ) in
  { env with locals = params @ env.locals } 

(* we would like to remove leading ./ and possibly add 
 * some node_modules/xxx/index.js but we can not do that here.
 * See graph_code_js and module_path_js for filename resolving.
 *)
let path_to_file (path, tok) =
  path, tok

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let rec program xs =
  let env = empty_env () in
  module_items env xs

and module_items env xs =
  xs |> List.map (module_item env) |> List.flatten

and module_item env = function
  | C.It x -> item None env x |> List.map (fun res -> 
      match res with
      | A.VarDecl var -> A.V var
      | _ -> 
         let tok = first_tok_of_item x in
         A.S (tok, res)
    )
  | C.Import (_, x, _) -> import env x |> List.map (fun x -> A.M x)
  | C.Export (tok, x) ->  export env tok x

and import env = function
  | C.ImportEffect ((file, tok)) ->
     if file =~ ".*\\.css$"
     then [A.ImportCss (file, tok)]
     else [A.ImportEffect (file, tok)]
  | C.ImportFrom ((default_opt, names_opt), (_, path)) ->
    let file = path_to_file path in
    (match default_opt with
    | Some n -> 
       [A.Import ((A.default_entity, snd n),  name env n, file)]
    | None -> []
    ) @
    (match names_opt with
    | None -> []
    | Some ni ->
      (match ni with
      | C.ImportNamespace (_star, _as, n1) ->
        let n1 = name env n1 in
        [A.ModuleAlias (n1, file)]
      | C.ImportNames xs ->
        xs |> C.unparen |> C.uncomma |> List.map (fun (n1, n2opt) ->
           let n1 = name env n1 in
           let n2 = 
              match n2opt with
              | None -> n1
              | Some (_, n2) -> name env n2
           in
           A.Import (n1, n2, file)
         )
      | C.ImportTypes (_tok, _xs) ->
         (* ignore for now *)
         []
       )
     )

and export env tok = function
 | C.ExportDefaultExpr (tok, e, _)  -> 
   let e = expr env e in
   let n = A.default_entity, tok in
   let v = {A.v_name = n; v_kind = A.Const; v_init = e; 
            v_resolved = not_resolved () } in
   [A.V v; A.M (A.Export (n))]
 | C.ExportDecl x ->
   let xs = item None env x in
   xs |> List.map (function
    | A.VarDecl v -> 
         [A.V v; A.M (A.Export (v.A.v_name))]
    | _ -> raise (UnhandledConstruct ("exporting a stmt", tok))
   ) |> List.flatten
 | C.ExportDefaultDecl (tok, x) ->
   (* this is ok to have anonymous entities here *)
   let xs = item (Some tok) env x in
   xs |> List.map (function
    | A.VarDecl v -> 
        [A.V v;  A.M (A.Export (v.A.v_name))]
    | _ -> raise (UnhandledConstruct ("exporting a stmt", tok))
   ) |> List.flatten
 | C.ExportNames (xs, _) ->
   xs |> C.unparen |> C.uncomma |> List.map (fun (n1, n2opt) ->
     let n1 = name env n1 in
     match n2opt with
     | None -> [A.M (A.Export (n1))]
     | Some (_, n2) -> 
         let n2 = name env n2 in
         let id = A.Id (n1, not_resolved ()) in
         let v = { A.v_name = n2; v_kind = A.Const; v_init = id;
                   v_resolved = not_resolved () } in
         [A.V v; A.M (A.Export n2)]
  ) |> List.flatten
 | C.ReExportNames (xs, (_from, path), _) ->
   xs |> C.unbrace |> C.uncomma |> List.map (fun (n1, n2opt) ->
     let n1 = name env n1 in
     let tmpname = ("!tmp_" ^ fst n1, snd n1) in
     let file = path_to_file path in
     let import = A.Import (n1, tmpname, file) in
     let id = A.Id (tmpname, not_resolved()) in
     match n2opt with
     | None -> 
       let v = { A.v_name = n1; v_kind = A.Const; v_init = id; 
                  v_resolved = not_resolved () } in
       [A.M import; A.V v; A.M (A.Export n1)]
     | Some (_, n2) ->
       let n2 = name env n2 in
       let v = { A.v_name = n2; v_kind = A.Const; v_init = id; 
                  v_resolved = not_resolved () } in
       [A.M import; A.V v; A.M (A.Export n2)]
   ) |> List.flatten

 | C.ReExportNamespace (_, _, _) ->
   raise (UnhandledConstruct ("reexporting namespace", tok))


and item default_opt env = function
  | C.St x -> stmt env x
  | C.FunDecl x -> 
    let fun_ = func_decl env x in
    (match x.C.f_kind, default_opt with
    | C.F_func (_, Some x), None ->
      let n = name env x in
      [A.VarDecl {A.v_name = n; v_kind = A.Const; 
                  v_init = A.Fun (fun_, None); v_resolved = not_resolved()}]

    | C.F_func (_, None), Some tok ->
      let n = A.default_entity, tok in 
      [A.VarDecl {A.v_name = n; v_kind = A.Const; 
                  v_init = A.Fun (fun_, None); v_resolved = not_resolved()}]
    | C.F_func (_, Some x), Some tok ->
      let n1 = A.default_entity, tok in 
      let n2 = name env x in
      [A.VarDecl {A.v_name = n1; v_kind = A.Const; 
                  v_init = A.Fun (fun_, Some n2); v_resolved = not_resolved()}]

    | C.F_func (_, None), None ->
       raise (UnhandledConstruct ("weird: anonymous func decl", 
                fst3 x.C.f_params))
    | _ ->
       raise (UnhandledConstruct ("weird func decl", fst3 x.C.f_params))
    )
  | C.ClassDecl x -> 
    let class_ = class_decl env x in
    (match x.C.c_name, default_opt with
    | Some x, None ->
      let n = name env x in
      [A.VarDecl {A.v_name = n; v_kind=A.Const;
                  v_init=A.Class (class_, None); v_resolved = not_resolved ()}]
    | None, Some tok ->
      let n = A.default_entity, tok in 
      [A.VarDecl {A.v_name = n; v_kind=A.Const;
                  v_init=A.Class (class_, None); v_resolved = not_resolved ()}]
    | Some x, Some tok ->
      let n1 = A.default_entity, tok in 
      let n2 = name env x in
      [A.VarDecl {A.v_name = n1; v_kind=A.Const;
                  v_init=A.Class (class_, Some n2); 
                  v_resolved = not_resolved ()}]
    | None, None ->
       raise (UnhandledConstruct ("weird: anonymous class decl", x.C.c_tok))
    )
  | C.InterfaceDecl x -> 
    raise (UnhandledConstruct ("Typescript", x.C.i_tok))
  | C.ItemTodo tok ->
    raise (TodoConstruct ("ItemTodo", tok))

(* ------------------------------------------------------------------------- *)
(* Names *)
(* ------------------------------------------------------------------------- *)
and name _env x = x
and label _env x = x

and property_name env = function
  | C.PN_Id x       -> A.PN (name env x)
  | C.PN_String x   -> A.PN (name env x)
  | C.PN_Num x      -> A.PN (name env x)
  | C.PN_Computed x -> A.PN_Computed (x |> C.unparen |> expr env)

(* ------------------------------------------------------------------------- *)
(* Statement *)
(* ------------------------------------------------------------------------- *)

and stmt env = function
  | C.VarsDecl ((vkind,_), bindings, _) ->
    bindings |> C.uncomma |> List.map (fun x -> 
     let vars = var_binding env vkind x in
     vars |> List.map (fun var -> A.VarDecl var)) |> List.flatten
  | C.Block x -> 
    [stmt1_item_list env (C.unparen x)]
  | C.Nop _ -> 
    []
  | C.ExprStmt (e, _) ->
    let  e = expr env e in
    (match e with
    | A.String("use strict", tok) -> 
      [A.ExprStmt (A.Apply(A.IdSpecial (A.UseStrict, tok), []))]
    | _ -> [A.ExprStmt e]
    )
  | C.If (_, e, then_, elseopt) ->
    let e = e |> C.unparen |> expr env in
    let then_ = stmt1 env then_ in
    let else_ = 
      match elseopt with
      | None -> noop
      | Some (_, st) -> stmt1 env st
    in
    [A.If (e, then_, else_)]
  | C.Do (_, st, _, e, _) ->
     let st = stmt1 env st in
     let e = e |> C.unparen |> expr env in
     [A.Do (st, e)]
  | C.While (_, e, st) ->
     let e = e |> C.unparen |> expr env in
     let st = stmt1 env st in
     [A.While (e, st)]
  | C.For (_, _, lhs_vars_opt, _, e2opt, _, e3opt, _, st) ->
     let e1 = 
       match lhs_vars_opt with
       | Some (C.LHS1 e) -> Right (expr env e)
       | Some (C.ForVars (((vkind, _), vbindings))) ->
         Left (vbindings |> C.uncomma |> List.map (fun x -> 
             (var_binding env vkind x)) |> List.flatten)
       | None -> Right (A.Nop)
     in
     let e2 = expr_opt env e2opt in
     let e3 = expr_opt env e3opt in
     let st = stmt1 env st in
     [A.For (A.ForClassic (e1, e2, e3), st)]
  | C.ForIn (_, _, lhs_var, _, e2, _, st) ->
    let e1 =
      match lhs_var with
      | C.LHS2 e -> Right (expr env e)
      | C.ForVar ((vkind,tok), binding) -> 
        let vars = var_binding env vkind binding in
        (match vars with
        | [var] -> Left var
        | _ -> raise (TodoConstruct ("For in with (pattern) vars?", tok))
        )
    in 
    let e2 = expr env e2 in
    let st = stmt1 env st in
    [A.For (A.ForIn (e1, e2), st)]
  | C.ForOf (_, _, lhs_var, tokof, e2, _, st) ->
    (try 
      Transpile_js.forof (lhs_var, tokof, e2, st) 
        (expr env, stmt env, var_binding env)
     with Failure s ->
       raise (TodoConstruct(spf "ForOf:%s" s, tokof))
    )
  | C.Switch (tok, e, xs) ->
    let e = e |> C.unparen |> expr env in
    let xs = xs |> C.unparen |> List.map (case_clause env) in
    [A.Switch (tok, e, xs)]
  | C.Continue (_, lopt, _) -> 
    [A.Continue (opt label env lopt)]
  | C.Break (_, lopt, _) -> 
    [A.Break (opt label env lopt)]
  | C.Return (_, eopt, _) -> 
    [A.Return (expr_opt env eopt)]
  | C.With (tok, _e, _st) ->
    raise (TodoConstruct ("with", tok))
  | C.Labeled (lbl, _, st) ->
    let lbl = label env lbl in
    let st = stmt1 env st in
    [A.Label (lbl, st)]
  | C.Throw (_, e, _) ->
    let e = expr env e in
    [A.Throw e]
  | C.Try (_, st, catchopt, finally_opt) ->
    let st = stmt1 env st in
    let catchopt = opt (fun env (_, arg, st) ->
       let arg = name env (C.unparen arg) in
       let st = stmt1 env st in
       (arg, st)
       ) env catchopt in
    let finally_opt = opt (fun env (_, st) -> stmt1 env st) env finally_opt in
    [A.Try (st, catchopt, finally_opt)]

and stmt_of_stmts xs = 
  match xs with
  | [] -> A.Block []
  | [x] -> x
  | xs -> A.Block xs

and stmt1 env st =
  stmt env st |> stmt_of_stmts

and case_clause env = function
  | C.Default (_, _, xs) -> A.Default (stmt1_item_list env xs)
  | C.Case (_, e, _, xs) ->
    let e = expr env e in
    A.Case (e, stmt1_item_list env xs)

and stmt_item_list env items =
 let rec aux acc env = function
    | [] -> List.rev acc |> List.flatten
    | x::xs ->
      let ys = item None env x in
      let env = 
         let locals = ys |> Common.map_filter (fun x ->
           match x with
           | A.VarDecl v -> Some v
           | _ -> None
         ) in
         add_locals env locals
      in
      aux (ys::acc) env xs
 in
 aux [] env items

and stmt1_item_list env items = 
  stmt_item_list env items |> stmt_of_stmts
  
(* ------------------------------------------------------------------------- *)
(* Expression *)
(* ------------------------------------------------------------------------- *)
and expr env = function
  | C.L x -> literal env x
  | C.V (s, tok) -> 
      let resolved = 
        try
          (List.assoc s env.locals)
        with Not_found ->
         if Hashtbl.mem env.vars s
         then A.Local
         else A.NotResolved
      in
      (match resolved with
      | A.Local | A.Param -> 
         A.Id ((s, tok), ref resolved)
      | A.NotResolved | A.Global _ ->
        (match s with
        | "eval" -> A.IdSpecial (A.Eval, tok)
        | "undefined" -> A.IdSpecial (A.Undefined, tok)
        (* commonJS *)
        | "require"   -> A.IdSpecial (A.Require, tok)
        | "exports"   -> A.IdSpecial (A.Exports, tok)
        | "module"   -> A.IdSpecial (A.Module, tok)
        (* AMD *)
        | "define"   -> A.IdSpecial (A.Define, tok)
        (* reflection *)
        | "arguments"   -> A.IdSpecial (A.Arguments, tok)
        | _ -> A.Id ((s, tok), ref resolved)
        )
      )
      
  | C.This tok -> A.IdSpecial (A.This, tok)
  | C.Super tok -> A.IdSpecial (A.Super, tok)
 
  | C.U ((op, tok), e) ->
    let special = unop env op in
    let e = expr env e in
    A.Apply (A.IdSpecial (special, tok), [e])
  | C.B (e1, op, e2) ->
    let e1 = expr env e1 in
    let e2 = expr env e2 in
    binop env op e1 e2
  | C.Period (e, _, n) ->
    let e = expr env e in
    A.ObjAccess (e, A.PN (name env n))
  | C.Bracket (e, e2) ->
    (* let e = expr env e in
    A.ObjAccess (e, A.PN_Computed (expr env (paren e2)))
    *)
    let e = expr env e in
    let e2 = expr env (C.unparen e2) in
    A.ArrAccess (e, e2)
  | C.Object xs ->
    A.Obj (xs |> C.unparen |> C.uncomma |> List.map (property env))
  | C.Array (tok, xs, _) ->
    (* A.Obj (array_obj env 0 tok xs) *)
    A.Arr (array_arr env tok xs)
  | C.Apply (e, es) ->
    let e = expr env e in
    let es = List.map (expr env) (es |> C.unparen |> C.uncomma) in
    A.Apply (e, es)
  | C.Conditional (e1, _, e2, _, e3) ->
    let e1 = expr env e1 in
    let e2 = expr env e2 in
    let e3 = expr env e3 in
    A.Conditional (e1, e2, e3)
  | C.Assign (e1, (op, tok), e2) ->
    let e1 = expr env e1 in
    let e2 = expr env e2 in
    let special op = A.IdSpecial (A.ArithOp op, tok) in
    (match op with
    | C.A_eq -> A.Assign (e1, tok, e2)
    (* less: should use intermediate? can unsugar like this? *)
    | C.A_add -> A.Assign (e1, tok, A.Apply(special G.Plus, [e1;e2]))
    | C.A_sub -> A.Assign (e1, tok, A.Apply(special G.Minus, [e1;e2]))
    | C.A_mul -> A.Assign (e1, tok, A.Apply(special G.Mult, [e1;e2]))
    | C.A_div -> A.Assign (e1, tok, A.Apply(special G.Div, [e1;e2]))
    | C.A_mod -> A.Assign (e1, tok, A.Apply(special G.Mod, [e1;e2]))
    | C.A_lsl -> A.Assign (e1, tok, A.Apply(special G.LSL, [e1;e2]))
    | C.A_lsr -> A.Assign (e1, tok, A.Apply(special G.LSR, [e1;e2]))
    | C.A_asr -> A.Assign (e1, tok, A.Apply(special G.ASR, [e1;e2]))
    | C.A_and -> A.Assign (e1, tok, A.Apply(special G.BitAnd, [e1;e2]))
    | C.A_or  -> A.Assign (e1, tok, A.Apply(special G.BitOr, [e1;e2]))
    | C.A_xor -> A.Assign (e1, tok, A.Apply(special G.BitXor, [e1;e2]))
    )
  | C.Seq (e1, tok, e2) ->
    let e1 = expr env e1 in
    let e2 = expr env e2 in
    A.Apply (A.IdSpecial (A.Seq, tok), [e1;e2])
  | C.Function x ->
    let fun_ = func_decl env x in
    (match x.C.f_kind with
    | C.F_func (_, None) -> A.Fun (fun_, None)
    | C.F_func (_, Some n) -> A.Fun (fun_, Some (name env n))
    | _ -> raise (UnhandledConstruct ("weird lambda", fst3 x.C.f_params))
    )
  | C.Class x ->
    let class_ = class_decl env x in
    (match x.C.c_name with
    | None -> A.Class (class_, None)
    | Some n -> A.Class (class_, Some (name env n))
    )
  | C.Arrow x -> A.Fun (arrow_func env x, None)
  | C.Yield (tok, star, eopt) ->
    let special = 
       if star = None
       then A.Yield
       else A.YieldStar 
    in
    let e = expr_opt env eopt in
    A.Apply (A.IdSpecial (special, tok), [e])
  | C.Await (tok, e) ->
    let e = expr env e in
    A.Apply (A.IdSpecial (A.Await, tok), [e])
  | C.NewTarget (tok, _, _) ->
    A.Apply (A.IdSpecial (A.NewTarget, tok), [])

  | C.Encaps (name_opt, tok, xs, _) ->
    let special = A.Encaps name_opt in
    let xs = List.map (encaps env) xs in
    A.Apply (A.IdSpecial (special, tok), xs)

  | C.XhpHtml x -> Transpile_js.xhp (expr env) x
  | C.Paren x ->
     expr env (C.unparen x)
  | C.Ellipses x ->
     A.Ellipses x

and expr_opt env = function
  | None -> A.Nop
  | Some e -> expr env e

and literal _env = function
  | C.Bool x -> A.Bool x
  | C.Num x -> A.Num x
  | C.String x -> A.String x
  | C.Regexp x -> A.Regexp x
  | C.Null tok -> A.IdSpecial (A.Null, tok)

and unop _env = function
  | C.U_new -> A.New  | C.U_delete -> A.Delete
  | C.U_typeof -> A.Typeof
  | C.U_void  -> A.Void
  | C.U_pre_increment -> A.IncrDecr (G.Incr, G.Prefix)  
  | C.U_pre_decrement -> A.IncrDecr (G.Decr, G.Prefix)
  | C.U_post_increment -> A.IncrDecr (G.Incr, G.Postfix) 
  | C.U_post_decrement -> A.IncrDecr (G.Decr, G.Postfix)
  | C.U_plus -> A.ArithOp G.Plus 
  | C.U_minus -> A.ArithOp G.Minus
  | C.U_not -> A.ArithOp G.Not 
  | C.U_bitnot -> A.ArithOp G.BitNot 
  | C.U_spread -> A.Spread

and binop _env (op,tok) e1 e2 = 
  let res = 
    match op with
    | C.B_instanceof -> Left A.Instanceof
    | C.B_in -> Left A.In

    | C.B_add -> Left (A.ArithOp G.Plus) 
    | C.B_sub -> Left (A.ArithOp G.Minus)
    | C.B_mul -> Left (A.ArithOp G.Mult) 
    | C.B_div -> Left (A.ArithOp G.Div) 
    | C.B_mod -> Left (A.ArithOp G.Mod)
    | C.B_expo -> Left (A.ArithOp G.Pow)
    | C.B_lt -> Left (A.ArithOp G.Lt) 
    | C.B_gt -> Left (A.ArithOp G.Gt)
    | C.B_lsr -> Left (A.ArithOp G.LSR) 
    | C.B_asr -> Left (A.ArithOp G.ASR) 
    | C.B_lsl -> Left (A.ArithOp G.LSL)
    | C.B_bitand -> Left (A.ArithOp G.BitAnd) 
    | C.B_bitor -> Left (A.ArithOp G.BitOr)
    | C.B_bitxor -> Left (A.ArithOp G.BitXor)
    | C.B_and -> Left (A.ArithOp G.And) 
    | C.B_or -> Left (A.ArithOp G.Or)
    | C.B_equal -> Left (A.ArithOp G.Eq) 
    | C.B_physequal -> Left (A.ArithOp G.PhysEq)
    | C.B_le -> Left (A.ArithOp G.LtE)
    | C.B_ge -> Left (A.ArithOp G.GtE)
    | C.B_notequal -> Left (A.ArithOp G.NotEq)
    | C.B_physnotequal -> Left (A.ArithOp G.NotPhysEq)
  in
  match res with
  | Left special ->A.Apply (A.IdSpecial (special, tok), [e1; e2])
  | Right x -> x

and encaps env = function
  | C.EncapsString x -> A.String x
  | C.EncapsExpr (_, e, _) -> expr env e

(* ------------------------------------------------------------------------- *)
(* Entities *)
(* ------------------------------------------------------------------------- *)
and var_binding env vkind = function
  | C.VarClassic x -> [variable_declaration env vkind x]
  | C.VarPattern x -> 
    (try Transpile_js.var_pattern (expr env, name env, property_name env) x
     with Failure s ->
       raise (TodoConstruct(spf "VarPattern:%s" s, 
        (C.Pattern x.C.vpat) |> Lib_parsing_js.ii_of_any |> List.hd))
     )

and variable_declaration env vkind x =
  let n = name env x.C.v_name in
  let init = init_opt env x.C.v_init in 
  let vkind = var_kind env vkind in
  { A.v_name = n; v_init = init; v_kind = vkind; v_resolved = not_resolved ()}

and init_opt env ini = 
  match ini with
  (* less Undefined? *)
  | None -> A.Nop
  | Some (_, e) -> expr env e

and var_kind _env = function
  | C.Var -> A.Var
  | C.Const -> A.Const
  | C.Let -> A.Let



and func_decl env x =
  (* bugfix: each function has its own vars *)
  let env = { env with vars = Hashtbl.copy env.vars } in
  let props = func_props env x.C.f_kind x.C.f_properties in
  let params_and_vars = 
   x.C.f_params |> C.unparen |> C.uncomma |> Common.index_list_0 |>
    List.map (fun (p, idx) -> parameter_binding env idx p)
  in
  let params = params_and_vars |> List.map fst in
  let vars = params_and_vars |> List.map snd |> List.flatten in
  let env = add_params env params in
  let xs = stmt_item_list env (x.C.f_body |> C.unparen) in
  let body = stmt_of_stmts (vars @ xs) in
  { A.f_props = props; f_params = params; f_body = body }

and func_props _env kind props = 
  (match kind with
  | C.F_func _ -> []
  | C.F_method _ -> []
  | C.F_get _ -> [A.Get]
  | C.F_set _ -> [A.Set]
  ) @
  (props |> List.map (function
   | C.Generator _ -> A.Generator
   | C.Async _ -> A.Async
   ))

and parameter_binding env idx = function
 | C.ParamClassic p -> parameter env p, []
 | C.ParamPattern x -> 
     let tok = (C.Pattern x.C.ppat) |> Lib_parsing_js.ii_of_any |> List.hd in
     let intermediate = spf "!arg%d!" idx, tok in
     let pat = x.C.ppat in
     (try 
       let vars = 
         Transpile_js.compile_pattern (expr env, name env, property_name env) 
            intermediate pat in
        let p = { C.p_name = intermediate;
                  C.p_type = x.C.ppat_type;
                  C.p_dots = None;
                  C.p_default = 
                   match x.C.ppat_default with
                   | None -> None;
                   | Some (tok, e) -> Some (C.DSome (tok, e))
                 } 
         in
         let p = parameter env p in
         p, vars |> List.map (fun x -> A.VarDecl x)
     with Failure s ->
       raise (TodoConstruct(spf "ParamPattern:%s" s, tok))
     )

and parameter env p =
  let name = name env p.C.p_name in
  let d = opt default env p.C.p_default in
  let dots = p.C.p_dots <> None in
  { A.p_name = name; p_default = d; p_dots = dots }

and default env = function
  (* less: use Undefined? *)
  | C.DNone _ -> A.Nop
  | C.DSome (_, e) -> expr env e

and arrow_func env x =
  (* todo: they can have some too, but not in CST for now *)
  let props = [] in
  let bindings = 
    match x.C.a_params with
    | C.ASingleParam x -> [x]
    | C.AParams xs -> xs |> C.unparen |> C.uncomma
  in
  let params_and_vars = 
        bindings |> Common.index_list_0 
        |> List.map (fun (p, idx) -> parameter_binding env idx p) in
  let params = params_and_vars |> List.map fst in
  let vars = params_and_vars |> List.map snd |> List.flatten in
  let env = add_params env params in
  let xs = 
    match x.C.a_body with
    (* Javascript has implicit returns for arrows like that *)
    | C.AExpr e -> [A.Return (expr env e)]
    | C.ABody xs -> stmt_item_list env (xs |> C.unparen)
  in
  let body = stmt_of_stmts (vars @ xs) in
  { A.f_props = props; f_params = params; f_body = body }


and property env = function
 | C.P_field (pname, _, e) ->
   let pname = property_name env pname in
   let e = expr env e in
   let props = [] in
   A.Field (pname, props, e)
 | C.P_method x ->
    method_ env [] x
  | C.P_shorthand n ->
    let n = name env n in
    A.Field (A.PN n, [], A.Id (n, not_resolved ()))
  | C.P_spread (_, e) ->
    let e = expr env e in
    A.FieldSpread e

and _array_obj env idx tok xs =
  match xs with
  | [] -> []
  | x::xs -> 
    (match x with
    | Right tok -> _array_obj env (idx+1) tok xs
    | Left e ->
      let n = A.PN (string_of_int idx, tok) in
      let e = expr env e in
      let elt = A.Field (n, [], e) in
      elt::_array_obj env idx tok xs
    )

and array_arr env tok xs =
  match xs with
  | [] -> []
  | [Right _] -> []
  | [Left e] -> [expr env e]
  | (Left e)::(Right tok)::xs -> 
     let e = expr env e in
     e::array_arr env tok xs
  | (Right _)::xs ->
    let e = A.Nop in
    e::array_arr env tok xs
  | (Left _)::(Left _)::_ ->
    raise (TodoConstruct ("array_arr, 2 left? impossible?", tok))

and class_decl env x =
  let extends = opt (fun env (_, typ) -> nominal_type env typ) env 
    x.C.c_extends in
  let xs = x.C.c_body |> C.unparen |> List.map (class_element env) |> 
    List.flatten in
  { A.c_extends = extends; c_body = xs }

and nominal_type env (e, _) = expr env e

and class_element env = function
  | C.C_field (fld, _) -> 
    let pn = property_name env fld.C.fld_name in
    let props = [] in (* TODO fld.fld_static *)
    let e = init_opt env fld.C.fld_init in
    [A.Field (pn, props, e)]
  | C.C_method (static_opt, x) ->
    let props = 
      match static_opt with
      | None -> []
      | Some _ -> [A.Static]
    in
    [method_ env props x]
  | C.C_extrasemicolon _ -> []

and method_ env props x =
  let fun_ = func_decl env x in
  let pname, fprops = 
    match x.C.f_kind with
    | C.F_method pn ->
      property_name env pn, []
    | C.F_get (_, pn) ->
      property_name env pn, [A.Get]
    | C.F_set (_, pn) ->
      property_name env pn, [A.Set]
    | C.F_func _ ->
      raise (UnhandledConstruct ("weird method decl: unexpected F_func", 
                                  fst3 x.C.f_params))
  in
  let fun_ = { fun_ with A.f_props = fprops @ fun_.A.f_props } in
  A.Field (pname, props, A.Fun (fun_, None))

(* ------------------------------------------------------------------------- *)
(* Misc *)
(* ------------------------------------------------------------------------- *)

let any x =
  let env = empty_env () in
  match x with
  | C.Expr x -> A.Expr (expr env x)
  | C.Stmt x -> A.Stmt (stmt1 env x)
  | C.Stmts x -> A.Stmts (List.map (stmt1 env) x)
  | C.Pattern _x -> raise Todo
  | C.Item _x -> raise Todo
  | C.Program _x -> raise Todo
