(* Yoann Padioleau
 *
 * Copyright (C) 2019 Yoann Padioleau
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Cst_js to Ast_js *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
(* not used for now *)
type _env = unit

let empty_env () = ()

exception TodoConstruct of string * Parse_info.info
(* The string is usually "advanced es6" or "Typescript" *)
exception UnhandledConstruct of string * Parse_info.info

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let opt f env x =
  match x with
  | None -> None
  | Some x -> Some (f env x)

let rec comma_list = function
  | [] -> []
  | Common.Left x  :: rl -> x :: comma_list rl
  | Common.Right _ :: rl -> comma_list rl

let paren (_, x, _) = x
let fst3 (x, _, _) = x

let noop = A.Block []
let noscope = None

exception Found of Parse_info.info

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

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let rec program xs =
  let env = empty_env () in
  module_items env xs

and module_items env xs =
  xs |> List.map (module_item env) |> List.flatten

and module_item env = function
  | C.It x -> item env x |> List.map (fun res -> 
      match res with
      | A.VarDecl var -> A.V var
      | _ -> 
         let tok = first_tok_of_item x in
         A.S (tok, res)
    )
  | C.Import (_, x, _) -> import env x
  | C.Export (tok, x) ->  export env tok x

and import env = function
  | C.ImportEffect ((_file, tok)) ->
     raise (UnhandledConstruct ("import effect", tok))
  | C.ImportFrom ((default_opt, names_opt) , (_, path)) ->
    (match default_opt with
    | Some n -> 
       [A.Import ((A.default_entity, snd n),  name env n, path)]
    | None -> []
    ) @
    (match names_opt with
    | None -> []
    | Some ni ->
      (match ni with
      | C.ImportNamespace (_, _, (_name, tok)) ->
        raise (UnhandledConstruct ("import namespace", tok))
      | C.ImportNames xs ->
        xs |> paren |> comma_list |> List.map (fun (n1, n2opt) ->
           let n1 = name env n1 in
           let n2 = 
              match n2opt with
              | None -> n1
              | Some (_, n2) -> name env n2
           in
           A.Import (n1, n2, path)
         )
       )
     )

and export env tok = function
 | C.ExportDefaultExpr (tok, e, _)  -> 
   let e = expr env e in
   let n = A.default_entity, tok in
   [A.Export (n, e)]
 | C.ExportDecl x ->
   let xs = item env x in
   xs |> List.map (function
    (* less: v_kind? *)
    | A.VarDecl v -> A.Export (v.A.v_name, v.A.v_init)
    | _ -> raise (UnhandledConstruct ("exporting a stmt", tok))
   )
 | C.ExportDefaultDecl (tok, x) ->
   let xs = item env x in
   xs |> List.map (function
    | A.VarDecl v -> 
        let n = A.default_entity, tok in (*v.A.v_name*)
        (* less: v_kind? *)
        A.Export (n, v.A.v_init)
    | _ -> raise (UnhandledConstruct ("exporting a stmt", tok))
   )
 | C.ExportNames (xs, _) ->
   xs |> paren |> comma_list |> List.map (fun (n1, n2opt) ->
     let n1 = name env n1 in
     let n2 = 
       match n2opt with
       | None -> n1
       | Some (_, n2) -> name env n2
     in
     A.Export (n2, A.Id (n1, noscope))
  )
 | C.ReExportNamespace (_, _, _) ->
   raise (UnhandledConstruct ("reexporting namespace", tok))
 | C.ReExportNames (_, _, _) ->
   raise (UnhandledConstruct ("reexporting names", tok))

and item env = function
  | C.St x -> stmt env x
  | C.FunDecl x -> 
    let fun_ = func_decl env x in
    (match x.C.f_name with
    | Some x ->
      [A.VarDecl {A.v_name = name env x; v_kind = A.Const; 
                  v_init = A.Fun (fun_, None)}]
    | None ->
       raise (UnhandledConstruct ("weird anon func decl", fst3 x.C.f_params))
    )
  | C.ClassDecl x -> 
    let class_ = class_decl env x in
    (match x.C.c_name with
    | Some x ->
      [A.VarDecl {A.v_name = name env x; v_kind=A.Const;v_init=A.Class class_}]
    | None ->
       raise (UnhandledConstruct ("weird anon class decl", x.C.c_tok))
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
  | C.PN_Computed x -> A.PN_Computed (x |> paren |> expr env)

(* ------------------------------------------------------------------------- *)
(* Statement *)
(* ------------------------------------------------------------------------- *)

and stmt env = function
  | C.VarsDecl ((vkind,_), bindings, _) ->
    bindings |> comma_list |> List.map (fun x -> 
     A.VarDecl (var_binding env vkind x))
  | C.Block x -> 
    [A.Block (x |> paren |> List.map (item env) |> List.flatten)]
  | C.Nop _ -> 
    []
  | C.ExprStmt (e, _) ->
    [A.ExprStmt (expr env e)]
  | C.If (_, e, then_, elseopt) ->
    let e = e |> paren |> expr env in
    let then_ = stmt1 env then_ in
    let else_ = 
      match elseopt with
      | None -> noop
      | Some (_, st) -> stmt1 env st
    in
    [A.If (e, then_, else_)]
  | C.Do (_, st, _, e, _) ->
     let st = stmt1 env st in
     let e = e |> paren |> expr env in
     [A.Do (st, e)]
  | C.While (_, e, st) ->
     let e = e |> paren |> expr env in
     let st = stmt1 env st in
     [A.While (e, st)]
  | C.For (_, _, lhs_vars_opt, _, e2opt, _, e3opt, _, st) ->
     let e1 = 
       match lhs_vars_opt with
       | Some (C.LHS1 e) -> Right (expr env e)
       | Some (C.ForVars (((vkind, _), vbindings))) ->
         Left (vbindings |> comma_list |> List.map (fun x -> 
             (var_binding env vkind x)))
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
      | C.ForVar ((vkind,_), binding) -> Left (var_binding env vkind binding)
    in 
    let e2 = expr env e2 in
    let st = stmt1 env st in
    [A.For (A.ForIn (e1, e2), st)]
  | C.ForOf (_, _, lhs_var, _, e2, _, st) ->
    let e1 =
      match lhs_var with
      | C.LHS2 e -> Right (expr env e)
      | C.ForVar ((vkind,_), binding) -> Left (var_binding env vkind binding)
    in 
    let e2 = expr env e2 in
    let st = stmt1 env st in
    [A.For (A.ForOf (e1, e2), st)]
  | C.Switch (_, e, xs) ->
    let e = e |> paren |> expr env in
    let xs = xs |> paren |> List.map (case_clause env) in
    [A.Switch (e, xs)]
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
       let arg = name env (paren arg) in
       let st = stmt1 env st in
       (arg, st)
       ) env catchopt in
    let finally_opt = opt (fun env (_, st) -> stmt1 env st) env finally_opt in
    [A.Try (st, catchopt, finally_opt)]

and stmt1 env st =
  match stmt env st with
  | [] -> A.Block []
  | [x] -> x
  | xs -> A.Block xs

and case_clause env = function
  | C.Default (_, _, xs) -> A.Default (stmt1_item_list env xs)
  | C.Case (_, e, _, xs) ->
    let e = expr env e in
    A.Case (e, stmt1_item_list env xs)

and stmt1_item_list env items =
  match items |> List.map (item env) |> List.flatten with
  | [] -> A.Block []
  | [x] -> x
  | xs -> A.Block xs
  
(* ------------------------------------------------------------------------- *)
(* Expression *)
(* ------------------------------------------------------------------------- *)
and expr env = function
  | C.L x -> literal env x
  | C.V (s, tok) -> 
     (match s with
     | "eval" -> A.IdSpecial (A.Eval, tok)
     | "undefined" -> A.IdSpecial (A.Undefined, tok)
     (* todo? require? import? *)
     | _ -> A.Id ((s, tok), noscope)
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
    let e = expr env e in
    A.ObjAccess (e, A.PN_Computed (expr env (paren e2)))
  | C.Object xs ->
    A.Obj (xs |> paren |> comma_list |> List.map (property env))
  | C.Array (tok, xs, _) ->
    A.Obj (array_obj env 0 tok xs)
  | C.Apply (e, es) ->
    let e = expr env e in
    let es = List.map (expr env) (es |> paren |> comma_list) in
    A.Apply (e, es)
  | C.Conditional (e1, _, e2, _, e3) ->
    let e1 = expr env e1 in
    let e2 = expr env e2 in
    let e3 = expr env e3 in
    A.Conditional (e1, e2, e3)
  | C.Assign (e1, (op, tok), e2) ->
    let e1 = expr env e1 in
    let e2 = expr env e2 in
    (match op with
    | C.A_eq -> A.Assign (e1, e2)
    (* todo: should use intermediate? can unsugar like this? *)
    | C.A_add -> A.Assign (e1, A.Apply(A.IdSpecial (A.Plus, tok), [e1;e2]))
    | C.A_sub -> A.Assign (e1, A.Apply(A.IdSpecial (A.Minus, tok), [e1;e2]))
    | C.A_mul -> A.Assign (e1, A.Apply(A.IdSpecial (A.Mul, tok), [e1;e2]))
    | C.A_div -> A.Assign (e1, A.Apply(A.IdSpecial (A.Div, tok), [e1;e2]))
    | C.A_mod -> A.Assign (e1, A.Apply(A.IdSpecial (A.Mod, tok), [e1;e2]))
    | C.A_lsl -> A.Assign (e1, A.Apply(A.IdSpecial (A.Lsl, tok), [e1;e2]))
    | C.A_lsr -> A.Assign (e1, A.Apply(A.IdSpecial (A.Lsr, tok), [e1;e2]))
    | C.A_asr -> A.Assign (e1, A.Apply(A.IdSpecial (A.Asr, tok), [e1;e2]))
    | C.A_and -> A.Assign (e1, A.Apply(A.IdSpecial (A.And, tok), [e1;e2]))
    | C.A_or  -> A.Assign (e1, A.Apply(A.IdSpecial (A.Or, tok), [e1;e2]))
    | C.A_xor -> A.Assign (e1, A.Apply(A.IdSpecial (A.Xor, tok), [e1;e2]))
    )
  | C.Seq (e1, tok, e2) ->
    let e1 = expr env e1 in
    let e2 = expr env e2 in
    A.Apply (A.IdSpecial (A.Seq, tok), [e1;e2])
  | C.Function x ->
    let fun_ = func_decl env x in
    (match x.C.f_name with
    | None -> A.Fun (fun_, None)
    | Some n -> A.Fun (fun_, Some (name env n))
    )
  | C.Class x ->
    let class_ = class_decl env x in
    (match x.C.c_name with
    | None -> A.Class class_
    | Some _ ->
       raise (UnhandledConstruct ("weird named class expr", x.C.c_tok))
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

  | C.XhpHtml x ->
    let tok = 
      match x with
      | C.Xhp ((_, tok), _, _, _, _) -> tok
      | C.XhpSingleton ((_, tok), _, _) -> tok
    in
    raise (UnhandledConstruct ("xhp", tok))
  | C.Paren x ->
     expr env (paren x)

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
  | C.U_pre_increment -> A.Incr (true)  | C.U_pre_decrement -> A.Decr (true)
  | C.U_post_increment -> A.Incr (false) | C.U_post_decrement -> A.Decr (false)
  | C.U_plus -> A.Plus | C.U_minus -> A.Minus
  | C.U_not -> A.Not | C.U_bitnot -> A.BitNot | C.U_spread -> A.Spread

and binop _env (op,tok) e1 e2 = 
  let res = 
    match op with
    | C.B_instanceof -> Left A.Instanceof
    | C.B_in -> Left A.In
    | C.B_add -> Left A.Plus | C.B_sub -> Left A.Minus
    | C.B_mul -> Left A.Mul | C.B_div -> Left A.Div | C.B_mod -> Left A.Mod
    | C.B_expo -> Left A.Expo
    | C.B_lt -> Left A.Lower | C.B_gt -> Left A.Greater
    | C.B_lsr -> Left A.Lsr | C.B_asr -> Left A.Asr | C.B_lsl -> Left A.Lsl
    | C.B_bitand -> Left A.BitAnd | C.B_bitor -> Left A.BitOr
    | C.B_bitxor -> Left A.BitXor
    | C.B_and -> Left A.And | C.B_or -> Left A.Or
    | C.B_equal -> Left A.Equal | C.B_physequal -> Left A.PhysEqual

    (* todo: e1 and e2 can have side effect, need intermediate var *)
    | C.B_le -> Right (
      A.Apply (A.IdSpecial (A.Or, tok), [
          A.Apply (A.IdSpecial (A.Lower, tok), [e1;e2]);
          A.Apply (A.IdSpecial (A.Equal, tok), [e1;e2]);
        ]))
    | C.B_ge -> Right (
      A.Apply (A.IdSpecial (A.Or, tok), [
          A.Apply (A.IdSpecial (A.Greater, tok), [e1;e2]);
          A.Apply (A.IdSpecial (A.Equal, tok), [e1;e2]);
        ]))

    | C.B_notequal -> Right (
      A.Apply (A.IdSpecial (A.Not, tok), [
        A.Apply (A.IdSpecial (A.Equal, tok), [e1;e2]);]))
    | C.B_physnotequal -> Right (
      A.Apply (A.IdSpecial (A.Not, tok), [
        A.Apply (A.IdSpecial (A.PhysEqual, tok), [e1;e2]);]))
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
  | C.VarClassic x -> variable_declaration env vkind x
  | C.VarPatternTodo -> raise Todo

and variable_declaration env vkind x =
  let n = name env x.C.v_name in
  let init = 
    match x.C.v_init with
    (* less Undefined? *)
    | None -> A.Nop
    | Some (_, e) -> expr env e
  in
  let vkind = var_kind env vkind in
  { A.v_name = n; v_init = init; v_kind = vkind }

and var_kind _env = function
  | C.Var -> A.Var
  | C.Const -> A.Const
  | C.Let -> A.Let



and func_decl env x =
  let props = func_kind env x.C.f_kind in
  let params = 
   x.C.f_params |> paren |> comma_list |> List.map (parameter_binding env) in
  let body = stmt1_item_list env (x.C.f_body |> paren) in
  { A.f_props = props; f_params = params; f_body = body }

and func_kind _env = function
 | C.Regular -> []
 | C.Get _ -> [A.Get]
 | C.Set _ -> [A.Set]
 | C.Generator _ -> [A.Generator]
 | C.Async _ -> [A.Async]

and parameter_binding env = function
 | C.ParamClassic p -> parameter env p
 | C.ParamPatternTodo -> raise Todo

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
    | C.AParams xs -> xs |> paren |> comma_list
  in
  let params = bindings |> List.map (parameter_binding env) in
  let body = 
    match x.C.a_body with
    | C.AExpr e -> A.ExprStmt (expr env e)
    | C.ABody xs -> stmt1_item_list env (xs |> paren)
  in
  { A.f_props = props; f_params = params; f_body = body }


and property env = function
 | C.P_field (pname, _, e) ->
   let pname = property_name env pname in
   let e = expr env e in
   let props = [] in
   A.Field (pname, props, e)
 | C.P_method x ->
    let fun_ = func_decl env x in
   (* todo: could be a property_name already, should not use f_name *)
    (match x.C.f_name with
    | Some x ->
      let pname = A.PN (name env x) in
      A.Field (pname, [], A.Fun (fun_, None))
    | None ->
       raise (UnhandledConstruct ("weird anon method decl", fst3 x.C.f_params))
    )
  | C.P_shorthand n ->
    let n = name env n in
    A.Field (A.PN n, [], A.Id (n, noscope))
  | C.P_spread (_, e) ->
    let e = expr env e in
    A.FieldSpread e

and array_obj env idx tok xs =
  match xs with
  | [] -> []
  | x::xs -> 
    (match x with
    | Right tok -> array_obj env (idx+1) tok xs
    | Left e ->
      let n = A.PN (string_of_int idx, tok) in
      let e = expr env e in
      let elt = A.Field (n, [], e) in
      elt::array_obj env idx tok xs
    )

and class_decl env x =
  let extends = opt (fun env (_, typ) -> nominal_type env typ) env 
    x.C.c_extends in
  let xs = x.C.c_body |> paren |> List.map (class_element env) |> 
    List.flatten in
  { A.c_extends = extends; c_body = xs }

and nominal_type env (e, _) = expr env e

and class_element env = function
  | C.Field (n, _, _) -> 
    let n = name env n in
    [A.Field (A.PN n, [], A.Nop)]
  | C.Method (static_opt, x) ->
    let fun_ = func_decl env x in
    let props = 
      match static_opt with
      | None -> []
      | Some _ -> [A.Static]
    in
   (* todo: could be a property_name already, should not use f_name *)
    (match x.C.f_name with
    | Some x ->
      let pname = A.PN (name env x) in
      [A.Field (pname, props, A.Fun (fun_, None))]
    | None ->
       raise (UnhandledConstruct ("weird anon method decl", fst3 x.C.f_params))
    )
  | C.ClassExtraSemiColon _ -> []
  | C.ClassTodo -> raise Todo 

(* ------------------------------------------------------------------------- *)
(* Misc *)
(* ------------------------------------------------------------------------- *)
