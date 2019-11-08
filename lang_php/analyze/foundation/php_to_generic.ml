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

open Cst_php
open Ast_php
module G = Ast_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Ast_php to Ast_generic.
 *
 * See ast_generic.ml for more information.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let id = fun x -> x
let option = Common.map_opt
let list = List.map
let vref f x = ref (f !x)

let bool = id
let string = id
let int_to_string = string_of_int
let float_to_string = string_of_float

(*
exception Error of string * Parse_info.info

let error tok msg = 
  raise (Error (msg, tok))
*)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let info x = x

let wrap = fun _of_a (v1, v2) ->
  let v1 = _of_a v1 and v2 = info v2 in 
  (v1, v2)

let ident v = wrap string v

let var v = wrap string v

let qualified_ident v = list ident v

let name v = qualified_ident v

let rec fixOp x = x
and binaryOp x = raise Todo
and arithOp x = raise Todo
and logicalOp x = raise Todo
and assignOp x = raise Todo
and unaryOp x = raise Todo


let modifierbis =
  function
  | Public -> ()
  | Private -> ()
  | Protected -> ()
  | Static -> ()
  | Abstract -> ()
  | Final -> ()
  | Async -> ()

let ptype =
  function
  | BoolTy -> ()
  | IntTy -> ()
  | DoubleTy -> ()
  | StringTy -> ()
  | ArrayTy -> ()
  | ObjectTy -> ()

let rec stmt =
  function
  | Expr v1 -> let v1 = expr v1 in ()
  | Block v1 -> let v1 = list stmt v1 in ()
  | If ((v1, v2, v3)) ->
      let v1 = expr v1 and v2 = stmt v2 and v3 = stmt v3 in ()
  | Switch ((v1, v2)) -> let v1 = expr v1 and v2 = list case v2 in ()
  | While ((v1, v2)) -> let v1 = expr v1 and v2 = list stmt v2 in ()
  | Do ((v1, v2)) -> let v1 = list stmt v1 and v2 = expr v2 in ()
  | For ((v1, v2, v3, v4)) ->
      let v1 = list expr v1
      and v2 = list expr v2
      and v3 = list expr v3
      and v4 = list stmt v4
      in ()
  | Foreach ((v1, v2, v3)) ->
      let v1 = expr v1
      and v2 = foreach_pattern v2
      and v3 = list stmt v3
      in ()
  | Return v1 -> let v1 = option expr v1 in ()
  | Break v1 -> let v1 = option expr v1 in ()
  | Continue v1 -> let v1 = option expr v1 in ()
  | Throw v1 -> let v1 = expr v1 in ()
  | Try ((v1, v2, v3)) ->
      let v1 = list stmt v1
      and v2 = list catch v2
      and v3 = list finally v3
      in ()
  | ClassDef v1 -> let v1 = class_def v1 in ()
  | FuncDef v1 -> let v1 = func_def v1 in ()
  | ConstantDef v1 -> let v1 = constant_def v1 in ()
  | TypeDef v1 -> let v1 = type_def v1 in ()
  | NamespaceDef ((v1, v2)) ->
      let v1 = qualified_ident v1 and v2 = list stmt v2 in ()
  | NamespaceUse ((v1, v2)) ->
      let v1 = qualified_ident v1 and v2 = option ident v2 in ()
  | StaticVars v1 ->
      let v1 =
        list
          (fun (v1, v2) ->
             let v1 = var v1 and v2 = option expr v2 in ())
          v1
      in ()
  | Global v1 -> let v1 = list expr v1 in ()
and case =
  function
  | Case ((v1, v2)) -> let v1 = expr v1 and v2 = list stmt v2 in ()
  | Default v1 -> let v1 = list stmt v1 in ()
and catch (v1, v2, v3) =
  let v1 = hint_type v1 and v2 = var v2 and v3 = list stmt v3 in ()
and finally v = list stmt v
and expr =
  function
  | Int v1 -> let v1 = string v1 in ()
  | Double v1 -> let v1 = string v1 in ()
  | String v1 -> let v1 = wrap string v1 in ()
  | Id v1 -> let v1 = name v1 in ()
  | Var v1 -> let v1 = var v1 in ()
  | Array_get ((v1, v2)) ->
      let v1 = expr v1 and v2 = option expr v2 in ()
  | This v1 -> let v1 = wrap string v1 in ()
  | Obj_get ((v1, v2)) -> let v1 = expr v1 and v2 = expr v2 in ()
  | Class_get ((v1, v2)) -> let v1 = expr v1 and v2 = expr v2 in ()
  | New ((v1, v2)) -> let v1 = expr v1 and v2 = list expr v2 in ()
  | InstanceOf ((v1, v2)) -> let v1 = expr v1 and v2 = expr v2 in ()
  | Assign ((v1, v2, v3)) ->
      let v1 = option binaryOp v1
      and v2 = expr v2
      and v3 = expr v3
      in ()
  | List v1 -> let v1 = list expr v1 in ()
  | Arrow ((v1, v2)) -> let v1 = expr v1 and v2 = expr v2 in ()
  | Ref v1 -> let v1 = expr v1 in ()
  | Unpack v1 -> let v1 = expr v1 in ()
  | Call ((v1, v2)) -> let v1 = expr v1 and v2 = list expr v2 in ()
  | Infix ((v1, v2)) -> let v1 = fixOp v1 and v2 = expr v2 in ()
  | Postfix ((v1, v2)) ->
      let v1 = fixOp v1 and v2 = expr v2 in ()
  | Binop ((v1, v2, v3)) ->
      let v1 = binaryOp v1
      and v2 = expr v2
      and v3 = expr v3
      in ()
  | Unop ((v1, v2)) -> let v1 = unaryOp v1 and v2 = expr v2 in ()
  | Guil v1 -> let v1 = list expr v1 in ()
  | ConsArray v1 -> let v1 = list array_value v1 in ()
  | Collection ((v1, v2)) ->
      let v1 = name v1 and v2 = list array_value v2 in ()
  | Xhp v1 -> let v1 = xml v1 in ()
  | CondExpr ((v1, v2, v3)) ->
      let v1 = expr v1 and v2 = expr v2 and v3 = expr v3 in ()
  | Cast ((v1, v2)) -> let v1 = ptype v1 and v2 = expr v2 in ()
  | Lambda v1 -> let v1 = func_def v1 in ()
and xhp =
  function
  | XhpText v1 -> let v1 = string v1 in ()
  | XhpExpr v1 -> let v1 = expr v1 in ()
  | XhpXml v1 -> let v1 = xml v1 in ()
and
  xml { xml_tag = xml_tag; xml_attrs = xml_attrs; xml_body = xml_body
        } =
  let arg = ident xml_tag in
  let arg =
    list (fun (v1, v2) -> let v1 = ident v1 and v2 = xhp_attr v2 in ())
      xml_attrs in
  let arg = list xhp xml_body in ()
and xhp_attr v = expr v
and foreach_pattern v = expr v
and array_value v = expr v
and string_const_expr v = expr v
and hint_type =
  function
  | Hint v1 -> let v1 = name v1 in ()
  | HintArray -> ()
  | HintQuestion v1 -> let v1 = hint_type v1 in ()
  | HintTuple v1 -> let v1 = list hint_type v1 in ()
  | HintCallback ((v1, v2)) ->
      let v1 = list hint_type v1 and v2 = option hint_type v2 in ()
  | HintShape v1 ->
      let v1 =
        list
          (fun (v1, v2) ->
             let v1 = string_const_expr v1 and v2 = hint_type v2 in ())
          v1
      in ()
  | HintTypeConst v1 ->
      let v1 =
        (match v1 with
         | (v1, v2) -> let v1 = hint_type v1 and v2 = hint_type v2 in ())
      in ()
  | HintVariadic v1 -> let v1 = option hint_type v1 in ()
and class_name v = hint_type v
and
  func_def {
               f_name = f_name;
               f_kind = f_kind;
               f_params = f_params;
               f_return_type = f_return_type;
               f_ref = f_ref;
               m_modifiers = m_modifiers;
               l_uses = l_uses;
               f_attrs = f_attrs;
               f_body = f_body
             } =
  let arg = ident f_name in
  let arg = function_kind f_kind in
  let arg = list parameter f_params in
  let arg = option hint_type f_return_type in
  let arg = bool f_ref in
  let arg = list modifier m_modifiers in
  let arg =
    list (fun (v1, v2) -> let v1 = bool v1 and v2 = var v2 in ())
      l_uses in
  let arg = list attribute f_attrs in
  let arg = list stmt f_body in ()
and function_kind =
  function
  | Function -> ()
  | AnonLambda -> ()
  | ShortLambda -> ()
  | Method -> ()
and
  parameter {
                p_type = p_type;
                p_ref = p_ref;
                p_name = p_name;
                p_default = p_default;
                p_attrs = p_attrs;
                p_variadic = p_variadic
              } =
  let arg = option hint_type p_type in
  let arg = bool p_ref in
  let arg = var p_name in
  let arg = option expr p_default in
  let arg = list attribute p_attrs in
  let arg = bool p_variadic in ()
and modifier v = modifierbis v
and attribute v = expr v
and constant_def { cst_name = cst_name; cst_body = cst_body } =
  let arg = ident cst_name in let arg = option expr cst_body in ()
and enum_type { e_base = e_base; e_constraint = e_constraint } =
  let arg = hint_type e_base in
  let arg = option hint_type e_constraint in ()
and
  class_def {
                c_name = c_name;
                c_kind = c_kind;
                c_extends = c_extends;
                c_implements = c_implements;
                c_uses = c_uses;
                c_enum_type = c_enum_type;
                c_attrs = c_attrs;
                c_xhp_fields = c_xhp_fields;
                c_xhp_attr_inherit = c_xhp_attr_inherit;
                c_constants = c_constants;
                c_variables = c_variables;
                c_methods = c_methods
              } =
  let arg = ident c_name in
  let arg = class_kind c_kind in
  let arg = option class_name c_extends in
  let arg = list class_name c_implements in
  let arg = list class_name c_uses in
  let arg = option enum_type c_enum_type in
  let arg = list attribute c_attrs in
  let arg = list xhp_field c_xhp_fields in
  let arg = list class_name c_xhp_attr_inherit in
  let arg = list constant_def c_constants in
  let arg = list class_var c_variables in
  let arg = list method_def c_methods in ()
and class_kind =
  function
  | ClassRegular -> ()
  | ClassFinal -> ()
  | ClassAbstract -> ()
  | ClassAbstractFinal -> ()
  | Interface -> ()
  | Trait -> ()
  | Enum -> ()
and xhp_field (v1, v2) = let v1 = class_var v1 and v2 = bool v2 in ()
and
  class_var {
                cv_name = cname;
                cv_type = ctype;
                cv_value = cvalue;
                cv_modifiers = cmodifiers
              } =
  let arg = var cname in
  let arg = option hint_type ctype in
  let arg = option expr cvalue in
  let arg = list modifier cmodifiers in ()
and method_def v = func_def v
and type_def { t_name = t_name; t_kind = t_kind } =
  let arg = ident t_name in let arg = type_def_kind t_kind in ()
and type_def_kind =
  function
  | Alias v1 -> let v1 = hint_type v1 in ()
  | Newtype v1 -> let v1 = hint_type v1 in ()
  | ClassConstType v1 -> let v1 = option hint_type v1 in ()

and program v = 
  (* list stmt v  *)
  raise Todo


(*
let any =
  function
  | Program v1 -> let v1 = program v1 in ()
  | Stmt v1 -> let v1 = stmt v1 in ()
  | Expr2 v1 -> let v1 = expr v1 in ()
  | Param v1 -> let v1 = parameter v1 in ()
*)
