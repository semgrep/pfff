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

module A = Ast_c
module G = Ast_generic

open Ast_cpp
open Ast_c

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Ast_c to Ast_generic.
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
let either f g x = raise Todo

let string = id
let bool = id
let int = id
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
let tok v = info v

let wrap = fun _of_a (v1, v2) ->
  let v1 = _of_a v1 and v2 = info v2 in 
  (v1, v2)

let name v = wrap string v


let rec unaryOp =
  function
  | GetRef -> ()
  | DeRef -> ()
  | UnPlus -> ()
  | UnMinus -> ()
  | Tilde -> ()
  | Not -> ()
  | GetRefLabel -> ()
and assignOp =
  function | SimpleAssign -> () | OpAssign v1 -> let v1 = arithOp v1 in ()
and fixOp = function | Dec -> () | Inc -> ()
and binaryOp =
  function
  | Arith v1 -> let v1 = arithOp v1 in ()
  | Logical v1 -> let v1 = logicalOp v1 in ()
and arithOp =
  function
  | Plus -> ()
  | Minus -> ()
  | Mul -> ()
  | Div -> ()
  | Mod -> ()
  | DecLeft -> ()
  | DecRight -> ()
  | And -> ()
  | Or -> ()
  | Xor -> ()
and logicalOp =
  function
  | Inf -> ()
  | Sup -> ()
  | InfEq -> ()
  | SupEq -> ()
  | Eq -> ()
  | NotEq -> ()
  | AndLog -> ()
  | OrLog -> ()


let rec type_ =
  function
  | TBase v1 -> let v1 = name v1 in ()
  | TPointer v1 -> let v1 = type_ v1 in ()
  | TArray ((v1, v2)) ->
      let v1 = option const_expr v1 and v2 = type_ v2 in ()
  | TFunction v1 -> let v1 = function_type v1 in ()
  | TStructName ((v1, v2)) ->
      let v1 = struct_kind v1 and v2 = name v2 in ()
  | TEnumName v1 -> let v1 = name v1 in ()
  | TTypeName v1 -> let v1 = name v1 in ()

and function_type (v1, v2) =
  let v1 = type_ v1 and v2 = list parameter v2 in ()
and parameter { p_type = p_type; p_name = p_name } =
  let arg = type_ p_type in let arg = option name p_name in ()
and struct_kind = function | Struct -> () | Union -> ()
and expr =
  function
  | Int v1 -> let v1 = wrap string v1 in ()
  | Float v1 -> let v1 = wrap string v1 in ()
  | String v1 -> let v1 = wrap string v1 in ()
  | Char v1 -> let v1 = wrap string v1 in ()
  | Id v1 -> let v1 = name v1 in ()
  | Call ((v1, v2)) -> let v1 = expr v1 and v2 = list argument v2 in ()
  | Assign ((v1, v2, v3)) ->
      let v1 = wrap assignOp v1
      and v2 = expr v2
      and v3 = expr v3
      in ()
  | ArrayAccess ((v1, v2)) -> let v1 = expr v1 and v2 = expr v2 in ()
  | RecordPtAccess ((v1, v2)) -> let v1 = expr v1 and v2 = name v2 in ()
  | Cast ((v1, v2)) -> let v1 = type_ v1 and v2 = expr v2 in ()
  | Postfix ((v1, v2)) ->
      let v1 = expr v1 and v2 = wrap fixOp v2 in ()
  | Infix ((v1, v2)) ->
      let v1 = expr v1 and v2 = wrap fixOp v2 in ()
  | Unary ((v1, v2)) ->
      let v1 = expr v1 and v2 = wrap unaryOp v2 in ()
  | Binary ((v1, v2, v3)) ->
      let v1 = expr v1
      and v2 = wrap binaryOp v2
      and v3 = expr v3
      in ()
  | CondExpr ((v1, v2, v3)) ->
      let v1 = expr v1 and v2 = expr v2 and v3 = expr v3 in ()
  | Sequence ((v1, v2)) -> let v1 = expr v1 and v2 = expr v2 in ()
  | SizeOf v1 -> let v1 = either expr type_ v1 in ()
  | ArrayInit v1 ->
      let v1 =
        list
          (fun (v1, v2) ->
             let v1 = option expr v1 and v2 = expr v2 in ())
          v1
      in ()
  | RecordInit v1 ->
      let v1 =
        list (fun (v1, v2) -> let v1 = name v1 and v2 = expr v2 in ())
          v1
      in ()
  | GccConstructor ((v1, v2)) -> let v1 = type_ v1 and v2 = expr v2 in ()
and argument v = expr v
and const_expr v = expr v
  
let rec stmt =
  function
  | ExprSt v1 -> let v1 = expr v1 in ()
  | Block v1 -> let v1 = list stmt v1 in ()
  | If ((v1, v2, v3)) ->
      let v1 = expr v1 and v2 = stmt v2 and v3 = stmt v3 in ()
  | Switch ((v1, v2)) -> let v1 = expr v1 and v2 = list case v2 in ()
  | While ((v1, v2)) -> let v1 = expr v1 and v2 = stmt v2 in ()
  | DoWhile ((v1, v2)) -> let v1 = stmt v1 and v2 = expr v2 in ()
  | For ((v1, v2, v3, v4)) ->
      let v1 = option expr v1
      and v2 = option expr v2
      and v3 = option expr v3
      and v4 = stmt v4
      in ()
  | Return v1 -> let v1 = option expr v1 in ()
  | Continue -> ()
  | Break -> ()
  | Label ((v1, v2)) -> let v1 = name v1 and v2 = stmt v2 in ()
  | Goto v1 -> let v1 = name v1 in ()
  | Vars v1 -> let v1 = list var_decl v1 in ()
  | Asm v1 -> let v1 = list expr v1 in ()

and case =
  function
  | Case ((v1, v2)) -> let v1 = expr v1 and v2 = list stmt v2 in ()
  | Default v1 -> let v1 = list stmt v1 in ()
and
  var_decl {
               v_name = xname;
               v_type = xtype;
               v_storage = xstorage;
               v_init = init
             } =
  let arg = name xname in
  let arg = type_ xtype in
  let arg = storage xstorage in
  let arg = option initialiser init in ()
and initialiser v = expr v
and storage = function | Extern -> () | Static -> () | DefaultStorage -> ()

let func_def {
                 f_name = f_name;
                 f_type = f_type;
                 f_body = f_body;
                 f_static = f_static
               } =
  let arg = name f_name in
  let arg = function_type f_type in
  let arg = list stmt f_body in let arg = bool f_static in ()

let rec
  struct_def { s_name = s_name; s_kind = s_kind; s_flds = s_flds } =
  let arg = name s_name in
  let arg = struct_kind s_kind in
  let arg = list field_def s_flds in ()
  
and field_def { fld_name = fld_name; fld_type = fld_type } =
  let arg = option name fld_name in let arg = type_ fld_type in ()
  

let enum_def (v1, v2) =
  let v1 = name v1
  and v2 =
    list
      (fun (v1, v2) ->
         let v1 = name v1 and v2 = option const_expr v2 in ())
      v2
  in ()

let type_def (v1, v2) = let v1 = name v1 and v2 = type_ v2 in ()

let define_body =
  function
  | CppExpr v1 -> let v1 = expr v1 in ()
  | CppStmt v1 -> let v1 = stmt v1 in ()

let toplevel =
  function
  | Include v1 -> let v1 = wrap string v1 in ()
  | Define ((v1, v2)) -> let v1 = name v1 and v2 = define_body v2 in ()
  | Macro ((v1, v2, v3)) ->
      let v1 = name v1
      and v2 = list name v2
      and v3 = define_body v3
      in ()
  | StructDef v1 -> let v1 = struct_def v1 in ()
  | TypeDef v1 -> let v1 = type_def v1 in ()
  | EnumDef v1 -> let v1 = enum_def v1 in ()
  | FuncDef v1 -> let v1 = func_def v1 in ()
  | Global v1 -> let v1 = var_decl v1 in ()
  | Prototype v1 -> let v1 = func_def v1 in ()

let program v = 
 (* list toplevel v *)
  raise Todo

(*
let any =
  function
  | Expr v1 -> let v1 = expr v1 in ()
  | Stmt v1 -> let v1 = stmt v1 in ()
  | Type v1 -> let v1 = type_ v1 in ()
  | Toplevel v1 -> let v1 = toplevel v1 in ()
  | Program v1 -> let v1 = program v1 in ()
*)
