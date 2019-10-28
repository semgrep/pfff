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

open Ast_java
module G = Ast_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Ast_java to Ast_generic.
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

let list1 _of_a = list _of_a

let ident v = wrap string v

let qualified_ident v = list ident v

let rec typ =
  function
  | TBasic v1 -> let v1 = wrap string v1 in ()
  | TClass v1 -> let v1 = class_type v1 in ()
  | TArray v1 -> let v1 = typ v1 in ()
and class_type v =
  list1
    (fun (v1, v2) ->
       let v1 = ident v1 and v2 = list type_argument v2 in ())
    v
and type_argument =
  function
  | TArgument v1 -> let v1 = ref_type v1 in ()
  | TQuestion v1 ->
      let v1 =
        option
          (fun (v1, v2) -> let v1 = bool v1 and v2 = ref_type v2 in ())
          v1
      in ()
and ref_type v = typ v

let type_parameter =
  function
  | TParam ((v1, v2)) ->
      let v1 = ident v1 and v2 = list ref_type v2 in ()

let rec modifier =
  function
  | Public -> ()
  | Protected -> ()
  | Private -> ()
  | Abstract -> ()
  | Static -> ()
  | Final -> ()
  | StrictFP -> ()
  | Transient -> ()
  | Volatile -> ()
  | Synchronized -> ()
  | Native -> ()
  | Annotation v1 -> let v1 = annotation v1 in ()
  
and annotation (v1, v2) =
  let v1 = name_or_class_type v1
  and v2 = option annotation_element v2
  in ()
and modifiers v = list (wrap modifier) v
and annotation_element =
  function
  | AnnotArgValue v1 -> let v1 = element_value v1 in ()
  | AnnotArgPairInit v1 -> let v1 = list annotation_pair v1 in ()
  | EmptyAnnotArg -> ()
and element_value =
  function
  | AnnotExprInit v1 -> let v1 = expr v1 in ()
  | AnnotNestedAnnot v1 -> let v1 = annotation v1 in ()
  | AnnotArrayInit v1 -> let v1 = list element_value v1 in ()
and annotation_pair (v1, v2) =
  let v1 = ident v1 and v2 = element_value v2 in ()

and name_or_class_type v = list identifier_ v

and identifier_ =
  function
  | Id v1 -> let v1 = ident v1 in ()
  | Id_then_TypeArgs ((v1, v2)) ->
      let v1 = ident v1 and v2 = list type_argument v2 in ()
  | TypeArgs_then_Id ((v1, v2)) ->
      let v1 = list type_argument v1 and v2 = identifier_ v2 in ()

and name v =
  list1
    (fun (v1, v2) ->
       let v1 = list type_argument v1 and v2 = ident v2 in ())
    v

and expr =
  function
  | Name v1 -> let v1 = name v1 in ()
  | NameOrClassType v1 -> let v1 = name_or_class_type v1 in ()
  | Literal v1 -> let v1 = wrap string v1 in ()
  | ClassLiteral v1 -> let v1 = typ v1 in ()
  | NewClass ((v1, v2, v3)) ->
      let v1 = typ v1
      and v2 = arguments v2
      and v3 = option decls v3
      in ()
  | NewArray ((v1, v2, v3, v4)) ->
      let v1 = typ v1
      and v2 = arguments v2
      and v3 = int v3
      and v4 = option init v4
      in ()
  | NewQualifiedClass ((v1, v2, v3, v4)) ->
      let v1 = expr v1
      and v2 = ident v2
      and v3 = arguments v3
      and v4 = option decls v4
      in ()
  | Call ((v1, v2)) -> let v1 = expr v1 and v2 = arguments v2 in ()
  | Dot ((v1, v2)) -> let v1 = expr v1 and v2 = ident v2 in ()
  | ArrayAccess ((v1, v2)) -> let v1 = expr v1 and v2 = expr v2 in ()
  | Postfix ((v1, v2)) -> let v1 = expr v1 and v2 = op v2 in ()
  | Prefix ((v1, v2)) -> let v1 = op v1 and v2 = expr v2 in ()
  | Infix ((v1, v2, v3)) ->
      let v1 = expr v1 and v2 = op v2 and v3 = expr v3 in ()
  | Cast ((v1, v2)) -> let v1 = typ v1 and v2 = expr v2 in ()
  | InstanceOf ((v1, v2)) -> let v1 = expr v1 and v2 = ref_type v2 in ()
  | Conditional ((v1, v2, v3)) ->
      let v1 = expr v1 and v2 = expr v2 and v3 = expr v3 in ()
  | Assignment ((v1, v2, v3)) ->
      let v1 = expr v1 and v2 = op v2 and v3 = expr v3 in ()

and arguments v = list expr v

and op v = string v

and stmt =
  function
  | Empty -> ()
  | Block v1 -> let v1 = stmts v1 in ()
  | Expr v1 -> let v1 = expr v1 in ()
  | If ((v1, v2, v3)) ->
      let v1 = expr v1 and v2 = stmt v2 and v3 = stmt v3 in ()
  | Switch ((v1, v2)) ->
      let v1 = expr v1
      and v2 =
        list
          (fun (v1, v2) -> let v1 = cases v1 and v2 = stmts v2 in ()) v2
      in ()
  | While ((v1, v2)) -> let v1 = expr v1 and v2 = stmt v2 in ()
  | Do ((v1, v2)) -> let v1 = stmt v1 and v2 = expr v2 in ()
  | For ((v1, v2)) -> let v1 = for_control v1 and v2 = stmt v2 in ()
  | Break v1 -> let v1 = option ident v1 in ()
  | Continue v1 -> let v1 = option ident v1 in ()
  | Return v1 -> let v1 = option expr v1 in ()
  | Label ((v1, v2)) -> let v1 = ident v1 and v2 = stmt v2 in ()
  | Sync ((v1, v2)) -> let v1 = expr v1 and v2 = stmt v2 in ()
  | Try ((v1, v2, v3)) ->
      let v1 = stmt v1
      and v2 = catches v2
      and v3 = option stmt v3
      in ()
  | Throw v1 -> let v1 = expr v1 in ()
  | LocalVar v1 -> let v1 = var_with_init v1 in ()
  | LocalClass v1 -> let v1 = class_decl v1 in ()
  | Assert ((v1, v2)) -> let v1 = expr v1 and v2 = option expr v2 in ()

and stmts v = list stmt v

and case = function | Case v1 -> let v1 = expr v1 in () | Default -> ()

and cases v = list case v

and for_control =
  function
  | ForClassic ((v1, v2, v3)) ->
      let v1 = for_init v1
      and v2 = list expr v2
      and v3 = list expr v3
      in ()
  | Foreach ((v1, v2)) -> let v1 = var v1 and v2 = expr v2 in ()

and for_init =
  function
  | ForInitVars v1 -> let v1 = list var_with_init v1 in ()
  | ForInitExprs v1 -> let v1 = list expr v1 in ()

and catch (v1, v2) = let v1 = var v1 and v2 = stmt v2 in ()
and catches v = list catch v

and var { v_name = name; v_mods = mods; v_type = xtyp } =
  let arg = ident name in
  let arg = modifiers mods in let arg = typ xtyp in ()

and vars v = list var v
and var_with_init { f_var = f_var; f_init = f_init } =
  let arg = var f_var in let arg = option init f_init in ()

and init =
  function
  | ExprInit v1 -> let v1 = expr v1 in ()
  | ArrayInit v1 -> let v1 = list init v1 in ()

and
  method_decl {
                  m_var = m_var;
                  m_formals = m_formals;
                  m_throws = m_throws;
                  m_body = m_body
                } =
  let arg = var m_var in
  let arg = vars m_formals in
  let arg = list qualified_ident m_throws in
  let arg = stmt m_body in ()

and field v = var_with_init v

and enum_decl {
                en_name = en_name;
                en_mods = en_mods;
                en_impls = en_impls;
                en_body = en_body
              } =
  let arg = ident en_name in
  let arg = modifiers en_mods in
  let arg = list ref_type en_impls in
  let arg =
    match en_body with
    | (v1, v2) ->
        let v1 = list enum_constant v1 and v2 = decls v2 in ()
  in ()

and enum_constant =
  function
  | EnumSimple v1 -> let v1 = ident v1 in ()
  | EnumConstructor ((v1, v2)) ->
      let v1 = ident v1 and v2 = arguments v2 in ()
  | EnumWithMethods ((v1, v2)) ->
      let v1 = ident v1 and v2 = list method_decl v2 in ()

and class_decl {
                 cl_name = cl_name;
                 cl_kind = cl_kind;
                 cl_tparams = cl_tparams;
                 cl_mods = cl_mods;
                 cl_extends = cl_extends;
                 cl_impls = cl_impls;
                 cl_body = cl_body
               } =
  let arg = ident cl_name in
  let arg = class_kind cl_kind in
  let arg = list type_parameter cl_tparams in
  let arg = modifiers cl_mods in
  let arg = option typ cl_extends in
  let arg = list ref_type cl_impls in let arg = decls cl_body in ()

and class_kind = function | ClassRegular -> () | Interface -> ()

and decl =
  function
  | Class v1 -> let v1 = class_decl v1 in ()
  | Method v1 -> let v1 = method_decl v1 in ()
  | Field v1 -> let v1 = field v1 in ()
  | Enum v1 -> let v1 = enum_decl v1 in ()
  | Init ((v1, v2)) -> let v1 = bool v1 and v2 = stmt v2 in ()

and decls v = list decl v

let compilation_unit {
                         package = package;
                         imports = imports;
                         decls = xdecls
                       } =
  let arg = option qualified_ident package in
  let arg =
    list
      (fun (v1, v2) -> let v1 = bool v1 and v2 = qualified_ident v2 in ())
      imports in
  let arg = decls xdecls in ()

let program v = 
  (* compilation_unit v *)
  raise Todo

(*
let any =
  function
  | AIdent v1 -> let v1 = ident v1 in ()
  | AExpr v1 -> let v1 = expr v1 in ()
  | AStmt v1 -> let v1 = stmt v1 in ()
  | ATyp v1 -> let v1 = typ v1 in ()
  | AVar v1 -> let v1 = var v1 in ()
  | AInit v1 -> let v1 = init v1 in ()
  | AMethod v1 -> let v1 = method_decl v1 in ()
  | AField v1 -> let v1 = field v1 in ()
  | AClass v1 -> let v1 = class_decl v1 in ()
  | ADecl v1 -> let v1 = decl v1 in ()
  | AProgram v1 -> let v1 = program v1 in ()
*)
