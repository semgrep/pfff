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

open Ast_ml
module G = Ast_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Ast_ml to Ast_generic.
 *
 * See ast_generic.ml for more information.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let id = fun x -> x
let option = Common.map_opt
let list = List.map

let string = id
let bool = id
let int = id

let error = Ast_generic.error

let fake_info () = Parse_info.fake_info "FAKE"

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let info x = x
let tok v = info v

let wrap = fun _of_a (v1, v2) ->
  let v1 = _of_a v1 and v2 = info v2 in 
  (v1, v2)

let rec ident v = wrap string v

and name (v1, v2) = let v1 = qualifier v1 and v2 = ident v2 in ()

and qualifier v = list ident v

and type_ =
  function
  | TyName v1 -> let v1 = name v1 in ()
  | TyVar v1 -> let v1 = ident v1 in ()
  | TyFunction ((v1, v2)) -> let v1 = type_ v1 and v2 = type_ v2 in ()
  | TyApp ((v1, v2)) -> let v1 = list type_ v1 and v2 = name v2 in ()
  | TyTuple v1 -> let v1 = list type_ v1 in ()

and expr =
  function
  | L v1 -> let v1 = literal v1 in ()
  | Name v1 -> let v1 = name v1 in ()
  | Constructor ((v1, v2)) ->
      let v1 = name v1 and v2 = option expr v2 in ()
  | Tuple v1 -> let v1 = list expr v1 in ()
  | List v1 -> let v1 = list expr v1 in ()
  | Sequence v1 -> let v1 = list expr v1 in ()
  | Prefix ((v1, v2)) -> let v1 = wrap string v1 and v2 = expr v2 in ()
  | Infix ((v1, v2, v3)) ->
      let v1 = expr v1 and v2 = wrap string v2 and v3 = expr v3 in ()
  | Call ((v1, v2)) -> let v1 = expr v1 and v2 = list argument v2 in ()
  | RefAccess ((v1, v2)) -> let v1 = tok v1 and v2 = expr v2 in ()
  | RefAssign ((v1, v2, v3)) ->
      let v1 = expr v1 and v2 = tok v2 and v3 = expr v3 in ()
  | FieldAccess ((v1, v2)) -> let v1 = expr v1 and v2 = name v2 in ()
  | FieldAssign ((v1, v2, v3)) ->
      let v1 = expr v1 and v2 = name v2 and v3 = expr v3 in ()
  | Record ((v1, v2)) ->
      let v1 = option expr v1
      and v2 =
        list (fun (v1, v2) -> let v1 = name v1 and v2 = expr v2 in ())
          v2
      in ()
  | New ((v1, v2)) -> let v1 = tok v1 and v2 = name v2 in ()
  | ObjAccess ((v1, v2)) -> let v1 = expr v1 and v2 = ident v2 in ()
  | LetIn ((v1, v2, v3)) ->
      let v1 = list let_binding v1
      and v2 = expr v2
      and v3 = rec_opt v3
      in ()
  | Fun ((v1, v2)) -> let v1 = list parameter v1 and v2 = expr v2 in ()
  | Nop -> ()
  | If ((v1, v2, v3)) ->
      let v1 = expr v1 and v2 = expr v2 and v3 = expr v3 in ()
  | Match ((v1, v2)) ->
      let v1 = expr v1 and v2 = list match_case v2 in ()
  | Try ((v1, v2)) ->
      let v1 = expr v1 and v2 = list match_case v2 in ()
  | While ((v1, v2)) -> let v1 = expr v1 and v2 = expr v2 in ()
  | For ((v1, v2, v3, v4, v5)) ->
      let v1 = ident v1
      and v2 = expr v2
      and v3 = for_direction v3
      and v4 = expr v4
      and v5 = expr v5
      in ()
  
and literal =
  function
  | Int v1 -> let v1 = wrap string v1 in ()
  | Float v1 -> let v1 = wrap string v1 in ()
  | Char v1 -> let v1 = wrap string v1 in ()
  | String v1 -> let v1 = wrap string v1 in ()

and argument =
  function
  | Arg v1 -> let v1 = expr v1 in ()
  | ArgKwd ((v1, v2)) -> let v1 = ident v1 and v2 = expr v2 in ()
  | ArgQuestion ((v1, v2)) -> let v1 = ident v1 and v2 = expr v2 in ()

and match_case (v1, v2) =
  let v1 = pattern v1 and v2 = match_action v2 in ()

and match_action (v1, v2) =
  let v1 = expr v1 and v2 = option expr v2 in ()

and for_direction =
  function
  | To v1 -> let v1 = tok v1 in ()
  | Downto v1 -> let v1 = tok v1 in ()

and rec_opt v = option tok v

and pattern =
  function
  | PatVar v1 -> let v1 = ident v1 in ()
  | PatLiteral v1 -> let v1 = literal v1 in ()
  | PatConstructor ((v1, v2)) ->
      let v1 = name v1 and v2 = option pattern v2 in ()
  | PatConsInfix ((v1, v2, v3)) ->
      let v1 = pattern v1 and v2 = tok v2 and v3 = pattern v3 in ()
  | PatTuple v1 -> let v1 = list pattern v1 in ()
  | PatList v1 -> let v1 = list pattern v1 in ()
  | PatUnderscore v1 -> let v1 = tok v1 in ()
  | PatRecord v1 ->
      let v1 =
        list
          (fun (v1, v2) -> let v1 = name v1 and v2 = pattern v2 in ()) v1
      in ()
  | PatAs ((v1, v2)) -> let v1 = pattern v1 and v2 = ident v2 in ()
  | PatDisj ((v1, v2)) -> let v1 = pattern v1 and v2 = pattern v2 in ()
  | PatTyped ((v1, v2)) -> let v1 = pattern v1 and v2 = type_ v2 in ()

and let_binding =
  function
  | LetClassic v1 -> let v1 = let_def v1 in ()
  | LetPattern ((v1, v2)) -> let v1 = pattern v1 and v2 = expr v2 in ()

and let_def { lname = lname; lparams = lparams; lbody = lbody } =
  let arg = ident lname in
  let arg = list parameter lparams in let arg = expr lbody in ()

and parameter v = pattern v
  
and type_declaration { tname = tname; tparams = tparams; tbody = tbody
                     } =
  let arg = ident tname in
  let arg = list type_parameter tparams in
  let arg = type_def_kind tbody in ()

and type_parameter v = ident v

and type_def_kind =
  function
  | AbstractType -> ()
  | CoreType v1 -> let v1 = type_ v1 in ()
  | AlgebricType v1 ->
      let v1 =
        list
          (fun (v1, v2) ->
             let v1 = ident v1 and v2 = list type_ v2 in ())
          v1
      in ()
  | RecordType v1 ->
      let v1 =
        list
          (fun (v1, v2, v3) ->
             let v1 = ident v1
             and v2 = type_ v2
             and v3 = option tok v3
             in ())
          v1
      in ()
  
and module_declaration { mname = mname; mbody = mbody } =
  let arg = ident mname in let arg = module_expr mbody in ()

and module_expr =
  function
  | ModuleName v1 -> let v1 = name v1 in ()
  | ModuleStruct v1 -> let v1 = list item v1 in ()

and item =
  function
  | Type v1 -> let v1 = list type_declaration v1 in ()
  | Exception ((v1, v2)) ->
      let v1 = ident v1 and v2 = list type_ v2 in ()
  | External ((v1, v2, v3)) ->
      let v1 = ident v1
      and v2 = type_ v2
      and v3 = list (wrap string) v3
      in ()
  | Open v1 -> let v1 = name v1 in ()
  | Val ((v1, v2)) -> let v1 = ident v1 and v2 = type_ v2 in ()
  | Let ((v1, v2)) ->
      let v1 = rec_opt v1 and v2 = list let_binding v2 in ()
  | Module v1 -> let v1 = module_declaration v1 in ()
  
