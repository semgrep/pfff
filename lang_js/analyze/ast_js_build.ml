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
(* The string is often "advanced es6" or "Typescript" usually *)
exception UnhandledConstruct of string * Parse_info.info

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let _opt f env x =
  match x with
  | None -> None
  | Some x -> Some (f env x)

let rec comma_list = function
  | [] -> []
  | Common.Left x  :: rl -> x :: comma_list rl
  | Common.Right _ :: rl -> comma_list rl

let paren (_, x, _) = x

let _noop = A.Block []

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let rec program xs =
  let env = empty_env () in
  module_items env xs

and module_items env xs =
  xs |> List.map (module_item env) |> List.flatten

and module_item env = function
  | C.It x -> item env x
  | C.Import (tok, _, _)
  | C.Export (tok, _)
     -> raise (TodoConstruct ("namespace", tok))

and item env = function
  | C.St x -> stmt env x
  | C.FunDecl x -> 
    let _fun_ = func_decl env x in 
    raise Todo
  | C.ClassDecl x -> 
    let _class_ = class_decl env x in
    raise Todo
  | C.InterfaceDecl x -> 
    raise (UnhandledConstruct ("Typescript", x.C.i_tok))
  | C.ItemTodo tok ->
    raise (TodoConstruct ("Todo", tok))

(* ------------------------------------------------------------------------- *)
(* Names *)
(* ------------------------------------------------------------------------- *)
and name env x = x
and label env x = x

and property_name env = function
  | C.PN_Id x       -> A.PN (name env x)
  | C.PN_String x   -> A.PN (name env x)
  | C.PN_Num x      -> A.PN (name env x)
  | C.PN_Computed x -> A.PN_Computed (x |> paren |> expr env)

(* ------------------------------------------------------------------------- *)
(* Statement *)
(* ------------------------------------------------------------------------- *)

and stmt env = function
  | C.VarsDecl (_vkind, _vbindings, _) ->
    raise Todo
  | C.Block x -> 
    [A.Block (x |> paren |> List.map (item env) |> List.flatten)]
  | C.Nop _ -> 
    []
  | C.ExprStmt (e, _) ->
    [A.ExprStmt (expr env e)]
  | _ -> raise Todo

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
     | _ -> A.Id (s, tok)
     )
  | C.This tok -> A.IdSpecial (A.This, tok)
  | C.Super tok -> A.IdSpecial (A.Super, tok)


  | _ -> raise Todo

and literal env = function
  | C.Bool x -> A.Bool x
  | C.Num x -> A.Num x
  | C.String x -> A.String x
  | C.Regexp x -> A.Regexp x
  | C.Null tok -> A.IdSpecial (A.Null, tok)

(* ------------------------------------------------------------------------- *)
(* Entities *)
(* ------------------------------------------------------------------------- *)

and func_decl _env _ =
  raise Todo

and class_decl _env _ =
  raise Todo


(* ------------------------------------------------------------------------- *)
(* Misc *)
(* ------------------------------------------------------------------------- *)
