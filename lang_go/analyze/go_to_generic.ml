(* Yoann Padioleau
 *
 * Copyright (C) 2020 r2c
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

open Ast_go
module G = Ast_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Ast_go to Ast_generic.
 *
 * See ast_generic.ml for more information.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let id = fun x -> x
let string = id
let list = List.map
let option = Common.map_opt
let either = Ocaml.map_of_either

let arithmetic_operator = id
let incr_decr = id
let prefix_postfix = id

let error = Ast_generic.error

let name_of_qualified_ident = function
  | Left id -> id, G.empty_name_info
  | Right (xs, id) -> id, { G.name_qualifier = Some xs; name_typeargs = None }

let fake_info () = Parse_info.fake_info "FAKE"
let fake_name s = (s, fake_info ()), G.empty_name_info

let ii_of_any = Lib_parsing_go.ii_of_any

(* TODO? do results "parameters" can have names? *)
let return_type_of_results results = 
  match results with
  | [] | [{G. ptype = None; _}] -> raise Impossible
  | [{G. ptype = Some t; _}] -> t
  | xs -> G.TyTuple (xs |> List.map (function
            | { G.ptype = Some t;_ } -> t
            | { G.ptype = None; _ } -> raise Impossible
            ))

let mk_func_def params ret st =
 { G.
    fparams = params |> List.map (fun x -> G.ParamClassic x);
    frettype = Some ret;
    fbody = st;
  }
      
      

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let tok v = v

let wrap _of_a (v1, v2) = 
  let v1 = _of_a v1 and v2 = tok v2 in 
  (v1, v2)

let ident v = wrap string v

let qualified_ident v = 
  match list ident v with
  | [x] -> Left x
  | [x;y] -> Right ([x], y)
  | _ -> raise Impossible

let top_func () = 
let anon_types = ref [] in
let cnt = ref 0 in
let gensym () = 
  incr cnt;
  spf "_anon%d" !cnt
in

let rec type_ =
  function
  | TName v1 -> let v1 = qualified_ident v1 in
      G.TyApply (name_of_qualified_ident v1, [])
  | TPtr v1 -> let v1 = type_ v1 in 
      G.TyPointer v1
  | TArray ((v1, v2)) -> let v1 = expr v1 and v2 = type_ v2 in 
      G.TyArray (Some v1, v2)
  | TSlice v1 -> let v1 = type_ v1 in 
      G.TyArray (None, v1) (* not sure worth introducing an OT_Slice *)
  | TArrayEllipsis ((v1, v2)) -> let v1 = tok v1 and v2 = type_ v2 in
      G.TyArray (None, v2)
  | TFunc v1 -> let (params, ret) = func_type v1 in 
      G.TyFun (params, ret)
  | TMap ((v1, v2)) -> let v1 = type_ v1 and v2 = type_ v2 in 
      G.TyApply (fake_name "map", [G.TypeArg v1; G.TypeArg v2])
  | TChan ((v1, v2)) -> let v1 = chan_dir v1 and v2 = type_ v2 in 
      G.TyApply (fake_name "chan", [G.TypeArg v1; G.TypeArg v2])

  | TStruct v1 -> let v1 = list struct_field v1 in 
      (* could also use StructName *)
      let s = gensym () in
      let ent = G.basic_entity (s, fake_info ()) [] in
      let def = G.TypeDef { G.tbody = G.AndType v1 } in
      Common.push (ent, def) anon_types;
      G.TyApply (fake_name s, [])
  | TInterface v1 -> let v1 = list interface_field v1 in 
      let s = gensym () in
      let ent = G.basic_entity (s, fake_info ()) [] in
      let def = G.ClassDef { G.ckind = G.Interface; 
          cextends = []; cimplements = []; 
          cbody = v1; } in
      Common.push (ent, def) anon_types;
      G.TyApply (fake_name s, [])

and chan_dir = function 
  | TSend -> G.TyApply (fake_name "send", [])
  | TRecv -> G.TyApply (fake_name "recv", []) 
  | TBidirectional -> G.TyApply (fake_name "bidirectional", [])

and func_type { fparams = fparams; fresults = fresults } =
  let fparams = list parameter fparams in
  let fresults = list parameter fresults in
  fparams, return_type_of_results fresults

and parameter { pname = pname; ptype = ptype; pdots = pdots } =
  let arg1 = option ident pname in
  let arg2 = type_ ptype in 
  let arg3 = option tok pdots in 
  { G.pname = arg1; ptype = Some arg2; 
    pdefault = None; 
    pattrs = (match arg3 with None -> [] | Some _ -> [G.Variadic]);
    pinfo = G.empty_id_info ()
    }

and struct_field (v1, v2) =
  let v1 = struct_field_kind v1 and v2TODO = option tag v2 in
  v1

and struct_field_kind =
  function
  | Field ((v1, v2)) -> let v1 = ident v1 and v2 = type_ v2 in 
      let ent = G.basic_entity v1 [] in
      G.FieldVar (ent, { G.vinit = None; vtype = Some v2 })
  | EmbeddedField ((v1, v2)) ->
      let _v1TODO = option tok v1 and v2 = qualified_ident v2 in
      let name = name_of_qualified_ident v2 in
      G.FieldSpread (G.Name (name, G.empty_id_info()))

and tag v = wrap string v

and interface_field =
  function
  | Method ((v1, v2)) -> 
      let v1 = ident v1 in
      let (params, ret) = func_type v2 in
      let ent = G.basic_entity v1 [] in
      G.FieldMethod (ent, mk_func_def params ret (Block []))
  | EmbeddedInterface v1 -> let v1 = qualified_ident v1 in 
      let name = name_of_qualified_ident v1 in
      G.FieldSpread (G.Name (name, G.empty_id_info()))

and expr_or_type v = either expr type_ v



and expr =
  function
  | BasicLit v1 -> let v1 = literal v1 in 
      G.L v1
  | Id (v1, vref) -> let v1 = ident v1 in 
      G.Name ((v1, G.empty_name_info), 
        { G.id_resolved = vref; id_type = ref None })
  | Selector ((v1, v2, v3)) ->
      let v1 = expr v1 and v2 = tok v2 and v3 = ident v3 in
      G.ObjAccess (v1, v3)
  | Index ((v1, v2)) -> let v1 = expr v1 and v2 = index v2 in
      G.ArrayAccess (v1, v2)
  | Call v1 -> let (e, args) = call_expr v1 in 
      G.Call (e, args)
  | Cast ((v1, v2)) -> let v1 = type_ v1 and v2 = expr v2 in
      G.Cast (v1, v2)
  | Deref ((v1, v2)) -> let v1 = tok v1 and v2 = expr v2 in
      G.DeRef v2
  | Ref ((v1, v2)) -> let v1 = tok v1 and v2 = expr v2 in
      G.Ref v2
  | Unary ((v1, v2)) ->
      let (v1, tok) = wrap arithmetic_operator v1
      and v2 = expr v2
      in
      G.Call (G.IdSpecial (ArithOp v1, tok), [G.expr_to_arg v2])
  | Binary ((v1, v2, v3)) ->
      let v1 = expr v1
      and (v2, tok) = wrap arithmetic_operator v2
      and v3 = expr v3
      in
      G.Call (G.IdSpecial (ArithOp v2, tok), [v1;v3] |> List.map G.expr_to_arg)
  | CompositeLit ((v1, v2)) ->
      let v1 = type_ v1 and v2 = list init v2 in
      G.Call (G.IdSpecial (G.New, fake_info ()), 
        (G.ArgType v1)::(v2 |> List.map G.expr_to_arg))
  | Slice ((v1, v2)) ->
      let e = expr v1 in
      let (v1, v2, v3) = v2 in
      let v1 = option expr v1
      and v2 = option expr v2
      and v3 = option expr v3
      in 
      G.SliceAccess (e, v1, v2, v3)
  | TypeAssert ((v1, v2)) -> let v1 = expr v1 and v2 = type_ v2 in
      G.Call (G.IdSpecial (G.Instanceof, fake_info()),
        [G.Arg v1; G.ArgType v2])
  | EllipsisTODO v1 -> let v1 = tok v1 in 
      G.Ellipsis v1
  | FuncLit ((v1, v2)) -> 
      let (params, ret) = func_type v1 
      and v2 = stmt v2 
      in
      G.Lambda (mk_func_def params ret v2)
  | Receive ((v1, v2)) -> let v1 = tok v1 and v2 = expr v2 in 
      G.OtherExpr (G.OE_Recv, [G.E v2])
  | Send ((v1, v2, v3)) ->
      let v1 = expr v1 and v2 = tok v2 and v3 = expr v3 in
      G.OtherExpr (G.OE_Send, [G.E v1; G.E v3])
  | TypeSwitchExpr ((v1, v2)) -> let v1 = expr v1 and v2 = tok v2 in
      error v2 "TypeSwitchExpr should be handled in Switch statement"
  | ParenType v1 -> let _v1 = type_ v1 in
      error (ii_of_any (T v1) |> List.hd) "ParenType should disappear"

and literal =
  function
  | Int v1 -> let v1 = wrap string v1 in 
      G.Int v1
  | Float v1 -> let v1 = wrap string v1 in 
      G.Float v1
  | Imag v1 -> let v1 = wrap string v1 in 
      G.Imag v1
  | Rune v1 -> let v1 = wrap string v1 in 
      G.Char v1
  | String v1 -> let v1 = wrap string v1 in 
      G.String v1

and index v = expr v

and arguments v = list argument v
and argument =
  function
  | Arg v1 -> let v1 = expr v1 in 
      G.Arg v1
  | ArgType v1 -> let v1 = type_ v1 in 
      G.ArgType v1
  | ArgDots (v1, v2) -> let v1 = expr v1 in let v2 = tok v2 in
      let special = G.Call (G.IdSpecial (G.Spread, v2), [G.expr_to_arg v1]) in
      G.Arg special

and init =
  function
  | InitExpr v1 -> let v1 = expr v1 in v1
  | InitKeyValue ((v1, v2, v3)) ->
      let v1 = init v1 and v2 = tok v2 and v3 = init v3 in
      G.Tuple [v1; v3]
  | InitBraces v1 -> let v1 = list init v1 in
      G.Container (G.List, v1)

and constant_expr v = expr v


and stmt =
  function
  | DeclStmts v1 -> let v1 = list decl v1 in raise Todo
  | Block v1 -> let v1 = list stmt v1 in raise Todo
  | Empty -> raise Todo
  | ExprStmt v1 -> let v1 = expr v1 in raise Todo
  | Assign ((v1, v2, v3)) ->
      let v1 = list expr v1
      and v2 = tok v2
      and v3 = list expr v3
      in raise Todo
  | DShortVars ((v1, v2, v3)) ->
      let v1 = list expr v1
      and v2 = tok v2
      and v3 = list expr v3
      in raise Todo
  | AssignOp ((v1, v2, v3)) ->
      let v1 = expr v1
      and v2 = wrap arithmetic_operator v2
      and v3 = expr v3
      in raise Todo
  | IncDec ((v1, v2, v3)) ->
      let v1 = expr v1
      and v2 = wrap incr_decr v2
      and v3 = prefix_postfix v3
      in raise Todo
  | If ((v1, v2, v3, v4)) ->
      let v1 = option stmt v1
      and v2 = expr v2
      and v3 = stmt v3
      and v4 = option stmt v4
      in raise Todo
  | Switch ((v1, v2, v3)) ->
      let v1 = option stmt v1
      and v2 = option stmt v2
      and v3 = list case_clause v3
      in raise Todo
  | Select ((v1, v2)) ->
      let v1 = tok v1 and v2 = list comm_clause v2 in raise Todo
  | For ((v1, v2)) ->
      let v1 =
        (match v1 with
         | (v1, v2, v3) ->
             let v1 = option stmt v1
             and v2 = option expr v2
             and v3 = option stmt v3
             in raise Todo)
      and v2 = stmt v2
      in raise Todo
  | Range ((v1, v2, v3, v4)) ->
      let v1 =
        option
          (fun (v1, v2) -> let v1 = list expr v1 and v2 = tok v2 in raise Todo)
          v1
      and v2 = tok v2
      and v3 = expr v3
      and v4 = stmt v4
      in raise Todo
  | Return ((v1, v2)) ->
      let v1 = tok v1 and v2 = option (list expr) v2 in raise Todo
  | Break ((v1, v2)) -> let v1 = tok v1 and v2 = option ident v2 in raise Todo
  | Continue ((v1, v2)) ->
      let v1 = tok v1 and v2 = option ident v2 in raise Todo
  | Goto ((v1, v2)) -> let v1 = tok v1 and v2 = ident v2 in raise Todo
  | Fallthrough v1 -> let v1 = tok v1 in raise Todo
  | Label ((v1, v2)) -> let v1 = ident v1 and v2 = stmt v2 in raise Todo
  | Go ((v1, v2)) -> let v1 = tok v1 and v2 = call_expr v2 in raise Todo
  | Defer ((v1, v2)) -> let v1 = tok v1 and v2 = call_expr v2 in raise Todo

and case_clause (v1, v2) = let v1 = case_kind v1 and v2 = stmt v2 in ()
and case_kind =
  function
  | CaseExprs v1 -> let v1 = list expr_or_type v1 in ()
  | CaseAssign ((v1, v2, v3)) ->
      let v1 = list expr_or_type v1
      and v2 = tok v2
      and v3 = expr v3
      in ()
  | CaseDefault v1 -> let v1 = tok v1 in ()

and comm_clause v = case_clause v

and call_expr (v1, v2) = 
  let v1 = expr v1 and v2 = arguments v2 in 
  v1, v2


and decl =
  function
  | DConst ((v1, v2, v3)) ->
      let v1 = ident v1
      and v2 = option type_ v2
      and v3 = option constant_expr v3
      in 
      raise Todo
  | DVar ((v1, v2, v3)) ->
      let v1 = ident v1
      and v2 = option type_ v2
      and v3 = option expr v3
      in
      raise Todo
  | DTypeAlias ((v1, v2, v3)) ->
      let v1 = ident v1 and v2 = tok v2 and v3 = type_ v3 in 
      raise Todo
  | DTypeDef ((v1, v2)) -> let v1 = ident v1 and v2 = type_ v2 in 
      raise Todo

and top_decl =
  function
  | DFunc ((v1, (v2, v3))) ->
      let v1 = ident v1 and (params, ret) = func_type v2 and v3 = stmt v3 in 
      let ent = G.basic_entity v1 [] in
      G.DefStmt (ent, G.FuncDef (mk_func_def params ret v3))
  | DMethod ((v1, v2, (v3, v4))) ->
      let v1 = ident v1
      and v2 = parameter v2
      and v3 = func_type v3
      and v4 = stmt v4
      in
      raise Todo
  | D v1 -> let v1 = decl v1 in
      raise Todo

and import { i_path = i_path; i_kind = i_kind } =
  let module_name = G.FileName (wrap string i_path) in
  let (s,tok) = i_path in
  import_kind i_kind module_name (Filename.basename s, tok)
  
and import_kind kind module_name id =
  match kind with
  | ImportOrig -> 
     (* in Go, import "a/b/c" is really equivalent to import c "a/b/c" *)
      G.ImportAs (module_name, Some id)
  | ImportNamed v1 -> let v1 = ident v1 in 
      G.ImportAs (module_name, Some v1)
  | ImportDot v1 -> let v1 = tok v1 in 
      G.ImportAll (module_name, v1)

and program { package = package; imports = imports; decls = decls } =
  let arg1 = ident package |> (fun x -> G.DirectiveStmt (G.Package [x])) in
  let arg2 = list import imports |> List.map (fun x -> G.DirectiveStmt x) in
  let arg3 = list top_decl decls in
  arg1 :: arg2 @ arg3
  

and any =
  function
  | E v1 -> let v1 = expr v1 in G.E v1
  | S v1 -> let v1 = stmt v1 in G.S v1
  | T v1 -> let v1 = type_ v1 in G.T v1
  | Decl v1 -> let v1 = decl v1 in G.S v1
  | I v1 -> let v1 = import v1 in G.S (G.DirectiveStmt v1)
  | P v1 -> let v1 = program v1 in G.Pr v1
  | Ident v1 -> let v1 = ident v1 in G.Id v1
  | Ss v1 -> let v1 = list stmt v1 in G.Ss v1

in
program, any


let program x = 
  let p, _ = top_func () in
  p x

let any x = 
  let _, a = top_func () in
  a x
