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

let list1 _of_a = list _of_a

let ident v = wrap string v

let qualified_ident v = list ident v

let rec typ =
  function
  | TBasic v1 -> let v1 = wrap string v1 in G.TyBuiltin v1
  | TClass v1 -> let v1 = class_type v1 in v1
  | TArray v1 -> let v1 = typ v1 in G.TyArray (None, v1)
and class_type v =
  let res = list1
    (fun (v1, v2) ->
       let v1 = ident v1 and _v2TODO = list type_argument v2 in 
        (v1))
    v
  in
  (match List.rev res with
  | [] -> raise Impossible (* list1 *)
  | name::xs ->
        let info = { G.
            name_typeargs = None; (* could be v1TODO above *)
            name_qualifier = Some (List.rev xs);
          } in
        G.TyApply ((name, info), [])
  )

and type_argument =
  function
  | TArgument v1 -> let v1 = ref_type v1 in G.TypeArg v1
  | TQuestion v1 ->
      let v1 =
        option
          (fun (v1, v2) -> let v1 = bool v1 and v2 = ref_type v2 in (v1,v2))
          v1
      in
      let anys = 
        match v1 with
        | None ->  []
        | Some (_boolTODO, t) -> [G.T t]
      in
      G.OtherTypeArg (G.OTA_Question, anys)
and ref_type v = typ v


let type_parameter =
  function
  | TParam ((v1, v2)) ->
      let v1 = ident v1 and v2 = list ref_type v2 in
      v1, (v2 |> List.map (fun t -> G.Extends t))

let rec modifier =
  function
  | Variadic -> G.Variadic
  | Public -> G.Public
  | Protected -> G.Protected
  | Private -> G.Private
  | Abstract -> G.Abstract
  | Static -> G.Static
  | Final -> G.Final
  | StrictFP -> G.OtherAttribute (G.OA_StrictFP, [])
  | Transient -> G.OtherAttribute (G.OA_Transient, [])
  | Volatile -> G.Volatile
  | Synchronized -> G.OtherAttribute (G.OA_Synchronized, [])
  | Native -> G.OtherAttribute (G.OA_Native, [])
  | Annotation _v1 -> 
      (* let _v1TODO = annotation v1 in *)
      G.OtherAttribute (G.OA_AnnotJavaOther, [])

and modifiers v = list (wrap modifier) v |> List.map fst

(*  
and annotation (v1, v2) =
  let _v1 = name_or_class_type v1
  and _v2 = option annotation_element v2
  in ()
  ()

and annotation_element =
  function
  | AnnotArgValue v1 -> let _v1 = element_value v1 in ()
  | AnnotArgPairInit v1 -> let _v1 = list annotation_pair v1 in ()
  | EmptyAnnotArg -> ()

and element_value =
  function
  | AnnotExprInit v1 -> let _v1 = expr v1 in ()
  | AnnotNestedAnnot v1 -> let _v1 = annotation v1 in ()
  | AnnotArrayInit v1 -> let _v1 = list element_value v1 in ()
and annotation_pair (v1, v2) =
  let _v1 = ident v1 and _v2 = element_value v2 in ()

and name_or_class_type v = list identifier_ v

and identifier_ =
  function
  | Id v1 -> let _v1 = ident v1 in ()
  | Id_then_TypeArgs ((v1, v2)) ->
      let _v1 = ident v1 and _v2 = list type_argument v2 in ()
  | TypeArgs_then_Id ((v1, v2)) ->
      let _v1 = list type_argument v1 and _v2 = identifier_ v2 in ()
*)




and name v =
  let res = list1
    (fun (v1, v2) ->
       let _v1TODO = list type_argument v1 
        and v2 = ident v2 in (v2))
    v
  in
  (match List.rev res with
  | [] -> raise Impossible (* list1 *)
  | name::xs ->
        let info = { G.
            name_typeargs = None; (* could be v1TODO above *)
            name_qualifier = Some (List.rev xs);
          } in
        (name, info)
  )


and literal = function
  | Int v1 -> let v1 = wrap string v1 in (G.Int v1)
  | Float v1 -> let v1 = wrap string v1 in (G.Float v1)
  | String v1 -> let v1 = wrap string v1 in (G.String v1)
  | Char v1 -> let v1 = wrap string v1 in (G.Char v1)
  | Null v1 -> let v1 = tok v1 in (G.Null v1)
  | Bool v1 -> let v1 = wrap bool v1 in (G.Bool v1)

and expr e =
  match e with
  | Ellipses v1 -> let v1 = tok v1 in G.Ellipsis v1
  | Name v1 -> let (a,b) = name v1 in G.Name ((a,b), G.empty_id_info())
  | NameOrClassType _v1 -> 
      let ii = Lib_parsing_java.ii_of_any (AExpr e) in
      error (List.hd ii) 
      "NameOrClassType should only appear in (ignored) annotations" 
  | Literal v1 -> let v1 = literal v1 in
      G.L v1
  | ClassLiteral v1 -> let v1 = typ v1 in
      G.OtherExpr (G.OE_ClassLiteral, [G.T v1])
  | NewClass ((v1, v2, v3)) ->
      let v1 = typ v1
      and v2 = arguments v2
      and v3 = option decls v3 in
      (match v3 with
      | None -> G.Call (G.IdSpecial (G.New, fake_info()), (G.ArgType v1)::v2)
      | Some decls -> 
         let anonclass = G.AnonClass { G.
                ckind = G.Class;
                cextends = [v1];
                cimplements = [];
                cbody = List.map G.stmt_to_field decls } in
         G.Call (G.IdSpecial (G.New, fake_info()), (G.Arg anonclass)::v2)
      )
  | NewArray ((v1, v2, v3, v4)) ->
      let v1 = typ v1
      and v2 = arguments v2
      and v3 = int v3
      and v4 = option init v4
      in 
      let rec mk_array n =
        if n <1 
        then raise Impossible; (* see parser_java.mly dims rule *)
        if n = 1
        then G.TyArray (None, v1) 
        else G.TyArray (None, mk_array (n - 1))
      in
      let t = mk_array v3 in
      (match v4 with
      | None -> G.Call (G.IdSpecial (G.New, fake_info()), (G.ArgType t)::v2)
      | Some _decls -> 
         let ii = Lib_parsing_java.ii_of_any (AExpr e) in
         error (List.hd ii) "TODO: NewArray with initializer not handled yet" 
      )

  (* x.new Y(...) {...} *)
  | NewQualifiedClass ((v1, v2, v3, v4)) ->
      let v1 = expr v1
      and v2 = ident v2
      and v3 = arguments v3
      and v4 = option decls v4
      in 
      let any = 
        [G.E v1; G.Id v2] @ (v3 |> List.map (fun arg -> G.Ar arg)) @
        (Common.opt_to_list v4 |> List.flatten |> List.map
            (fun st -> G.S st)) in
       G.OtherExpr (G.OE_NewQualifiedClass, any)

  | Call ((v1, v2)) -> let v1 = expr v1 and v2 = arguments v2 in
      G.Call (v1, v2)
  | Dot ((v1, v2)) -> let v1 = expr v1 and v2 = ident v2 in 
      G.ObjAccess (v1, v2)
  | ArrayAccess ((v1, v2)) -> let v1 = expr v1 and v2 = expr v2 in
      G.ArrayAccess (v1, v2)
  | Postfix ((v1, (v2, tok))) -> let v1 = expr v1 and v2 = fix_op v2 in
      G.Call (G.IdSpecial (G.IncrDecr (v2, G.Postfix), tok), [G.Arg v1]) 
  | Prefix (((v1, tok), v2)) -> let v1 = fix_op v1 and v2 = expr v2 in
      G.Call (G.IdSpecial (G.IncrDecr (v1, G.Prefix), tok), [G.Arg v2]) 
  | Unary ((v1, v2)) -> let (v1, tok) = v1 and v2 = expr v2 in
      G.Call (G.IdSpecial (G.ArithOp v1, tok), [G.Arg v2]) 
  | Infix ((v1, (v2, tok), v3)) ->
      let v1 = expr v1 and v2 = v2 and v3 = expr v3 in
      G.Call (G.IdSpecial (G.ArithOp (v2), tok), [G.Arg v1; G.Arg v3])
  | Cast ((v1, v2)) -> let v1 = typ v1 and v2 = expr v2 in
    G.Cast (v1, v2)
  | InstanceOf ((v1, v2)) -> let v1 = expr v1 and v2 = ref_type v2 in
    G.Call (G.IdSpecial (G.Instanceof, fake_info ()), 
        [G.Arg v1; G.ArgType v2])
  | Conditional ((v1, v2, v3)) ->
      let v1 = expr v1 and v2 = expr v2 and v3 = expr v3 in
      G.Conditional (v1, v2, v3)
  | Assign ((v1, v2)) ->
      let v1 = expr v1 and v2 = expr v2 in
      G.Assign (v1, v2)
  | AssignOp ((v1, (v2, tok), v3)) ->
      let v1 = expr v1 and v3 = expr v3 in
      G.AssignOp (v1, (v2, tok), v3)
  | Lambda (v1, v2) ->
      let v1 = params v1 in
      let v2 = stmt v2 in
      G.Lambda { G.fparams = v1; frettype = None; fbody = v2 }


and arguments v = list expr v |> List.map (fun e -> G.Arg e)

and fix_op v = v

and stmt =
  function
  | Empty -> G.Block []
  | Block v1 -> let v1 = stmts v1 in G.Block v1
  | Expr v1 -> let v1 = expr v1 in G.ExprStmt v1
  | If ((v1, v2, v3)) ->
      let v1 = expr v1 and v2 = stmt v2 and v3 = stmt v3 in
      G.If (v1, v2, v3)
  | Switch ((v1, v2)) ->
      let v1 = expr v1
      and v2 =
        list
          (fun (v1, v2) -> let v1 = cases v1 and v2 = stmts v2 in 
            v1, G.stmt1 v2
        ) v2
      in
      G.Switch (v1, v2)
  | While ((v1, v2)) -> let v1 = expr v1 and v2 = stmt v2 in
      G.While (v1, v2)
  | Do ((v1, v2)) -> let v1 = stmt v1 and v2 = expr v2 in
      G.DoWhile (v1, v2)
  | For ((v1, v2)) -> let v1 = for_control v1 and v2 = stmt v2 in
      G.For (v1, v2)
  | Break v1 -> let v1 = option ident_label v1 in
      G.Break v1
  | Continue v1 -> let v1 = option ident_label v1 in
      G.Continue v1
  | Return v1 -> let v1 = option expr v1 in
      G.Return v1
  | Label ((v1, v2)) -> let v1 = ident v1 and v2 = stmt v2 in
      G.Label (v1, v2)
  | Sync ((v1, v2)) -> 
      let v1 = expr v1 and v2 = stmt v2 in
      G.OtherStmt (G.OS_Sync, [G.E v1; G.S v2])
  | Try ((v1, v2, v3)) ->
      let v1 = stmt v1
      and v2 = catches v2
      and v3 = option stmt v3
      in
      G.Try (v1, v2, v3)
  | Throw v1 -> let v1 = expr v1 in
      G.Throw v1
  | LocalVar v1 -> let (ent, v) = var_with_init v1 in
      G.DefStmt (ent, G.VarDef v)
  | LocalClass v1 -> let (ent, cdef) = class_decl v1 in
      G.DefStmt (ent, G.ClassDef cdef)
  | Assert ((v1, v2)) -> let v1 = expr v1 and v2 = option expr v2 in
      G.Assert (v1, v2)

and ident_label x =
  let x = ident x in
  G.Name ((x, G.empty_name_info), G.empty_id_info())

and stmts v = list stmt v

and case = function 
  | Case v1 -> let v1 = expr v1 in G.Case v1
  | Default -> G.Default

and cases v = list case v

and for_control =
  function
  | ForClassic ((v1, v2, v3)) ->
      let v1 = for_init v1
      and v2 = list expr v2
      and v3 = list expr v3
      in 
      G.ForClassic (v1, G.Seq v2, G.Seq v3)
  | Foreach ((v1, v2)) -> let ent = var v1 and v2 = expr v2 in
      let pat = G.OtherPat (G.OP_Var, [G.En ent]) in
      G.ForEach (pat, v2)

and for_init =
  function
  | ForInitVars v1 -> let v1 = list var_with_init v1 in
      v1 |> List.map (fun (ent,v) -> G.ForInitVar (ent, v))
  | ForInitExprs v1 -> let v1 = list expr v1 in
      v1 |> List.map (fun e -> G.ForInitExpr e)

and var { name = name; mods = mods; type_ = xtyp } =
  let v1 = ident name in
  let v2 = modifiers mods in 
  let v3 = option typ xtyp in
  { G.name = v1; G.attrs = v2; G.type_ = v3; tparams = [];
    info = G.empty_id_info ();
  }

and catch (v1, v2) = let (ent: G.entity) = var v1 and v2 = stmt v2 in
  let pat = G.OtherPat (G.OP_Var, [G.En ent]) in
  pat, v2
and catches v = list catch v


and vars v = list var v

and var_with_init { f_var = f_var; f_init = f_init } =
  let ent = var f_var in 
  let init = option init f_init in
  ent, {G.vinit = init; vtype = None }

and init =
  function
  | ExprInit v1 -> let v1 = expr v1 in
      v1
  | ArrayInit v1 -> let v1 = list init v1 in
      G.Container (G.Array, v1)

and params v = 
  let v = vars v in
  v |> List.map (fun ent ->
      G.ParamClassic ( G.entity_to_param ent))
and
  method_decl {
                  m_var = m_var;
                  m_formals = m_formals;
                  m_throws = m_throws;
                  m_body = m_body
                } =
  let v1 = var m_var in
  let rett = match v1.G.type_ with None -> raise Impossible | Some x -> x in
  let v2 = params m_formals in
  let v3 = list qualified_ident m_throws in
  let v4 = stmt m_body in
  let throws = v3 |> List.map (fun qu_id ->
        G.OtherAttribute (G.OA_AnnotThrow, [G.Di qu_id]))
  in
  { v1 with G.attrs = v1.G.attrs @ throws },
  { G.fparams = v2; frettype  = Some rett; fbody = v4 }

and field v = var_with_init v

and enum_decl {
                en_name = en_name;
                en_mods = en_mods;
                en_impls = en_impls;
                en_body = en_body
              } =
  let v1 = ident en_name in
  let v2 = modifiers en_mods in
  let _v3TODO = list ref_type en_impls in
  let (v4, v5) = en_body in
  let v4 = list enum_constant v4 in
  let _v5TODO = decls v5 in
  let ent = G.basic_entity v1 v2 in
  let tdef = {G.tbody = G.OrType v4 } in
  ent, tdef

and enum_constant =
  function
  | EnumSimple v1 -> let v1 = ident v1 in 
      G.OrConstructor (v1, [])
  | EnumConstructor ((v1, v2)) ->
      let v1 = ident v1 and _v2TODO = arguments v2 in
      G.OrConstructor (v1, [])
      
  | EnumWithMethods ((v1, v2)) ->
      let v1 = ident v1 and _v2TODO = list method_decl v2 in
      G.OrConstructor (v1, [])

and class_decl {
                 cl_name = cl_name;
                 cl_kind = cl_kind;
                 cl_tparams = cl_tparams;
                 cl_mods = cl_mods;
                 cl_extends = cl_extends;
                 cl_impls = cl_impls;
                 cl_body = cl_body
               } =
  let v1 = ident cl_name in
  let v2 = class_kind cl_kind in
  let v3 = list type_parameter cl_tparams in
  let v4 = modifiers cl_mods in
  let v5 = option typ cl_extends in
  let v6 = list ref_type cl_impls in 
  let v7 = decls cl_body in 
  let fields = List.map G.stmt_to_field v7 in
  let ent = { (G.basic_entity v1 v4) with
      G.tparams = v3 } in
  let cdef = { G.
      ckind = v2;
      cextends = Common.opt_to_list v5;
      cimplements = v6;
      cbody = fields;
    } in
  ent, cdef


and class_kind = function 
  | ClassRegular ->  G.Class
  | Interface -> G.Interface

and decl decl =
  match decl with
  | Class v1 -> let (ent, def) = class_decl v1 in
      G.DefStmt (ent, G.ClassDef def)
  | Method v1 -> let (ent, def) = method_decl v1 in 
      G.DefStmt (ent, G.FuncDef def)
  | Field v1 -> let (ent, def) = field v1 in 
      G.DefStmt (ent, G.VarDef def)
  | Enum v1 -> let (ent, def) = enum_decl v1 in
      G.DefStmt (ent, G.TypeDef def)
  | Init ((v1, v2)) -> let _v1TODO = bool v1 and v2 = stmt v2 in
      v2

and decls v = list decl v

and import = function
  | ImportAll (xs, tok) -> G.ImportAll (G.DottedName xs, tok)
  | ImportFrom (xs, id) -> 
      let id = ident id in
      G.ImportFrom (G.DottedName xs, [id, None])


let compilation_unit { package = package;
                       imports = imports;
                       decls = xdecls
                      } =
  let v1 = option qualified_ident package in
  let v2 =
    list (fun (v1, v2) -> let _v1static = bool v1 and v2 = import v2 in v2)
    imports
  in
  let v3 = decls xdecls in
  let items = v3 |> List.map G.stmt_to_item in
  let imports = v2 |> List.map (fun import -> G.DirectiveStmt import) in
  let package = 
    match v1 with
    | None -> items @ imports
    | Some x -> [G.DirectiveStmt (G.Package x)]
  in
  package @ imports @ items

let program v = 
  compilation_unit v

let any =
  function
  | AIdent v1 -> let v1 = ident v1 in G.Id v1
  | AExpr v1 -> let v1 = expr v1 in G.E v1
  | AStmt v1 -> let v1 = stmt v1 in G.S v1
  | AStmts v1 -> let v1 = List.map stmt v1 in G.Ss v1
  | ATyp v1 -> let v1 = typ v1 in G.T v1
  | AVar v1 -> let v1 = var v1 in G.En v1
  | AInit v1 -> let v1 = init v1 in G.E v1
  | AMethod v1 -> let (ent, def) = method_decl v1 in 
      G.Def (ent, G.FuncDef def)
  | AField v1 -> let (ent, def) = field v1 in
      G.Def (ent, G.VarDef def)
  | AClass v1 -> let (ent, def) = class_decl v1 in
      G.Def (ent, G.ClassDef def)
  | ADecl v1 -> let v1 = decl v1 in G.S v1
  | AProgram v1 -> let v1 = program v1 in G.Pr v1
