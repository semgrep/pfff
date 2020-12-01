(* Copyright (C) 2012 Facebook
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
open OCaml

open Ast_java

(* Disable warnings against unused variables *)
[@@@warning "-26"]

(* Continuation-style visitor for a subset of concepts; similar to
   visitor_php. The bulk of this file was generated with:

   ocamltarzan -choice vi ast_java.ml

   (cf. 'generated by' comment below). The main visitor hooks
   were carefully handcrafted during a coffee binge.
*)

(* hooks *)
type visitor_in = {
  kident:   (ident       -> unit) * visitor_out -> ident       -> unit;
  kexpr:    (expr        -> unit) * visitor_out -> expr        -> unit;
  kstmt:    (stmt        -> unit) * visitor_out -> stmt        -> unit;
  ktype:    (typ         -> unit) * visitor_out -> typ         -> unit;
  kvar:     (var_definition      -> unit) * visitor_out -> var_definition         -> unit;
  kinit:    (init        -> unit) * visitor_out -> init        -> unit;
  kmethod:  (method_decl -> unit) * visitor_out -> method_decl -> unit;
  kfield:   (field       -> unit) * visitor_out -> field       -> unit;
  kclass:   (class_decl  -> unit) * visitor_out -> class_decl  -> unit;
  kdecl:    (decl        -> unit) * visitor_out -> decl        -> unit;
  kprogram: (program     -> unit) * visitor_out -> program     -> unit;

  kinfo: (tok -> unit) * visitor_out -> tok -> unit;

}
and visitor_out = any -> unit

let default_visitor = {
  kident   = (fun (k,_) x -> k x);
  kexpr    = (fun (k,_) x -> k x);
  kstmt    = (fun (k,_) x -> k x);
  ktype    = (fun (k,_) x -> k x);
  kvar     = (fun (k,_) x -> k x);
  kinit    = (fun (k,_) x -> k x);
  kmethod  = (fun (k,_) x -> k x);
  kfield   = (fun (k,_) x -> k x);
  kclass   = (fun (k,_) x -> k x);
  kdecl    = (fun (k,_) x -> k x);
  kprogram = (fun (k,_) x -> k x);
  kinfo = (fun (k,_) x -> k x);
}


let (mk_visitor: visitor_in -> visitor_out) = fun vin ->

  let rec v_wrap: 'a. ('a -> unit) -> 'a wrap -> unit = fun _of_a (v1, v2) ->
    let v1 = _of_a v1 and v2 = v_info v2 in ()

  and v_bracket: 'a. ('a -> unit) -> 'a bracket -> unit =
    fun of_a (v1, v2, v3) ->
      let v1 = v_info v1 and v2 = of_a v2 and v3 = v_info v3 in ()

  and v_info x =
    let k _x = () in
    vin.kinfo (k, all_functions) x

  and v_tok x = v_info x

  and v_incr_decr _x = ()

  and v_modifiers xs = v_list (v_wrap v_modifier) xs

  and v_modifier = function
    | Public | Protected | Private
    | Abstract | Final
    | Static
    | Transient | Volatile | Native | StrictFP
    | Synchronized
    | DefaultModifier -> ()
    | Annotation v1 -> v_annotation v1

  (* TODO: we should remove visitor_js anyway *)
  and v_annotation (_v1, _v2, _v3) =
    Common.pr2_once "TODO: Visitor_java.v_annotation"

  and v_directive_stmts s = (v_list v_import) s

  and v_program x =
    let k x = (v_list v_stmt) x in
    vin.kprogram (k, all_functions) x

  and v_partial = function
    | PartialDecl x -> v_decl x

  and v_any x = match x with
    | AMod x -> v_wrap v_modifier x
    | Partial x -> v_partial x
    | AIdent i   -> v_ident i
    | AExpr e   -> v_expr e
    | AStmt s    -> v_stmt s
    | AStmts s    -> v_list v_stmt s
    | ATyp t     -> v_typ t
    | AVar v     -> v_var v
    | AInit i   -> v_init i
    | AMethod m -> v_method_decl m
    | AField f  -> v_field f
    | AClass c  -> v_class_decl c
    | AProgram p -> v_program p

  (* generated by ocamltarzan with: camlp4o -o /tmp/yyy.ml -I pa/ pa_type_conv.cmo pa_visitor.cmo  pr_o.cmo /tmp/xxx.ml  *)

  and v_type_parameter =
    function
    | TParam (v1, v2) ->
        let v1 = v_ident v1 and v2 = v_list v_ref_type v2 in ()

  and v_qualified_ident v =
    v_list v_ident v

  and v_ident v =
    let k x = v_wrap v_string x in
    vin.kident (k, all_functions) v

  and v_typ x =
    let k x = match x with
      | TBasic v1 -> let v1 = v_wrap v_string v1 in ()
      | TClass v1 -> let v1 = v_class_type v1 in ()
      | TArray v1 -> let v1 = v_bracket v_typ v1 in ()
    in
    vin.ktype (k, all_functions) x

  and v_class_type v =
    v_list
      (fun (v1, v2) ->
         let v1 = v_ident v1 and v2 = v_list v_type_argument v2 in ())
      v

  and v_list1 _of_a = v_list _of_a
  and v_name v =
    v_list1
      (fun (v1, v2) ->
         let v1 = v_list v_type_argument v1 and v2 = v_ident v2 in ())
      v
  and v_name_or_class_type v = v_list v_identifier_ v
  and v_identifier_ =
    function
    | Id v1 -> let v1 = v_ident v1 in ()
    | Id_then_TypeArgs (v1, v2) ->
        let v1 = v_ident v1 and v2 = v_list v_type_argument v2 in ()
    | TypeArgs_then_Id (v1, v2) ->
        let v1 = v_list v_type_argument v1 and v2 = v_identifier_ v2 in ()
  and v_type_argument =
    function
    | TArgument v1 -> let v1 = v_ref_type v1 in ()
    | TWildCard (v1, v2) ->
        v_tok v1;
        v_option (fun (v1, v2) -> v_wrap v_bool v1; v_ref_type v2) v2;
        ()
  and v_type_arguments x = v_list v_type_argument x

  and v_literal =
    function
    | Bool v1 -> let v1 = v_wrap v_bool v1 in ()
    | Int v1 -> let v1 = v_wrap v_string v1 in ()
    | Float v1 -> let v1 = v_wrap v_string v1 in ()
    | Char v1 -> let v1 = v_wrap v_string v1 in ()
    | String v1 -> let v1 = v_wrap v_string v1 in ()
    | Null v1 -> let v1 = v_tok v1 in ()

  and v_expr (x : expr) =
    let k x = match x with
      | MethodRef (v1, v2, v3, v4) ->
          OCaml.v_either v_expr v_typ v1;
          v_tok v2;
          v_type_arguments v3;
          v_ident v4
      | Ellipsis v1 -> let v1 = v_tok v1 in ()
      | DeepEllipsis v1 -> let v1 = v_bracket v_expr v1 in ()
      | Name v1 -> let v1 = v_name v1 in ()
      | NameOrClassType v1 -> let v1 = v_name_or_class_type v1 in ()
      | Literal v1 -> let v1 = v_literal v1 in ()
      | ClassLiteral v1 -> let v1 = v_typ v1 in ()
      | NewClass (v0, v1, v2, v3) ->
          let v0 = v_tok v0 in
          let v1 = v_typ v1
          and v2 = v_arguments v2
          and v3 = v_option (v_bracket v_decls) v3
          in ()
      | NewArray (v0, v1, v2, v3, v4) ->
          let v0 = v_tok v0 in
          let v1 = v_typ v1
          and v2 = v_list v_expr v2
          and v3 = v_int v3
          and v4 = v_option v_init v4
          in ()
      | NewQualifiedClass (v0, t, v1, v2, v3, v4) ->
          let v0 = v_expr v0 in
          let _ = v_tok t in
          let v1 = v_tok v1
          and v2 = v_typ v2
          and v3 = v_arguments v3
          and v4 = v_option (v_bracket v_decls) v4
          in ()
      | Call (v1, v2) -> let v1 = v_expr v1 and v2 = v_arguments v2 in ()
      | Dot (v1, t, v2) ->
          let v1 = v_expr v1 and t = v_tok t and v2 = v_ident v2 in ()
      | ArrayAccess (v1, v2) -> let v1 = v_expr v1 and v2 = v_bracket v_expr v2 in ()
      | Postfix (v1, v2) -> let v1 = v_expr v1 and v2 = v_incr_decr v2 in ()
      | Prefix (v1, v2) -> let v1 = v_incr_decr v1 and v2 = v_expr v2 in ()
      | Unary (v1, v2) -> let v1 = v_wrap v_arith_op v1 and v2 = v_expr v2 in ()
      | Infix (v1, v2, v3) ->
          let v1 = v_expr v1 and v2 = v_wrap v_arith_op v2 and v3 = v_expr v3 in ()
      | Cast (v1, v2) -> let v1 = v_bracket (v_list v_typ) v1 and v2 = v_expr v2 in ()
      | InstanceOf (v1, v2) -> let v1 = v_expr v1 and v2 = v_ref_type v2 in ()
      | Conditional (v1, v2, v3) ->
          let v1 = v_expr v1 and v2 = v_expr v2 and v3 = v_expr v3 in ()
      | AssignOp (v1, v2, v3) ->
          let v1 = v_expr v1 and v2 = v_wrap v_arith_op v2 and v3 = v_expr v3 in ()
      | Assign (v1, v2, v3) ->
          let v1 = v_expr v1 and v2 = v_tok v2 and v3 = v_expr v3 in ()
      | TypedMetavar(v1, v2) ->
          let v1 = v_ident v1 in
          let v2 = v_typ v2 in
          ()
      | Lambda (v1, t, v2) ->
          let v1 = v_parameters v1 in
          v_tok t;
          let v2 = v_stmt v2 in
          ()
    in
    vin.kexpr (k, all_functions) x

  and v_parameters v = v_list v_parameter_binding v


  and v_parameter_binding =
    function
    | ParamClassic v1 -> let v1 = v_parameter v1 in ()
    | ParamReceiver v1 -> let v1 = v_parameter v1 in ()
    | ParamSpread (v0, v1) -> v_tok v0; let v1 = v_parameter v1 in ()
    | ParamEllipsis v1 -> let v1 = v_tok v1 in ()

  and v_parameter x = v_var x

  and v_ref_type v = v_typ v
  and v_arith_op _v = ()

  and v_arguments v = v_bracket (v_list v_expr) v
  and v_stmt (x : stmt) =
    let k x = match x with
      | EmptyStmt t -> v_tok t
      | Block v1 -> let v1 = v_bracket v_stmts v1 in ()
      | Expr (v1, t) -> let v1 = v_expr v1 in let t = v_info t in ()
      | If (t, v1, v2, v3) ->
          let t = v_info t in
          let v1 = v_expr v1 and v2 = v_stmt v2 and v3 = v_option v_stmt v3 in ()
      | Switch (v0, v1, v2) ->
          let v0 = v_info v0 in
          let v1 = v_expr v1
          and v2 =
            v_list
              (fun (v1, v2) -> let v1 = v_cases v1 and v2 = v_stmts v2 in ()) v2
          in ()
      | While (t, v1, v2) ->
          let t = v_info t in
          let v1 = v_expr v1 and v2 = v_stmt v2 in ()
      | Do (t, v1, v2) ->
          let t = v_info t in
          let v1 = v_stmt v1 and v2 = v_expr v2 in ()
      | For (t, v1, v2) ->
          let t = v_info t in
          let v1 = v_for_control v1 and v2 = v_stmt v2 in ()
      | Break (t, v1) ->
          let t = v_info t in
          let v1 = v_option v_ident v1 in ()
      | Continue (t, v1) ->
          let t = v_info t in
          let v1 = v_option v_ident v1 in ()
      | Return (t, v1) ->
          let t = v_info t in
          let v1 = v_option v_expr v1 in ()
      | Label (v1, v2) -> let v1 = v_ident v1 and v2 = v_stmt v2 in ()
      | Sync (v1, v2) -> let v1 = v_expr v1 and v2 = v_stmt v2 in ()
      | Try (t, v0, v1, v2, v3) ->
          let t = v_info t in
          let v0 = v_option v_resources v0 in
          let v1 = v_stmt v1
          and v2 = v_catches v2
          and v3 = v_option v_tok_and_stmt v3
          in ()
      | Throw (t, v1) ->
          let t = v_info t in
          let v1 = v_expr v1 in ()
      | LocalVar v1 -> let v1 = v_var_with_init v1 in ()
      | DeclStmt v1 -> let v1 = v_decl v1 in ()
      | DirectiveStmt v1 -> let v1 = v_directive v1 in ()
      | Assert (t, v1, v2) ->
          let t = v_info t in
          let v1 = v_expr v1 and v2 = v_option v_expr v2 in ()
    in
    vin.kstmt (k, all_functions) x

  and v_resources x = v_bracket (v_list v_resource) x

  and v_resource = function
    | Common.Left x -> v_var_with_init x
    | Common.Right y -> v_expr y

  and v_directive v =
    match v with
    | Package (v1, v2, v3) ->
        let v1 = v_tok v1 in
        let v2 = v_qualified_ident v2 in
        let v3 = v_tok v3 in
        ()
    | Import (_v1, v2) -> v_import v2
    | ModuleTodo v1 -> v_tok v1

  and v_import = function
    | ImportAll (v1, v2, v3) ->
        v_tok v1; v_qualified_ident v2; v_tok v3
    | ImportFrom (v1, v2, v3) ->
        v_tok v1; v_qualified_ident v2; v_ident v3


  and v_tok_and_stmt (t, v) =
    let t = v_tok t in
    let v = v_stmt v in
    ()

  and v_stmts v = v_list v_stmt v
  and v_case = function
    | Case (t, v1) ->
        let t = v_tok t in
        let v1 = v_expr v1 in ()
    | Default t ->
        let t = v_tok t in
        ()
  and v_cases v = v_list v_case v
  and v_for_control =
    function
    | ForClassic (v1, v2, v3) ->
        let v1 = v_for_init v1
        and v2 = v_list v_expr v2
        and v3 = v_list v_expr v3
        in ()
    | Foreach (v1, v2) -> let v1 = v_var v1 and v2 = v_expr v2 in ()
  and v_for_init =
    function
    | ForInitVars v1 -> let v1 = v_list v_var_with_init v1 in ()
    | ForInitExprs v1 -> let v1 = v_list v_expr v1 in ()
  and v_catch (t, v1, v2) =
    let t = v_tok t in
    let v1 = v_catch_var v1 and v2 = v_stmt v2 in ()
  and v_catches v = v_list v_catch v
  and v_var x =
    let k x = match x with
      | { name = v_v_name; mods = v_v_mods; type_ = v_v_type } ->
          let arg = v_ident v_v_name in
          let arg = v_modifiers v_v_mods in
          let arg = v_option v_typ v_v_type
          in ()
    in
    vin.kvar (k, all_functions) x
  and v_catch_var (v1, v2) =
    let v1 = v_var v1 in
    let v2 = v_list v_typ v2 in
    ()

  and v_var_with_init { f_var = v_f_var; f_init = v_f_init } =
    let arg = v_var v_f_var in let arg = v_option v_init v_f_init in ()
  and v_init (x : init) =
    let k x = match x with
      | ExprInit v1 -> let v1 = v_expr v1 in ()
      | ArrayInit v1 -> let v1 = v_bracket (v_list v_init) v1 in ()
    in
    vin.kinit (k, all_functions) x

  and v_method_decl (x : method_decl) =
    let k x = match x with
        {  m_var = v_m_var;
           m_formals = v_m_formals;
           m_throws = v_m_throws;
           m_body = v_m_body
        } ->  let arg = v_var v_m_var in
          let arg = v_parameters v_m_formals in
          let arg = v_list v_typ v_m_throws in
          let arg = v_stmt v_m_body in ()
    in
    vin.kmethod (k, all_functions) x

  and v_field v =
    let k x = v_var_with_init x in
    vin.kfield (k, all_functions) v


  and
    v_enum_decl {
      en_name = v_en_name;
      en_mods = v_en_mods;
      en_impls = v_en_impls;
      en_body = v_en_body
    } =
    let arg = v_ident v_en_name in
    let arg = v_modifiers v_en_mods in
    let arg = v_list v_ref_type v_en_impls in
    let arg =
      match v_en_body with
      | (v1, v2) ->
          let v1 = v_list v_enum_constant v1 and v2 = v_decls v2 in ()
    in ()
  and v_enum_constant (v1, v2, v3) =
    v_ident v1;
    v_option v_arguments v2;
    v_option v_class_body v3

  and v_class_body v = v_bracket v_decls v

  and v_class_decl (x : class_decl) =
    let k x = match x with
        { cl_name = v_cl_name;
          cl_kind = v_cl_kind;
          cl_tparams = v_cl_tparams;
          cl_mods = v_cl_mods;
          cl_extends = v_cl_extends;
          cl_impls = v_cl_impls;
          cl_body = v_cl_body
        } -> let arg = v_ident v_cl_name in
          let arg = v_class_kind v_cl_kind in
          let arg = v_list v_type_parameter v_cl_tparams in
          let arg = v_modifiers v_cl_mods in
          let arg = v_option v_typ v_cl_extends in
          let arg = v_list v_ref_type v_cl_impls in
          let arg = v_class_body v_cl_body in ()
    in
    vin.kclass (k, all_functions) x

  and v_class_kind (x, t) =
    let v1 = v_tok t in
    match x with
    | ClassRegular | Interface | AtInterface -> ()

  and v_decl x =
    let k x = match x with
      | Class v1 -> let v1 = v_class_decl v1 in ()
      | Method v1 -> let v1 = v_method_decl v1 in ()
      | Field v1 -> let v1 = v_field v1 in ()
      | Enum v1 -> let v1 = v_enum_decl v1 in ()
      | Init (v1, v2) -> let v1 = v_option v_tok v1 and v2 = v_stmt v2 in ()
      | DeclEllipsis v1 -> let v1 = v_tok v1 in ()
      | EmptyDecl t -> v_tok t
      | AnnotationTypeElementTodo t -> v_tok t
    in
    vin.kdecl (k, all_functions) x


  and v_decls v = (v_list v_decl) v

  (* end not-really-auto generation... *)
  and all_functions x = v_any x
  in
  v_any
