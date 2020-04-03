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
open Ast_generic
module Ast = Ast_generic
module V = Visitor_ast

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The goal of this module is to resolve names, a.k.a naming or
 * scope resolution, and to do it in a generic way on the generic AST.
 * 
 * In a compiler frontend you often have those phases:
 *  - lexing
 *  - parsing
 *  - naming (the goal of this file)
 *  - typing
 *  
 * The goal of naming is to simplify further phases by having each
 * use of an entity clearly linked to its definition. For example,
 * when you see in the AST the use of the identifier 'a', this 'a'
 * could reference a local variable, or a parameter, or a global, 
 * or a global defined in another module but imported in the current
 * namespace, or a variable defined in a nested block that "shadows" an
 * enclosing variable with the same name.
 * By resolving once and for all all uses of an entity to its definition,
 * for example by renaming some shadow variables (see Ast_generic.gensym),
 * we simpify further phases that don't have to maintain a complex environment 
 * to deal with scoping issues (see the essence Of Python paper 
 * "Python: The Full Monty" where they show that even complex IDEs still
 * don't correctly handle Python scoping rules and perform wrong renaming
 * refactorings).
 *
 * Resolving names by tagging identifiers is also useful for 
 * codemap/efuns to colorize identifiers (locals, params, globals, unknowns)
 * differently.
 * 
 * Note that we also abuse this module to provide advanced features to
 * sgrep such as the constant propagation of literals (which is arguably naming
 * related).
 * 
 * alternatives:
 *  - CURRENT: generic naming and use of a 'ref resolved_name' to annotate
 *    the generic AST. Note that the use of a ref that can be shared with
 *    the lang-specific AST (e.g., ast_go.ml) allows tools like codemap/efuns
 *    to benefit from the generic naming analysis while still caring only
 *    about the lang-specific AST (even though we may want at some point
 *    to have a generic highlighter).
 *  - define a separate type for a named ast, e.g., nast.ml (as done in 
 *    hack/skip) instead of modifying refs, with a unique identifier
 *    for each entity. However, this is tedious to
 *    write as both types are almost identical (maybe a functor could help,
 *    or a generic aast type as in recent hack code). Moreover, this is really
 *    useful for complex global analysis (or at least semi-global as in 
 *    OCaml where you still need to open many .cmi when you locally type a .ml)
 *    such as typing where we want to resolve every use of a global.
 *    For sgrep, where we might for quite some time restrict ourselves to
 *    local analysis, maybe the ref implementation technique is good enough.
 *  - implement a resolve_xxx.ml for each language instead of doing
 *    on the generic AST. That is what I was doing previously, which
 *    has some advantages (some language-specific constructs that introduce
 *    new variables, for example Python comprehensions, are hard to analyze
 *    once converted to the generic AST because they are under an 
 *    Other_xxx category). However, there's potentially lots of code 
 *    duplication for each language and it's easy for a language to fall
 *    behind.
 *    A nice compromise might be to do most of the work in naming_ast.ml
 *    but still have lang-specific resolve_xxx.ml to tag special
 *    constructs that override what naming_ast.ml would do. 
 *    See set_resolved()
 *
 * TODO:
 *  - generalize the original "resolvers":
 *    * resolve_go.ml
 *    * resolve_python.ml
 *    * ast_js_build.ml
 *    * check_variables_cpp.ml
 *    * check_variables_php.ml
 *  - introduce extra VarDef for languages that do not have them like
 *    Python/PHP where the first use is a def (which in turn requires
 *    special construct like 'global' or 'nonlocal' to disable this).
 *  - go:
 *    * handle DShortVars and Foreach local vars, DMethod receiver parameter,
 *      and TypeName for new types
 *    * in theory if/for/switch with their init declare new scope, as well 
 *      as Block
 *    * should do first pass to get all toplevel decl as you can use
 *      forward ref in Go
 *  - python:
 *     * Global/NonLocal directives
 *  - get rid of the original "resolvers":
 *    * resolve_xxx.ml
 *    * ast_js_build.ml
 *    * check_variables_xxx.ml
 *  - get rid of or unify scope_code.ml, scope_php.ml, and 
 *    ast_generic.resolved_name
 *
 * history:
 *  - PHP deadcode detector with global analysis and global code database
 *  - local name resolution for PHP and C/C++ in check_variables_cpp.ml and
 *    check_variables_php.ml for codemap semantic highlighting of identifiers
 *    (mainly local vs params vs globals vs unknown) and for checkModule
 *    (scheck ancestor). Use of a ref for C/C++.
 *  - graph_code_xxx.ml global name resolution for PHP, then Java,
 *    then ML, then ML via cmt, then Clang ASTs, then C, then Javascript
 *  - separate named AST (nast.ml) and naming phase for Hack
 *  - local name resolution for code highlighting for Javascript, then Python
 *    to better colorize identifiers in codemap/efuns, but separate from
 *    a variable checker (resolve_xxx.ml instead of check_variables_xxx.ml)
 *  - AST generic and its resolved_name ref
 *  - simple resolve_python.ml with variable and import resolution
 *  - separate resolve_go.ml  with import resolution
 *  - try to unify those resolvers in one file, naming_ast.ml
 *)

(*****************************************************************************)
(* Scope *)
(*****************************************************************************)

(* this includes the "single unique id" (sid) *)
type resolved_name = Ast_generic.resolved_name

type scope = (string, resolved_name) assoc

type scopes = {
  global: scope ref;
  (* function, nested blocks, nested functions (lambdas) *)
  blocks: scope list ref;
  (* useful for python, kind of global scope but for entities *)
  imported: scope ref;
  (* todo? class? function? (for 'var' in JS) *)
 }

let default_scopes () = {
  global = ref [];
  blocks = ref [];
  imported = ref [];
}

(* because we use a Visitor instead of a clean recursive 
 * function passing down an environment, we need to emulate a scoped
 * environment by using save_excursion.
 *)

let with_new_function_scope params scopes f =
  Common.save_excursion scopes.blocks (params::!(scopes.blocks)) f

let _with_new_block_scope _params _lang _scopes _f =
  raise Todo

let add_ident_current_scope id resolved scopes =
  let s = Ast.str_of_ident id in
  match !(scopes.blocks) with
  | [] -> scopes.global := (s, resolved)::!(scopes.global)
  | xs::xxs -> scopes.blocks := ((s, resolved)::xs)::xxs

(* for Python *)
let add_ident_imported_scope id resolved scopes =
  let s = Ast.str_of_ident id in
  scopes.imported := (s, resolved)::!(scopes.imported)

(* for JS 'var' *)
let _add_ident_function_scope id _resolved _scopes =
  let _s = Ast.str_of_ident id in
  raise Todo

let rec lookup s xxs =
   match xxs with
   | [] -> None
   | xs::xxs ->
      (match List.assoc_opt s xs with
      | None -> lookup s xxs
      | Some res -> Some res
      )

(* accessors *)
let lookup_scope_opt id lang scopes =
  let s = Ast.str_of_ident id in

  let actual_scopes =
    match !(scopes.blocks) with
    | [] -> [!(scopes.global);!(scopes.imported)]
    | xs::xxs -> 
       match lang with
       (* just look current scope! no access to nested scopes or global *)
       | Lang.Python -> 
            [xs;                            !(scopes.imported)]
       | _ ->  
            [xs] @ xxs @ [!(scopes.global); !(scopes.imported)]
  in
  lookup s actual_scopes

(* for Python, PHP? *)
let lookup_global_scope id scopes =
  let s = Ast.str_of_ident id in
  lookup s [!(scopes.global)]

(* for Python, PHP *)
let lookup_nonlocal_scope id scopes =
  let (s, tok) = id in
  match !(scopes.blocks) with
  | _::xxs -> lookup s xxs
  | [] -> 
      let _ = error tok "no outerscope" in
      None

(*****************************************************************************)
(* Environment *)
(*****************************************************************************)
type context = 
  | AtToplevel
  | InClass
  (* separate InMethod? InLambda? just look for InFunction::InClass::_ *)
  | InFunction 

type env = {
  ctx: context list ref;

  (* handle locals/params/globals
   * todo: block vars
   * todo: enclosed vars (closures)
   * todo: use for types for Go
   *)
  names: scopes;

  (* basic constant propagation of literals for sgrep *)
  constants: (string, sid * literal) assoc ref;

  in_lvalue: bool ref;
}

let default_env () = {
  ctx = ref [AtToplevel];
  names = (default_scopes());
  constants = ref [];
  in_lvalue = ref false;
}

(*****************************************************************************)
(* Environment Helpers *)
(*****************************************************************************)
let add_constant_env ident (sid, literal) env =
  env.constants := (Ast.str_of_ident ident, (sid, literal))::!(env.constants)

let with_new_context ctx env f = 
  Common.save_excursion env.ctx (ctx::!(env.ctx)) f

let top_context env = 
  match !(env.ctx) with
  | [] -> raise Impossible
  | x::_xs -> x

let set_resolved id_info x =
  (* TODO? maybe do it only if we have something better than what the
   * lang-specific resolved found?
   *)
  id_info.id_resolved := Some x

(*****************************************************************************)
(* Error manangement *)
(*****************************************************************************)
let error_report = ref false

let error tok s = 
  if !error_report
  then raise (Parse_info.Other_error (s,tok))
  else ()

(*****************************************************************************)
(* Other Helpers *)
(*****************************************************************************)

let is_local_or_global_ctx env =
  match top_context env with
  | AtToplevel | InFunction -> true
  | InClass -> false

let resolved_name_kind env =
  match top_context env with
  | AtToplevel -> Global
  | InFunction -> Local
  | InClass -> raise Impossible

(* !also set the id_info of the parameter as a side effect! *)
let params_of_parameters xs =
 xs |> Common.map_filter (function
  | ParamClassic { pname = Some id; pinfo = id_info; _ } ->
        let sid = Ast.gensym () in
        let resolved = Param, sid in
        set_resolved id_info resolved;
        Some (Ast.str_of_ident id, resolved)
   | _ -> None
  )

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let resolve lang prog =
  let env = default_env () in

  (* would be better to use a classic recursive with environment visit *)
  let visitor = V.mk_visitor { V.default_visitor with
    (* the defs *)

    V.kfunction_definition = (fun (k, _v) x ->
      (* todo: add the function as a Global. In fact we should do a first
       * pass for some languages to add all of them first, because
       * Go for example allow the use of forward function reference
       * (no need to declarare prototype and forward decls as in C).
       *)
      let new_params = params_of_parameters x.fparams in
      with_new_context InFunction env (fun () ->
      with_new_function_scope new_params env.names (fun () ->
        k x
      ))
    );
    V.kclass_definition = (fun (k, _v) x ->
      with_new_context InClass env (fun () ->
        k x
      )
    );
    V.kdef = (fun (k, _v) x ->
      match x with
      | { name = id; info = id_info; attrs = attrs; _}, 
        (* note that some languages such as Python do not have VarDef 
         * construct
         * todo? should add those somewhere instead of in_lvalue detection? *)
        VarDef ({ vinit = vinit; _}) when is_local_or_global_ctx env ->
          (* name resolution *)
          let sid = Ast.gensym () in
          let resolved = resolved_name_kind env, sid in
          add_ident_current_scope id resolved env.names;
          set_resolved id_info resolved;

          (* sgrep: literal constant propagation! *)
          (match vinit with 
          | Some (L literal) when Ast.has_keyword_attr Const attrs && 
                                   is_local_or_global_ctx env ->
              id_info.id_const_literal := Some literal;
              add_constant_env id (sid, literal) env;
          | _ -> ()
          );
           
          k x
      | { name = id; info = id_info; _}, UseOuterDecl tok ->
          let s = Parse_info.str_of_info tok in
          let flookup = 
             match s with
             | "global" -> lookup_global_scope
             | "nonlocal" -> lookup_nonlocal_scope
             | _ -> 
                error tok (spf "unrecognized UseOuterDecl directive: %s" s);
                lookup_global_scope    
          in
          (match flookup id env.names with
          | Some resolved ->
             set_resolved id_info resolved;
             add_ident_current_scope id resolved env.names
          | None ->
             error tok (spf "could not find %s for directive %s" 
                              (Ast.str_of_ident id) s)
          );
          k x
      | _ -> k x
    );

    (* sgrep: the import aliases *)
    V.kdir = (fun (k, _v) x ->
       (match x with
       | ImportFrom (_, DottedName xs, id, Some (alias)) ->
          (* for python *)
          let sid = Ast.gensym () in
          let resolved = ImportedEntity (xs @ [id]), sid in
          add_ident_imported_scope alias resolved env.names;
       | ImportFrom (_, DottedName xs, id, None) ->
          (* for python *)
          let sid = Ast.gensym () in
          let resolved = ImportedEntity (xs @ [id]), sid in
          add_ident_imported_scope id resolved env.names;
       | ImportAs (_, DottedName xs, Some alias) ->
          (* for python *)
          let sid = Ast.gensym () in
          let resolved = ImportedModule (DottedName xs), sid in
          add_ident_imported_scope alias resolved env.names;

       | ImportAs (_, FileName (s, tok), Some alias) ->
          (* for Go *)
          let sid = Ast.gensym () in
          let base = Filename.basename s, tok in
          let resolved = ImportedModule (DottedName [base]), sid in
          add_ident_imported_scope alias resolved env.names;

       | _ -> ()
       );
       k x
    );

    (* the uses *)

    V.kexpr = (fun (k, vout) x ->
       let recurse = ref true in
       (match x with
       | Assign (e1, _, e2) | AssignOp (e1, _, e2) ->
           Common.save_excursion env.in_lvalue true (fun () ->
             vout (E e1);
           );
           vout (E e2);
           recurse := false;

       | Id (id, id_info) ->
          (match lookup_scope_opt id lang env.names with
          | Some (resolved) -> 
             (* name resolution *)
             set_resolved id_info resolved;

             (* sgrep: constant propagation *)
             let (_kind, sid) = resolved in
             let s = Ast.str_of_ident id in
             (match List.assoc_opt s !(env.constants) with
             | Some (sid2, literal) when sid = sid2 ->
                 id_info.id_const_literal := Some literal
             | _ -> ()
             )
          | None ->
             (match !(env.in_lvalue), lang with
             (* first use of a variable can be a VarDef in some languages *)
             | true, Lang.Python (* Ruby? PHP? *) ->
               (* mostly copy-paste of VarDef code *)
               let sid = Ast.gensym () in
               let resolved = resolved_name_kind env, sid in
               add_ident_current_scope id resolved env.names;
               set_resolved id_info resolved;

             (* hopefully the lang-specific resolved may have resolved that *)
             | _ -> 
                let (s, tok) = id in
                error tok (spf "could not find %s in environment" s)
             )
          )
       | _ -> ()
       );
       if !recurse then k x
    );
    V.kattr = (fun (k, _v) x ->
      (match x with
      | NamedAttr (id, id_info, _args) ->
          (match lookup_scope_opt id lang env.names with
          | Some resolved -> 
             (* name resolution *)
             set_resolved id_info resolved;
          | _ -> ()          
          )
      | _ -> ()
      );
      k x
     );

  }
  in
  visitor (Pr prog)
