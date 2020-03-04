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
 *    See set_if_not_already_set()
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
 *  - use gensym to create unique identifiers for locals/params
 *  - go:
 *    * in theory if/for/switch with their init declare new scope, as well 
 *      as Block
 *    * should do first pass to get all toplevel decl as you can use
 *      forward ref in Go
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
(* Type *)
(*****************************************************************************)
type context = 
  | AtToplevel
  | InClass
  (* separate InMethod? InLambda? just look for InFunction::InClass::_ *)
  | InFunction 

type resolved_name = Ast_generic.resolved_name

type env = {
  ctx: context list ref;
  (* todo: local/param
   * todo: block vars => list list with new_scope/del_scope/lookup
   * todo: enclosed vars (closures)
   * todo: globals
   * todo: use for module aliasing
   * todo: use for types for Go
   *)
  names: (string, resolved_name) assoc ref;
  (* todo: constant propagation for sgrep *)
  constants: (string, sid * literal) assoc ref;
  (* todo?
   * (* block scope *)
   * locals: (string * resolved_name) list ref; 
   * (* javascript function scope *)
   * vars: (string, bool) Hashtbl.t; 
   *)
}

let default_env () = {
  ctx = ref [AtToplevel];
  names = ref [];
  constants = ref [];
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
(* because we use a Visitor instead of a clean recursive 
 * function passing down an environment, we need to emulate a scoped
 * environment by using save_excursion.
 *)
let _with_added_env xs env f = 
  let newnames = xs @ !(env.names) in
  Common.save_excursion env.names newnames f

let add_ident_env ident resolved env =
  env.names := (Ast.str_of_ident ident, resolved)::!(env.names)

let add_constant_env ident (sid, literal) env =
  env.constants := (Ast.str_of_ident ident, (sid, literal))::!(env.constants)

let with_new_context ctx env f = 
  Common.save_excursion env.ctx (ctx::!(env.ctx)) f

let top_context env = 
  match !(env.ctx) with
  | [] -> raise Impossible
  | x::_xs -> x

let is_local_or_global_ctx env =
  match top_context env with
  | AtToplevel | InFunction -> true
  | InClass -> false

let resolved_name_kind env =
  match top_context env with
  | AtToplevel -> Global [] (* TODO *)
  | InFunction -> Local
  | InClass -> raise Impossible

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let resolve _lang prog =
  let env = default_env () in

  (* would be better to use a classic recursive with environment visit *)
  let visitor = V.mk_visitor { V.default_visitor with
    V.kfunction_definition = (fun (k, _v) x ->
      with_new_context InFunction env (fun () ->
        k x
      )
    );
    V.kclass_definition = (fun (k, _v) x ->
      with_new_context InClass env (fun () ->
        k x
      )
    );
    V.kdef = (fun (k, _v) x ->
      match x with
      (* literal constant propagation! *)
      | { name = id; info = id_info; attrs = attrs; _}, 
         VarDef ({ vinit = Some (L literal); _})
         when Ast.has_keyword_attr Const attrs && 
              is_local_or_global_ctx env
         ->
            let sid = Ast.gensym () in
            let resolved = resolved_name_kind env, sid in
            (* TODO? 
             * set_if_not_already_set? id_info resolved;
             *)
            id_info.id_const_literal := Some literal;
            add_ident_env id resolved env;
            add_constant_env id (sid, literal) env;
             
            k x
      | _ -> k x
    );


    V.kexpr = (fun (k, _v) x ->
       (match x with
       | Id (id, id_info) ->
          let s = Ast.str_of_ident id in
          (match List.assoc_opt s !(env.names) with
          | Some ((_kind, sid) as _resolved) -> 
               (* TODO: set_if_not_already_set id_info resolved *)
               (match List.assoc_opt s !(env.constants) with
               | Some (sid2, literal) when sid = sid2 ->
                   id_info.id_const_literal := Some literal
               | _ -> ()
               )
          | None -> () 
          )
       | _ -> ()
       );
       k x
    );

  }
  in
  visitor (Pr prog)
