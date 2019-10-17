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
open Ast_python
module Ast = Ast_python
module V = Visitor_python

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Identifiers tagger (so we can colorize them differently in codemap/efuns).
 *
 * See also Ast_python.resolved_name.
 *)

(*****************************************************************************)
(* Type *)
(*****************************************************************************)
type context = 
  | AtToplevel
  | InClass
  | InFunction
 
type env = {
  ctx: context ref;
  names: (string * resolved_name) list ref;
}

let default_env () = {
  ctx = ref AtToplevel;
  names = ref [];
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* because we use Visitor_python instead of a clean recursive 
 * function passing down an environment, we need to emulate a scoped
 * environment by using save_excursion.
 *)
let with_added_env xs env f = 
  let newnames = xs @ !(env.names) in
  Common.save_excursion env.names newnames f

let add_name_env name kind env =
  env.names := (Ast.str_of_name name, kind)::!(env.names)

let with_new_context ctx env f = 
  Common.save_excursion env.ctx ctx f


let params_of_parameters params =
  let (args, _varargs, kwargs) = params in
  (args |> Common.map_filter (fun (arg, _optval) ->
      match arg with
      | Name (name, _ctx, _typ, _resolved) -> Some name
      (* todo: tuples? *)
      | _ -> None
     )) @ 
  (match kwargs with Some (name, _t) -> [name] | None -> [])

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let resolve prog =
  let env = default_env () in
  (* would be better to use a classic recursive with environment visit *)
  let visitor = V.mk_visitor { V.default_visitor with
    (* No need to resolve at the definition sites (for parameters, locals).
     * This will be patterned-match specially anyway in the highlighter. What
     * you want is to tag the use sites, and to maintain the right environment.
     *)
    V.kexpr = (fun (k, v) x ->
      match x with
      | Name (name, ctx, _typ, resolved) ->
          (match ctx with
          | Load | AugLoad ->
            (* assert resolved = NotResolved *)
            let s = Ast.str_of_name name in
            (match List.assoc_opt s !(env.names) with
            | Some x -> resolved := x
            | None -> () (* will be tagged as Error by highlighter later *)
            )
          | Store | AugStore ->
            let kind = 
              match !(env.ctx) with
              | AtToplevel -> GlobalVar
              | InClass -> ClassField
              | InFunction -> LocalVar
            in
            env |> add_name_env name kind;
            resolved := kind; (* optional *)
            
          | Del -> (* should remove from env *)
             ()
          | Param -> 
            resolved := Parameter; (* optional *)
          );
          k x
      | ListComp (e, xs) | GeneratorExp (e, xs) ->
        let new_vars = 
          xs |> Common.map_filter (fun (target, _iter, _ifs) ->
            match target with
            | Name (name, _ctx, _typ, _res) -> Some name
            (* tuples? *)
            | _ -> None
           ) in
        let new_names = new_vars |> List.map (fun name ->
            Ast.str_of_name name, Ast.LocalVar
        ) in
        with_added_env new_names env (fun () ->
          v (Expr e);
        );
        xs |> List.iter (fun (target, iter, ifs) ->
           v (Expr target);
           v (Expr iter);
           ifs |> List.iter (fun if_ -> v (Expr if_))
        );
        
      (* general case *)
      | _ -> k x
    );
    V.kstmt = (fun (k, v) x ->
      match x with
      | FunctionDef (name, params, _typopt, _body, _decorators) ->
          let new_params = params_of_parameters (params: parameters) in
          let new_names = new_params |> List.map (fun name ->
               Ast.str_of_name name, Ast.Parameter
          ) in
          with_added_env new_names env (fun () -> 
            with_new_context InFunction env (fun () ->
               k x              
          ));
         (* nested function *)
         if !(env.ctx) = InFunction
         then env |> add_name_env name (LocalVar);
     | ClassDef (name, _bases, _body, _decorators) ->
           env |> add_name_env name ImportedEntity; (* could be more precise *)
           with_new_context InClass env (fun () ->
               k x              
            )
    
     | Import (aliases) ->
         aliases |> List.iter (fun (dotted_name, asname_opt) ->
           (match dotted_name with
           | [name] -> env |> add_name_env name ImportedModule
           | _ -> (* TODO *) ()
           );
           asname_opt |> Common.do_option (fun asname ->
             env |> add_name_env asname ImportedModule
           );
         );
         k x

     | ImportFrom (_dotted_name, aliases, _) ->
         aliases |> List.iter (fun (name, asname_opt) ->
           env |> add_name_env name ImportedEntity;
           asname_opt |> Common.do_option (fun asname ->
             env |> add_name_env asname ImportedEntity
           );
         );
         k x
     | With (e, eopt, stmts) ->
       v (Expr e);
       (match eopt with
       | None -> v (Stmts stmts)
       | Some (Name (name, _ctx, _typ, _res)) ->
          (* the scope of name is valid only inside the body, but the
           * body may define variables that are used then outside the with
           * so simpler to use add_name_env() here, not with_add_env()
          let new_names = (fst name, LocalVar)::!(env.names) in
          with_added_env new_names env (fun () ->
            v (Stmts stmts)
          )
           *)
          env |> add_name_env name LocalVar;
          v (Stmts stmts);
       (* todo: tuples? *)
       | Some e -> 
           v (Expr e);
           v (Stmts stmts)
       )
     | TryExcept (stmts1, excepts, stmts2) ->
       v (Stmts stmts1);
       excepts |> List.iter (fun (ExceptHandler (_typ, e, body)) ->
         match e with
         | None -> v (Stmts body)
         | Some (Name (name, _ctx, _typ, _res)) ->
           let new_names = (fst name, LocalVar)::!(env.names) in
           with_added_env new_names env (fun () ->
             v (Stmts body)
           )
         (* tuples? *)
         | Some e -> 
            v (Expr e);
            v (Stmts body)
       );
       v (Stmts stmts2);
     
     (* general case *)
     | _ -> k x
    );  
  } in
  visitor (Program prog)

