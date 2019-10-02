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
open Ast_python
module Ast = Ast_python
module V = Visitor_python

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

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

let with_added_env xs env f = 
  let newnames = xs @ !(env.names) in
  Common.save_excursion env.names newnames f

let with_new_context ctx env f = 
  Common.save_excursion env.ctx ctx f

let add_name_env name kind env =
  env.names := (Ast.str_of_name name, kind)::!(env.names)


(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let resolve prog =
  let env = default_env () in
  (* would be better to use a classic recursive with environment visit *)
  let visitor = V.mk_visitor { V.default_visitor with
    V.kexpr = (fun (k, _) x ->
      match x with
      | Name (name, ctx, _typ, resolved) ->
          (match ctx with
          | Load | AugLoad ->
            (* assert resolved = NotResolved *)
            let s = Ast.str_of_name name in
            (match List.assoc_opt s !(env.names) with
            | Some x -> resolved := x
            | None -> ()
            )
          | Store | AugStore ->
            let kind = 
              match !(env.ctx) with
              | AtToplevel -> GlobalVar
              | InClass -> ClassField
              | InFunction -> LocalVar
            in
            resolved := kind;
            env |> add_name_env name kind
            
          | Del -> (* should remove from env *)
             ()
          | Param -> 
            resolved := Parameter;
          );
          k x
      | _ -> k x
    );
    V.kstmt = (fun (k, _) x ->
      match x with
      | FunctionDef (_name, params, _typopt, _body, _decorators) ->
          let new_params = 
             let (args, _varargs, _kwargs, _defaults) = params in
             args |> Common.map_filter (fun arg ->
               match arg with
               | Name (name, _ctx, _typ, _resolved) -> Some name
               | _ -> None
              )
           in
           let new_names = new_params |> List.map (fun name ->
               Ast.str_of_name name, Ast.Parameter
            ) in
            with_added_env new_names env (fun () -> 
              with_new_context InFunction env (fun () ->
               k x              
            ))
     | ClassDef (_name, _bases, _body, _decorators) ->
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

     | _ -> k x
    );  
  } in
  visitor (Program prog)

