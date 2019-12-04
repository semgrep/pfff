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

open Ast_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Try to generate a normalized form for the program so we need to handle
 * less variations in sgrep, which in turn will allow one pattern
 * to match more equivalent code.
 *
 * TODO: 
 *  - rewrite also IncrDecr to AssignOp
 *  - use IL instead and go even further in the normalization process?
 *)


(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let normalize2 any =
  let visitor = Map_ast.mk_visitor { Map_ast.default_visitor with Map_ast.
    kexpr = (fun (k, _) e ->
      (* apply on children *)
      let e = k e in
      match e with
      | Call (IdSpecial (ArithOp op, tok), [a; b]) ->
        let rewrite_opt =
          match op with
          | NotEq -> Some (Not, Eq)
          | _ -> None
        in
        (match rewrite_opt with
        | None -> e
        | Some (not_op, other_op) ->
          Call (IdSpecial (ArithOp not_op, tok),
                [Arg (Call (IdSpecial (ArithOp other_op, tok), [a;b]))])
        )
      | _ -> e
    )
  } in
  visitor.Map_ast.vany any

let normalize a = 
  Common.profile_code "Normalize_ast.normalize" (fun () -> normalize2 a)
