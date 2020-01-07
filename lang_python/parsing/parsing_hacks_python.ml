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

module T = Parser_python

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The goal for this module is to add extra "closing" tokens 
 * for the grammar to remain simple.
 *
 * alt: 
 *  - could insert those closing tokens during error recovery
 *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let fix_tokens toks =
  let rec aux xs =
    match xs with
    | [T.NEWLINE _; T.EOF _] -> xs
    | [T.EOF ii] -> [T.NEWLINE ii; T.EOF ii]
    | [] -> raise Common.Impossible
    | x::xs -> x::aux xs
  in
  aux toks

