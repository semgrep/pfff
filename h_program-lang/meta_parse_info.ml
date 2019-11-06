(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
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
open Parse_info

type dumper_precision = {
  full_info: bool;
  token_info: bool;
  type_info: bool;
}
let default_dumper_precision = {
  full_info = false;
  token_info = false;
  type_info = false;
}

let vof_filename v = Ocaml.vof_string v

let vof_token_location {
                     str = v_str;
                     charpos = v_charpos;
                     line = v_line;
                     column = v_column;
                     file = v_file
                   } =
  let bnds = [] in
  let arg = vof_filename v_file in
  let bnd = ("file", arg) in
  let bnds = bnd :: bnds in
  let arg = Ocaml.vof_int v_column in
  let bnd = ("column", arg) in
  let bnds = bnd :: bnds in
  let arg = Ocaml.vof_int v_line in
  let bnd = ("line", arg) in
  let bnds = bnd :: bnds in
  let arg = Ocaml.vof_int v_charpos in
  let bnd = ("charpos", arg) in
  let bnds = bnd :: bnds in
  let arg = Ocaml.vof_string v_str in
  let bnd = ("str", arg) in let bnds = bnd :: bnds in Ocaml.VDict bnds


let vof_token_origin =
  function
  | OriginTok v1 ->
      let v1 = vof_token_location v1 in Ocaml.VSum (("OriginTok", [ v1 ]))
  | FakeTokStr (v1, opt) ->
      let v1 = Ocaml.vof_string v1 in
      let opt = Ocaml.vof_option (fun (p1, i) ->
        Ocaml.VTuple [vof_token_location p1; Ocaml.vof_int i]
      ) opt
      in
      Ocaml.VSum (("FakeTokStr", [ v1; opt ]))
  | Ab -> Ocaml.VSum (("Ab", []))
  | ExpandedTok (v1, v2, v3) ->
      let v1 = vof_token_location v1 in
      let v2 = vof_token_location v2 in
      let v3 = Ocaml.vof_int v3 in
      Ocaml.VSum (("ExpandedTok", [ v1; v2; v3 ]))


let rec vof_transformation =
  function
  | NoTransfo -> Ocaml.VSum (("NoTransfo", []))
  | Remove -> Ocaml.VSum (("Remove", []))
  | AddBefore v1 -> let v1 = vof_add v1 in Ocaml.VSum (("AddBefore", [ v1 ]))
  | AddAfter v1 -> let v1 = vof_add v1 in Ocaml.VSum (("AddAfter", [ v1 ]))
  | Replace v1 -> let v1 = vof_add v1 in Ocaml.VSum (("Replace", [ v1 ]))
  | AddArgsBefore v1 -> let v1 = Ocaml.vof_list Ocaml.vof_string v1 in Ocaml.VSum
  (("AddArgsBefore", [ v1 ]))

and vof_add =
  function
  | AddStr v1 ->
      let v1 = Ocaml.vof_string v1 in Ocaml.VSum (("AddStr", [ v1 ]))
  | AddNewlineAndIdent -> Ocaml.VSum (("AddNewlineAndIdent", []))

let vof_info
 { token = v_token; transfo = v_transfo } =
  let bnds = [] in
  let arg = vof_transformation v_transfo in
  let bnd = ("transfo", arg) in
  let bnds = bnd :: bnds in
  let arg = vof_token_origin v_token in
  let bnd = ("token", arg) in 
  let bnds = bnd :: bnds in 
  Ocaml.VDict bnds


(* todo? could also do via a post processing phase with a OCaml.map_v ? *)
let _current_precision = ref default_dumper_precision

let vof_info_adjustable_precision x =
  if !_current_precision.full_info
  then vof_info x
  else if !_current_precision.token_info
       then 
        Ocaml.VDict [
          "line", Ocaml.VInt (line_of_info x);
          "col", Ocaml.VInt (col_of_info x);
        ]
      else Ocaml.VUnit

