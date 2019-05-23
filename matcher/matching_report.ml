(* Yoann Padioleau
 *
 * Copyright (C) 2013 Facebook
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

module PI = Parse_info
module J = Json_type

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* could perhaps create a special file related to display of code ? *)
type match_format =
  (* ex: tests/misc/foo4.php:3
   *  foo(
   *   1,
   *   2);
   *)
  | Normal
  (* ex: tests/misc/foo4.php:3: foo( *)
  | Emacs
  (* ex: tests/misc/foo4.php:3: foo(1,2) *)
  | OneLine
  (* ex: { check_id: ...; path: ...; start: ... end: ...; extra: ... *)
  | Json

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)
(* used only for Json format *)
let first_entry = ref true

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* When we print in the OneLine format we want to normalize the matched
 * expression or code and so only print the tokens in the AST (and not
 * the extra whitespace, newlines or comments). It's not enough though
 * to just List.map str_of_info because some PHP expressions such as
 * '$x = print FOO' would then be transformed into $x=printFOO, hence
 * this function
 *)
let rec join_with_space_if_needed xs = 
  match xs with
  | [] -> ""
  | [x] -> x
  | x::y::xs ->
      if x =~ ".*[a-zA-Z0-9_]$" && 
         y =~ "^[a-zA-Z0-9_]"
      then x ^ " " ^ (join_with_space_if_needed (y::xs))
      else x ^ (join_with_space_if_needed (y::xs))

let info_to_json info = 
  let loc = PI.token_location_of_info info in
  J.Object [
    "line", J.Int loc.PI.line;
    "col", J.Int loc.PI.column;
  ]

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let print_match ?(format = Normal) ii = 
  let (mini, maxi) = PI.min_max_ii_by_pos ii in
  let (file, line) = 
    PI.file_of_info mini, PI.line_of_info mini in
  let prefix = spf "%s:%d" file line in
  let arr = Common2.cat_array file in
  let lines = Common2.enum (PI.line_of_info mini) (PI.line_of_info maxi) in
  
  match format with
  | Normal ->
      pr prefix;
      (* todo? some context too ? *)
      lines +> List.map (fun i -> arr.(i)) +> List.iter (fun s -> pr (" " ^ s));
  | Emacs ->
      pr (prefix ^ ": " ^ arr.(List.hd lines))
  | OneLine ->
      pr (prefix ^ ": " ^ (ii +> List.map PI.str_of_info 
                            +> join_with_space_if_needed))
  | Json ->
      if not !first_entry
      then pr ",";
      first_entry := false;

      let matched_str = ii |> List.map PI.str_of_info 
                            |> join_with_space_if_needed in
      let json = J.Object [
        (* r2c: quite specific to r2c *)
        "check_id", J.String "pfff-parse_js_r2c";
        "path", J.String file;
        "start", info_to_json mini;
        "end", info_to_json maxi;
        "extra", J.Object [
          "matched_str", J.String matched_str;
          (* todo: put metavars content *)
        ];
      ] in
      let s = Json_io.string_of_json json in
      pr s


(*****************************************************************************)
(* Header/Trailer *)
(*****************************************************************************)
let print_header = function
  | Normal | Emacs | OneLine -> ()
  | Json -> 
     pr "{ \"results\": [";
     first_entry := true

let print_trailer = function
  | Normal | Emacs | OneLine -> ()
  | Json -> 
     pr "] }";
     first_entry := false

