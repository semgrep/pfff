(* Yoann Padioleau
 *
 * Copyright (C) 2019 r2c
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *)
open Common

module FT = File_type

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type t = 
  | Python
  | Javascript
  | Java
  | Go
  | C
  | ML

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let list_of_lang = [
      "py", Python;
      "python", Python;
      "js", Javascript;
      "javascript", Javascript;
      "c", C;
      "ml", ML;
      "ocaml", ML;
      "java", Java;
      "go", Go;
      "golang", Go;
    ]

let lang_of_string_map = Common.hash_of_list list_of_lang

let lang_of_string_opt x = Hashtbl.find_opt lang_of_string_map (String.lowercase_ascii x)


let lang_of_filename_opt filename =
 let typ = File_type.file_type_of_file filename in
 match typ with
 | FT.PL (FT.Web (FT.Js)) -> Some Javascript
 | FT.PL (FT.Python) -> Some Python
 | FT.PL (FT.C ("c" | "h" )) -> Some C
 | FT.PL (FT.ML _) -> Some ML
 | FT.PL (FT.Java) -> Some Java
 | FT.PL (FT.Go) -> Some Go
 | _ -> None

let string_of_lang = function
  | Python -> "Python"
  | Javascript -> "Javascript"
  | Java -> "Java"
  | C -> "C"
  | ML -> "ML"
  | Go -> "Golang"


let find_source lang xs = 
  Common.files_of_dir_or_files_no_vcs_nofilter xs 
   |> List.filter (fun filename ->
     lang_of_filename_opt filename =*= Some lang
  ) |> Common.sort

(* this is used by sgrep, so it is probably better to keep the logic 
 * simple and not perform any Skip_code filtering (bento already does that)
 *) 
let files_of_dirs_or_files lang xs =
  let xs = List.map Common.fullpath xs in
  find_source lang xs

