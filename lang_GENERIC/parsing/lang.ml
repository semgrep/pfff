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
  | C
  | ML

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let lang_of_string_opt x = 
  match String.lowercase_ascii x with
  | "py" | "python"     -> Some Python
  | "js" | "javascript" -> Some Javascript
  | "c"  -> Some C
  | "ml" | "ocaml" -> Some ML
  | "java" -> Some Java
  | _ -> None

let lang_of_filename_opt filename =
 let typ = File_type.file_type_of_file filename in
 match typ with
 | FT.PL (FT.Web (FT.Js)) -> Some Javascript
 | FT.PL (FT.Python) -> Some Python
 | FT.PL (FT.C ("c" | "h" )) -> Some C
 | FT.PL (FT.ML _) -> Some ML
 | FT.PL (FT.Java) -> Some Java
 | _ -> None






(* copy-paste: very similar to pfff/find_source.ml *)
let finder lang =
  match lang with
  | Python  -> 
    Lib_parsing_python.find_source_files_of_dir_or_files
  | Javascript  -> 
    Lib_parsing_js.find_source_files_of_dir_or_files ~include_scripts:false
  | C
  | Java
  | ML
   -> raise Todo


let files_of_dirs_or_files lang xs =
  let finder = finder lang in
  let xs = List.map Common.fullpath xs in
  finder xs
 (* |> Skip_code.filter_files_if_skip_list *)



