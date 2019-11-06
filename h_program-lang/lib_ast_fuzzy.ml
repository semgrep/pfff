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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A few helpers function to build Ast_fuzzy tree from a list of tokens.
 * It factorizes the language-independent part of those AST fuzzy builder.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type 'tok hooks = {
  kind: 'tok -> Parse_info.token_kind;
  tokf: 'tok -> Parse_info.info;
}

exception Unclosed of string * Parse_info.info (* starting point *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let char_of_token_kind = function
 | PI.RAngle -> '>'
 | PI.RBracket -> ']'
 | PI.RBrace -> '}'
 | _ -> raise (Impossible)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(*
 * less: I should also factorize with Parse_cpp.parse_fuzzy. 
 * put here also generic parts of  token_views_of_xxx?
 * 
 * less: check that it's consistent with the indentation? 
 * less: more fault tolerance? if col == 0 and { then reset?
 *)

let mk_trees h xs =

 (* filter comment tokens *)
  let xs = xs +> Common.exclude (fun t ->
      let kind = h.kind t in
      match kind with
      | PI.Esthet _ | PI.Eof -> true
      | _ -> false
  )
  in

  let rec consume x xs =
    match x with
    | tok when h.kind tok = PI.LBrace -> 
        let body, closing, rest = look_close PI.RBrace x [] xs in
        Ast_fuzzy.Braces (h.tokf x, body, h.tokf closing), rest
    | tok when h.kind tok = PI.LBracket -> 
        let body, closing, rest = look_close PI.RBracket x [] xs in
        Ast_fuzzy.Bracket (h.tokf x, body, h.tokf closing), rest
    | tok when h.kind tok = PI.LAngle -> 
        let body, closing, rest = look_close PI.RAngle x [] xs in
        Ast_fuzzy.Angle (h.tokf x, body, h.tokf closing), rest
    | tok when h.kind tok = PI.LPar ->
        let body, closing, rest = look_close_paren x [] xs in
        let body' = split_comma body in
        Ast_fuzzy.Parens (h.tokf x, body', h.tokf closing), rest
    | tok -> 
      Ast_fuzzy.Tok (PI.str_of_info (h.tokf tok), h.tokf x), xs
(*
    (match Ast.str_of_info (tokext tok) with
    | "..." -> Ast_fuzzy.Dots (tokext tok)
    | s when Ast_fuzzy.is_metavar s -> Ast_fuzzy.Metavar (s, tokext tok)
    | s -> Ast_fuzzy.Tok (s, tokext tok)
*)
  
  and aux xs =
  match xs with
  | [] -> []
  | x::xs ->
      let x', xs' = consume x xs in
      x'::aux xs'

  and look_close close_kind tok_start accbody xs = 
    match xs with
    | [] -> 
        raise (Unclosed (spf "look_close '%c'"
                         (char_of_token_kind close_kind),
                         h.tokf tok_start))

    | x::xs -> 
        (match x with
        | tok when h.kind tok = close_kind -> 
          List.rev accbody, x, xs
        | _ -> let (x', xs') = consume x xs in
               look_close close_kind tok_start (x'::accbody) xs'
        )

  (* todo? diff with look_close PI.RPar ? *)
  and look_close_paren tok_start accbody xs =
    match xs with
    | [] -> 
        raise (Unclosed ("look_close_paren", h.tokf tok_start))
    | x::xs -> 
        (match x with
        | tok when h.kind tok = PI.RPar -> 
            List.rev accbody, x, xs
        | _ -> 
            let (x', xs') = consume x xs in
            look_close_paren tok_start (x'::accbody) xs'
        )

  and split_comma xs =
     let rec aux acc xs =
       match xs with
       | [] ->
         if null acc
         then []
         else [Left (acc +> List.rev)]
       | x::xs ->
         (match x with
         | Ast_fuzzy.Tok (",", info) ->
           let before = acc +> List.rev in
           if null before
           then aux [] xs
           else (Left before)::(Right (info))::aux [] xs
         | _ ->
           aux (x::acc) xs
         )
     in
     aux [] xs
  in
  aux xs


(* 

(*****************************************************************************)
(* Fuzzy parsing *)
(*****************************************************************************)

PHP

(* for generalized sgrep/spatch patterns *)
val parse_fuzzy:
  Common.filename -> Ast_fuzzy.tree list * Parser_php.token list


let parse_fuzzy file =
  let toks = tokens file in
  let trees = Parse_fuzzy.mk_trees { Parse_fuzzy.
     tokf = TH.info_of_tok;
     kind = TH.token_kind_of_tok;
  } toks 
  in
  trees, toks
     parse_fuzzy.ml \

ML
(*****************************************************************************)
(* Fuzzy parsing *)
(*****************************************************************************)

(* This is similar to what I did for OPA. This is also similar
 * to what I do for parsing hacks for C++, but this fuzzy AST can be useful
 * on its own, e.g. for a not too bad sgrep/spatch.
 *)
let parse_fuzzy file =
  let toks = tokens file in
  let trees = Parse_fuzzy.mk_trees { Parse_fuzzy.
     tokf = TH.info_of_tok;
     kind = TH.token_kind_of_tok;
  } toks 
  in
  trees, toks




let test_parse_ml_fuzzy dir_or_file =
  let fullxs = 
    Lib_parsing_ml.find_source_files_of_dir_or_files [dir_or_file] 
    +> Skip_code.filter_files_if_skip_list
  in
  fullxs +> Console.progress (fun k -> List.iter (fun file -> 
     k ();
      try 
        let _fuzzy = Parse_ml.parse_fuzzy file in
        ()
      with _exn ->
        (* pr2 (spf "PB with: %s, exn = %s" file (Common.exn_to_s exn)); *)
        pr2 file;
  ));
  ()

let test_dump_ml_fuzzy file =
  let fuzzy, _toks = Parse_ml.parse_fuzzy file in
  let v = Ast_fuzzy.vof_trees fuzzy in
  let s = Ocaml.string_of_v v in
  pr2 s

  "-parse_ml_fuzzy", "   <file or dir>", 
  Common.mk_action_1_arg test_parse_ml_fuzzy;
  "-dump_ml_fuzzy", "   <file>", 
  Common.mk_action_1_arg test_dump_ml_fuzzy;


SKIP
(*****************************************************************************)
(* Fuzzy parsing *)
(*****************************************************************************)

val parse_fuzzy:
  Common.filename -> Ast_fuzzy.tree list * Parser_skip.token list

let parse_fuzzy file =
  let toks = tokens file in
  let trees = Parse_fuzzy.mk_trees { Parse_fuzzy.
     tokf = TH.info_of_tok;
     kind = TH.token_kind_of_tok;
  } toks 
  in
  trees, toks



let test_parse_fuzzy dir_or_file =
  let fullxs = 
    Lib_parsing_skip.find_source_files_of_dir_or_files [dir_or_file] 
    +> Skip_code.filter_files_if_skip_list
  in
  fullxs +> Console.progress (fun k -> List.iter (fun file -> 
     k ();
      try 
        let _fuzzy = Parse_skip.parse_fuzzy file in
        ()
      with _exn ->
        (* pr2 (spf "PB with: %s, exn = %s" file (Common.exn_to_s exn)); *)
        pr2 file;
  ));
  ()

let test_dump_fuzzy file =
  let fuzzy, _toks = Parse_skip.parse_fuzzy file in
  let v = Ast_fuzzy.vof_trees fuzzy in
  let s = Ocaml.string_of_v v in
  pr2 s


  "-parse_sk_fuzzy", "   <file or dir>", 
  Common.mk_action_1_arg test_parse_fuzzy;
  "-dump_sk_fuzzy", "   <file>", 
  Common.mk_action_1_arg test_dump_fuzzy;

JAVA

(*****************************************************************************)
(* Fuzzy parsing *)
(*****************************************************************************)

(* for generalized sgrep/spatch patterns *)
val parse_fuzzy:
  Common.filename -> Ast_fuzzy.tree list * Parser_java.token list

let parse_fuzzy file =
  let toks = tokens file in
  let trees = Parse_fuzzy.mk_trees { Parse_fuzzy.
     tokf = TH.info_of_tok;
     kind = TH.token_kind_of_tok;
  } toks
  in
  trees, toks


*)
