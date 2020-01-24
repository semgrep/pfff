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

open Ast_go
open Highlight_code
module T = Parser_go
module V = Visitor_go
module E = Entity_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Syntax highlighting for Go code for codemap (and now also efuns)
 *)

(*****************************************************************************)
(* Helpers when have global-analysis information *)
(*****************************************************************************)

(* we generate fake value here because the real one are computed in a
 * later phase in rewrite_categ_using_entities in pfff_visual.
 *)
let def2 = Def2 NoUse
let use2 = Use2 (NoInfoPlace, UniqueDef, MultiUse)


(* coupling: dupe of list in lexer_go.mll comment *)
(* declared in the "universe block"
 *  - true, false
 *  - iota
 *  - new, make, 
 *    panic (CFG effect, like goto), recover,
 *    print, println
 *    complex, imag, real
 *    append, cap, 
 *    close, delete, copy, 
 *    len,
 *  - nil
 *  - _ (blank identifier)
 *)

let _builtin_functions = Common.hashset_of_list [
    "iota";
    "new"; "make";

    "print"; "println";

    "append"; "cap";
    "close"; "delete";"copy";
    "len";
]

(*****************************************************************************)
(* Code highlighter *)
(*****************************************************************************)

(* The idea of the code below is to visit the program either through its
 * AST or its list of tokens. The tokens are easier for tagging keywords,
 * number and basic entities. The AST is better for tagging idents
 * to figure out what kind of ident it is.
 *)

let visit_program ~tag_hook _prefs (program, toks) =
  let already_tagged = Hashtbl.create 101 in
  let tag = (fun ii categ ->
    tag_hook ii categ;
    Hashtbl.replace already_tagged ii true
  )
  in
  let tag_ident (_s, ii) categ = 
    (* so treat the most specific in the enclosing code and then
     * do not fear to write very general case patterns later because
     * the specific will have priority over the general
     * (e.g., a Method use vs a Field use)
     *)
    if not (Hashtbl.mem already_tagged ii)
    then tag ii categ 
  in
  let tag_if_not_tagged ii categ =
   if not (Hashtbl.mem already_tagged ii)    
   then tag ii categ
  in
  let tag_qid xs categ =
    match xs with
    | [] | _::_::_::_ -> raise Common.Impossible
    | [x] -> tag_ident x categ
    | [x;y] -> 
        tag_ident x (Entity (E.Module, use2));
        tag_ident y categ
   in


  (* TODO program |> Common.do_option Resolve_python.resolve;*)

  (* -------------------------------------------------------------------- *)
  (* AST phase 1 *) 
  (* -------------------------------------------------------------------- *)
  (* try to better colorize identifiers which can be many different things
   * e.g. a field, a type, a function, a parameter, etc
   *)
  let in_toplevel = ref true in

  let visitor = V.mk_visitor { V.default_visitor with
    (* use 'k x' as much as possible below. No need to 
     * do v (Stmt st1); v (Expr e); ... Go deep to tag
     * special stuff (e.g., a local var in an exception handler) but then
     * just recurse from the top with 'k x'
     *)

    (* defs *)
    V.kprogram = (fun (k, _) x ->
      tag_ident x.package (Entity (E.Module, def2));
      x.imports |> List.iter (fun import ->
        (* could color import.i_path *)
        match import.i_kind with
        | ImportNamed id -> tag_ident id (Entity (E.Module, def2))
        | ImportOrig | ImportDot _ -> ()
      );
      k x
    );
    V.ktop_decl = (fun (k, _) x ->
      (match x with
      | DFunc (id, _t, _st) -> tag_ident id (Entity (E.Function, def2))
      | DMethod (id, _o, _t, _st) -> tag_ident id (Entity (E.Method, def2))
      | D _ -> ()
      );
     Common.save_excursion in_toplevel false (fun () -> 
       k x
     );
    );
    V.kdecl = (fun (k, _) x ->
      (match x with
      | DTypeDef (id, _) | DTypeAlias (id, _, _) ->
         tag_ident id (Entity (E.Type, def2))
      | DConst (id, _, _) -> tag_ident id (Entity (E.Constant, def2))
      | DVar (id, _, _) -> 
          if !in_toplevel
          then tag_ident id (Entity (E.Global, def2))
          else tag_ident id (Local Def)
      );
      k x
    );
    V.kstmt = (fun (k, _) x ->
      (match x with
      | DShortVars (xs, _, _) ->
         xs |> List.iter (function
           | Id id -> 
              if !in_toplevel 
              then tag_ident id (Entity (E.Global, def2))
              else tag_ident id (Local Def)
           | _ -> ()
         )
       | _ -> ()
      );
      k x
    );


    (* uses *)

    V.ktype = (fun (k, _) x ->
      (match x with
      | TName (["int", ii]) -> tag ii TypeInt
      | TName qid -> tag_qid qid (Entity (E.Type, use2))
      | _ -> () 
      );
      k x
    );
  } in
  visitor (P program);

  (* -------------------------------------------------------------------- *)
  (* tokens phase 1 (list of tokens) *)
  (* -------------------------------------------------------------------- *)
  (* -------------------------------------------------------------------- *)
  (* Tokens phase 2 (individual tokens) *)
  (* -------------------------------------------------------------------- *)

  toks |> List.iter (fun tok -> 
    match tok with

    (* specials *)
    | T.TUnknown ii -> 
       tag ii Error
    | T.EOF _ii -> 
       ()

    (* comments *)
    | T.TComment ii -> 
       tag_if_not_tagged ii Comment
    | T.TCommentSpace _ | T.TCommentNewline _ -> ()

    (* values  *)
    | T.LSTR (_,ii) ->
        tag_if_not_tagged ii String (* can be a Module in an import *)
    | T.LRUNE (_, ii) ->
        tag ii String
    | T.LFLOAT (_,ii) | T.LINT (_,ii) | T.LIMAG (_,ii) ->
        tag ii Number

    (* ident  *)
    | T.LNAME (s, ii) -> 
        (match s with
        | "true" | "false" -> tag_if_not_tagged ii Boolean
        | "nil" -> tag_if_not_tagged ii Null

        | "panic" | "recover" -> tag ii KeywordExn

        (* should have been tagged by the AST visitor *)
        | _ -> 
          ()
          (* TODO: tag_if_not_tagged ii Error *)
        )

    (* keywords  *)
    | T.LFUNC ii | T.LCONST ii | T.LVAR ii | T.LTYPE ii ->
        tag ii Keyword
    | T.LSTRUCT ii | T.LINTERFACE ii
        -> tag ii KeywordObject
    | T.LIF ii | T.LELSE ii 
    | T.LSWITCH ii | T.LCASE ii | T.LDEFAULT ii
        ->
        tag ii KeywordConditional
    | T.LFOR ii | T.LRANGE ii
      -> tag ii KeywordLoop
    | T.LPACKAGE ii  | T.LIMPORT ii
        -> tag ii KeywordModule
    | T.LSELECT ii | T.LGO ii | T.LCHAN ii
        -> tag ii Keyword (* TODO: KeywordComm? *)
    | T.LCONTINUE ii | T.LBREAK ii
    | T.LFALL ii
    | T.LRETURN ii
        -> tag ii Keyword
    | T.LGOTO ii 
        -> tag ii Keyword (* dangerous? *)
    | T.LMAP ii -> 
          tag ii Keyword (* type? *)
    | T.LDEFER ii ->
          tag ii Keyword

    (* symbols *)
    | T.LEQ ii | T.LCOLAS ii ->
        tag ii Punctuation

    | T.LASOP (_, ii) -> tag ii Punctuation

    | T.LBRACE ii | T.LBODY ii
    | T.RBRACE ii
    | T.LBRACKET ii | T.RBRACKET ii
    | T.LPAREN ii | T.RPAREN ii
        -> tag ii Punctuation

    | T.LPLUS ii ->
        tag ii Punctuation
    | T.LMINUS ii ->
        tag ii Punctuation

    | T.LMULT ii | T.LDIV ii
    | T.LPERCENT ii

    | T.LLSH ii | T.LRSH ii
    | T.LINC ii | T.LDEC ii

    | T.LANDAND ii | T.LOROR ii
    | T.LAND ii | T.LPIPE ii | T.LHAT ii | T.LTILDE ii | T.LANDNOT ii

    | T.LBANG ii

    | T.LEQEQ ii | T.LNE ii 
    | T.LLT ii  | T.LGT ii
    | T.LLE ii | T.LGE ii

    | T.LDOT (ii)
    | T.LCOLON (ii)
    | T.LCOMMA ii
    | T.LSEMICOLON ii
    ->
        tag ii Punctuation

    | T.LCOMM ii -> 
          tag ii Punctuation (* keywordComm *)

    | T.LDDD ii-> 
          tag ii Punctuation
  );
  ()
