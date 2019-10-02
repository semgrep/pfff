(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
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
open Common

open Ast_python
open Highlight_code
module T = Parser_python
module V = Visitor_python
module E = Entity_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Syntax highlighting for Python code for codemap (and now also efuns)
 *)

(*****************************************************************************)
(* Helpers when have global-analysis information *)
(*****************************************************************************)

(* we generate fake value here because the real one are computed in a
 * later phase in rewrite_categ_using_entities in pfff_visual.
 *)
let fake_no_def2 = NoUse
let fake_no_use2 = (NoInfoPlace, UniqueDef, MultiUse)


let builtin_functions = Common.hashset_of_list [
  "isinstance";
  "set";
  "dict";
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
  let tag_name (_s, ii) categ = 
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

  let lexer_based_tagger = (program = None) in
  program |> Common.do_option Resolve_python.resolve;

  (* -------------------------------------------------------------------- *)
  (* AST phase 1 *) 
  (* -------------------------------------------------------------------- *)
  (* try to better colorize identifiers which can be many different things
   * e.g. a field, a type, a function, a parameter, etc
   *)
  let in_class = ref false in
  let in_type = ref false in
  let in_decorator = ref false in

  let visitor = V.mk_visitor { V.default_visitor with
    V.kexpr = (fun (k, _) x ->
     match x with
     | Name (name, ctx, _typ, resolved) ->
        (match !resolved with
        | _ when !in_type -> 
          (match fst name with
          | "int" -> tag_name name TypeInt
          | _ ->
            let kind = E.Type in
            tag_name name (Entity (kind, (Use2 fake_no_use2)))
          )
        | _ when !in_decorator -> 
           tag_name name Highlight_code.Attribute
        | Ast_python.Parameter ->
             tag_name name (Highlight_code.Parameter Use)
        | GlobalVar ->
            let usedef = 
              match ctx with
              | Store -> Def2 fake_no_def2
              | Load -> Use2 fake_no_use2
              | _ -> Use2 fake_no_use2 (* TODO *)
            in
            tag_name name (Entity (E.Global, usedef))
        | ClassField ->
            let usedef = 
              match ctx with
              | Store -> Def2 fake_no_def2
              | Load -> Use2 fake_no_use2
              | _ -> Use2 fake_no_use2 (* TODO *)
            in
            tag_name name (Entity (E.Field, usedef))
        | LocalVar ->
            let usedef = 
              match ctx with
              | Store -> Def
              | Load -> Use
              | _ -> Use (* TODO *)
             in
             tag_name name (Local usedef)
        | ImportedEntity ->
            let kind = E.Function in
            tag_name name (Entity (kind, (Use2 fake_no_use2)))
        | ImportedModule ->
            let kind = E.Module in
            tag_name name (Entity (kind, (Use2 fake_no_use2)))
        | NotResolved ->
            (*
            let kind = E.Global in
            tag_name name (Entity (kind, (Use2 fake_no_use2)))
            *)
            ()
        );
        k x
     | Call (f, _args, keywords, _starargs, _kwargs) ->
       (match f with
       | Name (name, _ctx, _typ, _resolved) ->
           let kind = E.Function in
           tag_name name (Entity (kind, Use2 fake_no_use2))
       | Ast_python.Attribute (_e, name, _ctx) ->
           let kind = E.Method in
           tag_name name (Entity (kind, Use2 fake_no_use2))
       | _ -> ()
       );
       keywords |> List.iter (fun (name, _) ->
          tag_name name Comment
       );
       k x
     | Ast_python.Attribute (_e, name, _ctx) ->
        let kind = E.Field in
        tag_name name (Entity (kind, (Use2 fake_no_use2)));
        k x
     | _ -> k x
    );
    V.kstmt = (fun (k, _) x ->
     match x with
     | FunctionDef (name, _params, _typopt, _body, _decorators) ->
       let kind = if !in_class then E.Method else E.Function in
       tag_name name (Entity (kind, (Def2 fake_no_def2)));
       k x
     | ClassDef (name, _bases, _body, _decorators) ->
       let kind = E.Class in
       tag_name name (Entity (kind, (Def2 fake_no_def2)));
       Common.save_excursion in_class true (fun () -> 
          k x);
     | Import (aliases) ->
         aliases |> List.iter (fun (dotted_name, asname_opt) ->
           let kind = E.Module in
           dotted_name |> List.iter (fun name ->
             tag_name name (Entity (kind, (Use2 fake_no_use2)));
           );
           asname_opt |> Common.do_option (fun asname ->
             tag_name asname (Entity (kind, (Def2 fake_no_def2)));
           );
         );
         k x

     | ImportFrom (dotted_name, aliases, _) ->
         let kind = E.Module in
         dotted_name |> List.iter (fun name ->
           tag_name name (Entity (kind, (Use2 fake_no_use2)));
         );
         aliases |> List.iter (fun (name, asname_opt) ->
           let kind = E.Function in
           tag_name name (Entity (kind, (Use2 fake_no_use2)));
           asname_opt |> Common.do_option (fun asname ->
             tag_name asname (Entity (kind, (Def2 fake_no_def2)));
           );
         );
         k x
     | _ -> k x
    );
    V.ktype_ = (fun (k, _) x ->
       Common.save_excursion in_type true (fun () -> 
          k x);
    );
    V.kdecorator = (fun (k, _) x ->
       Common.save_excursion in_decorator true (fun () -> 
          k x);
    );
    V.kparameters = (fun (k, _) x ->
      let (args, varargs, kwargs, _defaults) = x in
      args |> List.iter (fun arg ->
        match arg with
        | Name (name, _ctx, _typ, _resolved) ->
          tag_name name (Parameter Def);
        | _ -> ()
      );
      varargs |> Common.do_option (fun varargs ->
        tag_name varargs (Parameter Def);
      );
      kwargs |> Common.do_option (fun kwargs ->
        tag_name kwargs (Parameter Def);
      );
      k x
    );
  }
  in
  program |> Common.do_option (fun prog ->
    visitor (Program prog);
  );

  (* -------------------------------------------------------------------- *)
  (* tokens phase 1 (list of tokens) *)
  (* -------------------------------------------------------------------- *)
  let rec aux_toks xs = 
    match xs with
    | [] -> ()
    (* a little bit pad specific *)
(*
    |   T.TComment(ii)
      ::T.TCommentNewline (_ii2)
      ::T.TComment(ii3)
      ::T.TCommentNewline (_ii4)
      ::T.TComment(ii5)
      ::xs ->
        let s = Parse_info.str_of_info ii in
        let s5 =  Parse_info.str_of_info ii5 in
        (match () with
        | _ when s =~ ".*\\*\\*\\*\\*" && s5 =~ ".*\\*\\*\\*\\*" ->
          tag ii CommentEstet;
          tag ii5 CommentEstet;
          tag ii3 CommentSection0
        | _ when s =~ ".*------" && s5 =~ ".*------" ->
          tag ii CommentEstet;
          tag ii5 CommentEstet;
          tag ii3 CommentSection1
        | _ when s =~ ".*####" && s5 =~ ".*####" ->
          tag ii CommentEstet;
          tag ii5 CommentEstet;
          tag ii3 CommentSection2
        | _ ->
            ()
        );
        aux_toks xs
*)

    (* poor's man identifier tagger *)

    (* defs *)
    | T.CLASS _ii1::T.NAME (_s, ii2)::xs ->
        if not (Hashtbl.mem already_tagged ii2) && lexer_based_tagger
        then tag ii2 (Entity (E.Class, (Def2 fake_no_def2)));
        aux_toks xs

    | T.DEF _ii1::T.NAME (_s, ii2)::xs ->
        (* todo: actually could be a method if in class scope *)
        if not (Hashtbl.mem already_tagged ii2) && lexer_based_tagger
        then tag ii2 (Entity (E.Function, (Def2 fake_no_def2)));
        aux_toks xs


    (* uses *)

    | T.NAME (_s, ii1)::T.DOT _::T.NAME (_s3, ii3)::T.LPAREN _::xs ->
        if not (Hashtbl.mem already_tagged ii3) && lexer_based_tagger
        then begin 
          tag ii3 (Entity (E.Method, (Use2 fake_no_use2)));
          if not (Hashtbl.mem already_tagged ii1)
          then tag ii1 (Local Use);
        end;
        aux_toks xs

    | T.NAME (s, ii1)::T.LPAREN _::xs ->
        if not (Hashtbl.mem already_tagged ii1) && lexer_based_tagger
        then 
          (if Hashtbl.mem builtin_functions s
          then tag ii1 Builtin
          else tag ii1 (Entity (E.Function, (Use2 fake_no_use2)))
          );
        aux_toks xs

    | T.NAME (_s, ii1)::T.DOT _::T.NAME (s3, ii3)::xs ->
        (match xs with
        | (T.DOT _)::_ ->

            if not (Hashtbl.mem already_tagged ii3) && lexer_based_tagger
            then tag ii3 (Entity (E.Field, (Use2 fake_no_use2)));

            if not (Hashtbl.mem already_tagged ii1)
            then tag ii1 (Local Use);

            aux_toks (T.NAME (s3, ii3)::xs)

        | _ ->
          if not (Hashtbl.mem already_tagged ii3) && lexer_based_tagger
          then begin 
            tag ii3 (Entity (E.Field, (Use2 fake_no_use2)));
            (* TODO *)
            if not (Hashtbl.mem already_tagged ii1)
            then tag ii1 (Local Use);
          end;
            aux_toks xs
        )

    | T.NAME (_s, _ii1)::xs ->
        (*
        if s =~ "[a-z]" then begin
          if not (Hashtbl.mem already_tagged ii1) && lexer_based_tagger
          then tag ii1 (Local (Use));
        end;
        *)
        aux_toks xs
        
        

    | _x::xs ->
        aux_toks xs
  in
  let toks' = toks +> Common.exclude (function
    | T.TCommentSpace _ -> true
    | _ -> false
  )
  in
  aux_toks toks';


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
    | T.INDENT | T.DEDENT -> 
       raise Impossible (* filtered in parse_python.ml with is_special *)

    (* comments *)
    | T.TComment ii -> 
       tag_if_not_tagged ii Comment
    (* in lexer_python.mll comments and space and newlines are sometimes 
     * put together *)
    | T.TCommentSpace ii -> 
       tag_if_not_tagged ii Comment
    | T.NEWLINE ii -> 
       tag_if_not_tagged ii Comment


    (* values  *)
    | T.STR (_s,ii) ->
        tag ii String
    | T.FLOAT (_,ii) | T.INT (_,ii) | T.LONGINT (_,ii) ->
        tag ii Number
    | T.IMAG (_, ii) ->
        tag ii Number
(*
    | T.TLongString (_s,ii) ->
        (* most of the time they are used as documentation strings *)
        tag ii Comment
*)

    (* ident  *)
    | T.NAME (s, ii) -> 
        (match s with
        | "None" -> tag ii Null
        | "True" | "False" -> tag ii Boolean
        | "self" -> tag ii KeywordObject
        | _ -> tag_if_not_tagged ii Error
        )

    (* keywords  *)
    | T.DEF ii | T.LAMBDA ii ->
        tag ii Keyword
    | T.IF ii | T.ELIF ii | T.ELSE ii ->
        tag ii KeywordConditional
    | T.FOR ii | T.WHILE ii
      -> tag ii KeywordLoop
    | T.TRY ii  | T.FINALLY ii | T.RAISE ii| T.EXCEPT ii
      -> tag ii KeywordExn
    | T.CLASS ii
        -> tag ii KeywordObject
    | T.IMPORT ii  | T.AS ii | T.FROM ii
        -> tag ii KeywordModule

    | T.CONTINUE ii | T.BREAK ii
    | T.YIELD ii
    | T.RETURN ii
        -> tag ii Keyword

    | T.IS ii | T.IN ii
    | T.PRINT ii 
    | T.EXEC ii 
    | T.PASS ii
    | T.ASSERT ii
    | T.WITH ii
    | T.DEL ii
    | T.GLOBAL ii
        -> tag ii Keyword

    | T.NOT ii  | T.AND ii | T.OR ii -> 
       tag ii BuiltinBoolean


    (* symbols *)
    | T.EQ ii ->
        tag ii Punctuation

    | T.ADDEQ ii | T.SUBEQ ii | T.MULTEQ ii | T.DIVEQ ii 
    | T.MODEQ ii  | T.POWEQ ii | T.FDIVEQ ii 
    | T.ANDEQ ii | T.OREQ ii | T.XOREQ ii 
    | T.LSHEQ ii | T.RSHEQ ii 
       -> tag ii Punctuation

    | T.LBRACE ii | T.RBRACE ii
    | T.LBRACK ii | T.RBRACK ii
    | T.LPAREN ii | T.RPAREN ii
        -> tag ii Punctuation

    | T.ADD ii ->
        tag ii Punctuation
    | T.SUB ii ->
        tag ii Punctuation

    | T.MULT ii | T.DIV ii
    | T.MOD ii | T.FDIV ii | T.POW ii

    | T.LSHIFT ii | T.RSHIFT ii

    | T.BITXOR ii | T.BITOR ii | T.BITAND ii | T.BITNOT ii

    | T.EQUAL ii | T.NOTEQ ii 
    | T.LT ii  | T.GT ii
    | T.LEQ ii | T.GEQ ii

    | T.DOT (ii)
    | T.COLON (ii)
    | T.COMMA ii
    | T.SEMICOL ii
    | T.BACKQUOTE ii

    | T.AT ii
        ->
        tag ii Punctuation
(*
    | T.TEllipsis ii
        -> tag ii Punctuation
*)
  );
  ()
