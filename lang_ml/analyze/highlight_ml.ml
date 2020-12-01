(* Yoann Padioleau
 *
 * Copyright (C) 2010, 2013 Facebook
 * Copyright (C) 2020 R2C
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

open Highlight_code
module PI = Parse_info
module T = Parser_ml
open Entity_code
module E = Entity_code
module G = AST_generic
module V = Visitor_AST

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Syntax highlighting for OCaml code for codemap (and now also for efuns).
 *
 * This code can also be abused to generate the light database
 * and the TAGS file (because codemap needs to know about
 * def and use of entities), but you should now prefer to
 * base such analysis on graph_code_cmt.ml instead of this file.
 *)

(*****************************************************************************)
(* Helpers when have global analysis information *)
(*****************************************************************************)

(* pad-specific: see my ~/.emacs *)
let h_pervasives_pad = Common.hashset_of_list [
  "pr2";"pr";"pr2_gen";
  "sprintf";"i_to_s";
  "pp2";"spf";
  "log";"log2";"log3"
]

let h_builtin_modules = Common.hashset_of_list [
  "Pervasives"; "Common";
  "List"; "Hashtbl"; "Array"; "Stack";
  "String"; "Bytes"; "Str";
  "Sys"; "Unix"; "Gc";
  "Filename";
]

let h_builtin_bool = Common.hashset_of_list [
  "not";
  "exists"; "forall";
]


let fake_no_def2 = NoUse
let fake_no_use2 = (NoInfoPlace, UniqueDef, MultiUse)

(* set to true when want to debug the AST-based tagger *)
let disable_token_phase2 = false

(*****************************************************************************)
(* AST helpers *)
(*****************************************************************************)

let kind_of_body x =
  let def2 = Def2 fake_no_def2 in
  match x with
  | G.Lambda _ -> Entity (Function, def2)
  | G.Call (G.IdQualified ((("ref", _), _nameinfo), _idinfo), _args) ->
     Entity (Global, def2)
  | G.Call (G.IdQualified ((("create", _),
          { G.name_qualifier = Some (G.QDots ["Hashtbl", _]); _ }), _idinfo),
          _args) ->
      Entity (Global, def2)
  | _ -> Entity (Constant, def2)

(* todo: actually it can be a typedef alias to a function too
 * but this would require some analysis
 *)
let kind_of_ty ty =
  let def2 = Def2 fake_no_def2 in
  match ty with
  | G.TyFun _ -> (FunctionDecl NoUse)
  | G.TyNameApply ((("ref", _), _), _) -> Entity (Global, def2)
  (* todo: should handle module aliases there too *)
  | G.TyNameApply ((("t", _),
         { G.name_qualifier = Some (G.QDots ["Hashtbl", _]); _ }), _)->
      Entity (Global, def2)
  | _ -> Entity (Constant, def2)

let last_id xs =
  match List.rev xs with
  | x::_xs -> x
  | [] -> failwith "last_id: empty list of idents"

let info_of_name ((_s, info), _nameinfo) = info
let info_of_id (_s, info) = info

(*****************************************************************************)
(* Code highlighter *)
(*****************************************************************************)

(* The idea of the code below is to visit the program either through its
 * AST or its list of tokens. The tokens are easier for tagging keywords,
 * number and basic entities. The AST is better for tagging idents
 * to figure out what kind of ident it is.
 *)
let visit_program
 ?(lexer_based_tagger=false)
 ~tag_hook _prefs  (*db_opt *) (ast, toks) =

  let already_tagged = Hashtbl.create 101 in
  let tag = (fun ii categ ->
    tag_hook ii categ;
    Hashtbl.add already_tagged ii true
  )
  in

  (* -------------------------------------------------------------------- *)
  (* AST phase 1 *)
  (* -------------------------------------------------------------------- *)
  let gen = Ml_to_generic.program ast in
  (* TODO: move this in h_program-lang/? to factorize code! *)

  (* try to better colorize identifiers which can be many different things
   * e.g. a field, a type, a function, a parameter, etc
   *)
  let in_let = ref false in
  let in_try_with = ref false in

  let v = V.mk_visitor { V.default_visitor with

    V.kdef = (fun (k, _) x ->
     match x with
     | ({ G.name = G.EId (_s, info); _}, def) ->
      (match def with
      | G.Signature ty ->
          tag info (kind_of_ty ty);
          k x

      | G.ModuleDef { G.mbody = body } ->
          tag info (Entity (E.Module, Def2 fake_no_def2));
          (match body with
          | G.ModuleAlias name ->
            let info = info_of_name name in
            tag info (Entity (Module, Use2 fake_no_use2));
          | _ -> ()
          );
          k x

      | G.TypeDef { G.tbody = G.Exception _ } ->
          tag info (Entity (E.Exception, Def2 fake_no_def2));
          k x
      | G.TypeDef { G.tbody = kind } ->
          tag info (Entity (E.Type, Def2 fake_no_def2));
          (* todo: ty_params *)
          (match kind with
          | G.OrType xs ->
            xs |> List.iter (function
              | G.OrConstructor (id, _) ->
                 let info = info_of_id id in
                 tag info (Entity (Constructor, Def2 fake_no_def2))
              | _ -> ()
            )
          | G.AndType (_, xs, _) ->
            xs |> List.iter (function
              | G.FieldStmt (G.DefStmt ({ G.name = G.EId (_id, info); _}, _))->
                tag info (Entity (Field, (Def2 fake_no_def2)));
              | _ ->  ()
            );
          | _ -> ()
          );
          k x

       | G.VarDef { G.vinit = Some body; vtype = _ }  ->
          (if not !in_let
           then tag info (kind_of_body body)
           else tag info (Local Def)
          );
          Common.save_excursion in_let true (fun () ->
            k x
          )

       | G.FuncDef _ ->
          (if not !in_let
          then tag info (Entity (Function, (Def2 NoUse)))
          else tag info (Local Def)
          );
          Common.save_excursion in_let true (fun () ->
            k x
          )

      | _ -> k x
      )
     | _ -> k x
    );

    V.kdir = (fun (k, _) x ->
     (match x with
     | G.ImportAll (_, G.DottedName xs, _) ->
        let info = snd (last_id xs) in
        tag info (Entity (Module, Use2 fake_no_use2))
     | _-> ()
     );
     k x
    );

    V.kname = (fun (k, _) x ->
      let (_id, infos) = x in
      (match infos.G.name_qualifier with
      | Some (G.QDots xs) ->
          xs |> List.iter (fun (_, ii) ->
           tag ii (Entity (Module, Use2 fake_no_use2))
          )
      | _ -> ()
      );
      k x
    );

    V.kparam = (fun (k, _) x ->
      (match x with
      | G.ParamPattern (G.PatId (id, _idinfo)) ->
        let info = info_of_id id in
        tag info (Parameter Def);
      | _ -> ()
      );
      k x
    );

(*
    V.kargument = (fun (k, _) x ->
      (match x with
      | ArgImplicitTildeExpr (_, name) ->
        let info = Ast.info_of_name name in
        (* todo: could be a Parameter use, need scope analysis *)
        tag info (Local Use)
      | _ -> ()
      );
      k x
    );

      | LetPattern (pat, _tok, body) ->
          (match pat with
          | PatTyped (_, PatVar name, _, _ty, _) ->
              let info = Ast.info_of_name name in
              if not !in_let
              then tag info (kind_of_body body)
              else tag info (Local (Def))
          | _ -> ()
          );
          Common.save_excursion in_let true (fun () ->
            k x
          )
    );
*)

    V.kstmt = (fun (k, _) x ->
      match x with
      | G.Try (_try_tok, _e (*, tok_with*), _match_cases, _finally) ->
          (*tag tok_with (KeywordExn); *)
          (*k (Try (try_tok, e, tok_with, []));*)
          Common.save_excursion in_try_with true (fun () ->
            k x
          )
       | _ -> k x
    );

    V.kexpr = (fun (k, _) x ->
      match x with
      | G.IdQualified (name, _idinfo) ->
          let info = info_of_name name in
          (* could have been tagged as a function name in the rule below *)
          if not (Hashtbl.mem already_tagged info)
          then begin
            (* TODO could be a param, could be a local. Need scope analysis
             * TODO could also be actually a func passed to a higher
             *  order function, as in List.map snd, or even x +> Common.sort
             *)
            tag info (Local Use)
          end;
          k x

      | G.Call (G.IdQualified ((("=~", _), _nameinfo), _idinfo),
                (_, [_arg1; G.Arg (G.L (G.String (_, info)))], _)) ->
          tag info Regexp;
          k x

      | G.Call (G.IdQualified ((id, {G.name_qualifier = qu; _}), _idinfo),
                _args) ->
          let (s, info) = id in
          (match qu with
          | _ when s = "ref" -> tag info UseOfRef
          | Some (G.QDots [s2, info2]) when Hashtbl.mem h_builtin_modules s2 ->
              tag info2 BuiltinCommentColor;
              tag info Builtin;
          | _ ->
              tag info (Entity (Function, (Use2 fake_no_use2)));
          );
          k x

      (* disambiguate "with" which can be used for match, try, or record *)
      | G.MatchPattern (_e1, (*tok_with,*) _match_cases) ->
          (*tag tok_with (KeywordConditional); *)
          k x

      | G.DotAccess (_e, tok, (G.EId id | G.EName (id, _))) ->
          let info = snd id in
          (match PI.str_of_info tok with
          | "#" -> tag info (Entity (Method, (Use2 fake_no_use2)))
          | _ -> tag info (Entity (Field, (Use2 fake_no_use2)))
          );
          k x
      | G.Constructor (name, _eopt) ->
          let info = info_of_name name in
          tag info (Entity (Constructor,(Use2 fake_no_use2)));
          k x

      | G.Record (_, xs, _) ->
          xs |> List.iter (fun x ->
            match x with
            | G.FieldStmt (G.DefStmt ({ G.name = G.EId (_, info); _}, _)) ->
               tag info (Entity (Field, (Use2 fake_no_use2)));
            | _ -> ()
          );
          k x
      (* coupling: with how record with qualified name in ml_to_generic.ml *)
      | G.OtherExpr (G.OE_RecordFieldName, (G.N name)::_) ->
          let info = info_of_name name in
          tag info (Entity (Field, (Use2 fake_no_use2)));
          k x

      | _ -> k x
    );

    V.kpattern = (fun (k, _) x ->
      (match x with
      | G.PatConstructor ((id, _name_info), _popt) ->
          let info = snd id in
          if !in_try_with
          then tag info (KeywordExn)
          else tag info (ConstructorMatch fake_no_use2)
      | G.PatId (id, _idinfo) ->
          let info = info_of_id id in
          tag info (Parameter Def)
      | G.PatRecord (_, xs, _) ->
           xs |> List.iter (fun (name, _pat) ->
             let info = info_of_name name in
             tag info (Entity (Field, (Use2 fake_no_use2)));
           )

      | _ -> ()
      );
      k x
    );

    V.ktype_ = (fun (k, _) t ->
      (match t with
      | G.TyName name ->
          let info = info_of_name name in
          tag info (Entity (Type, (Use2 fake_no_use2)))
      | G.TyNameApply (name, _ty_args) ->
          let info = info_of_name name in
          (* different color for higher-order types *)
          tag info TypeVoid;
          (* todo: ty_args *)
      | G.TyVar id ->
          let info = info_of_id id in
          tag info TypeVoid;
      | _ -> ()
      );
      k t
    );

  }
  in
  v (G.Pr gen);

  (* -------------------------------------------------------------------- *)
  (* toks phase 1 (sequence of tokens) *)
  (* -------------------------------------------------------------------- *)
  (* note: all TCommentSpace are filtered in xs so it should be easier to
   * write rules (but regular comments are kept as well as newlines).
   *)
  let rec aux_toks xs =
    match xs with
    | [] -> ()

    (* pad-specific: *)
    |   T.TComment(ii)
      ::T.TCommentNewline _ii2
      ::T.TComment(ii3)
      ::T.TCommentNewline ii4
      ::T.TComment(ii5)
      ::xs ->

        let s = PI.str_of_info ii in
        let s5 =  PI.str_of_info ii5 in
        (match () with
        | _ when s =~ ".*\\*\\*\\*\\*" && s5 =~ ".*\\*\\*\\*\\*" ->
          tag ii CommentEstet;
          tag ii5 CommentEstet;
          tag ii3 CommentSection1
        | _ when s =~ ".*------" && s5 =~ ".*------" ->
          tag ii CommentEstet;
          tag ii5 CommentEstet;
          tag ii3 CommentSection2
        | _ when s =~ ".*####" && s5 =~ ".*####" ->
          tag ii CommentEstet;
          tag ii5 CommentEstet;
          tag ii3 CommentSection0
        | _ ->
            ()
        );
        aux_toks (T.TComment ii3::T.TCommentNewline ii4::T.TComment ii5::xs)

    |   T.TComment(ii)::xs when (PI.str_of_info ii) =~ "(\\*[ ]*coupling:" ->
        tag ii CommentImportance3;
        aux_toks xs


    (* When we get a parse error, the AST does not contain the definitions, but
     * we can still try to tag certain things. Here is a
     * poor's man semantic tagger. We try to infer if an ident is a func,
     * or class, or module based on the few tokens around.
     *
     * This may look ridiculous to do such semantic tagging using tokens
     * instead of the full AST but many OCaml files could not parse with
     * the default parser because of camlp4 extensions so having
     * a solid token-based tagger is still useful as a last resort.
     * update: with attributes this becomes less true as attributes have
     *  a far more regular-syntax (that's what they were designed for)
     *)
    | T.Tlet(ii)::T.TLowerIdent(_s, ii3)::T.TEq _ii5::xs
        when PI.col_of_info ii = 0 ->

        if not (Hashtbl.mem already_tagged ii3) && lexer_based_tagger
        then tag ii3 (Entity (Global, (Def2 NoUse)));
        aux_toks xs;

    | T.Tlet(ii)::T.TLowerIdent(_s, ii3)::xs
        when PI.col_of_info ii = 0 ->

        if not (Hashtbl.mem already_tagged ii3) && lexer_based_tagger
        then tag ii3 (Entity (Function, (Def2 NoUse)));
        aux_toks xs;

    | (T.Tval(ii)|T.Texternal(ii))::T.TLowerIdent(_s, ii3)::xs
        when PI.col_of_info ii = 0 ->

        if not (Hashtbl.mem already_tagged ii3) && lexer_based_tagger
        then tag ii3 (FunctionDecl NoUse);
        aux_toks xs;

    | T.Tlet(ii)::
      T.Trec(_ii)::
      T.TLowerIdent(_s, ii3)::xs
        when PI.col_of_info ii = 0 ->

        if not (Hashtbl.mem already_tagged ii3) && lexer_based_tagger
        then tag ii3 (Entity (Function, (Def2 NoUse)));
        aux_toks xs;

    | T.Tand(ii)::T.TLowerIdent(_s, ii3)::xs
        when PI.col_of_info ii = 0 ->

        if not (Hashtbl.mem already_tagged ii3) && lexer_based_tagger
        then tag ii3 (Entity (Function, (Def2 NoUse)));
        aux_toks xs;

    | T.Ttype(ii)::T.TLowerIdent(_s, ii3)::xs
        when PI.col_of_info ii = 0 ->

        if not (Hashtbl.mem already_tagged ii3) && lexer_based_tagger
        then tag ii3 (Entity (Type, Def2 NoUse));
        aux_toks xs;

    (* module defs *)

    | T.Tmodule(_)
      ::T.TUpperIdent(_,ii_mod)
      ::T.TEq _
      ::T.Tstruct _::xs ->
        tag ii_mod (Entity (Module, Def2 fake_no_def2));
        aux_toks xs

    | T.Tmodule(_)
      ::T.TUpperIdent(_,ii_mod)
      ::T.TColon _
      ::T.Tsig _::xs ->
        tag ii_mod (Entity (Module, Def2 fake_no_def2));
        aux_toks xs

    (* bad smell, use of ref *)

    | T.TBang _ii1::T.TLowerIdent(_s2, ii2)::xs ->
        tag ii2 (UseOfRef);
        aux_toks xs

    | T.TBang _ii1::T.TUpperIdent(_s, ii)::T.TDot _::T.TLowerIdent(_s2, ii2)::xs ->
        tag ii (Entity (Module, Use2 fake_no_use2));
        tag ii2 (UseOfRef);
        aux_toks xs

    |    T.TLowerIdent(_, ii1)
      ::(T.TAssign ii2 | T.TAssignMutable ii2)
      ::xs ->
        tag ii1 (UseOfRef);
        tag ii2 (UseOfRef);
        aux_toks xs

    (* module use, and function call! *)

    | T.TUpperIdent(_s, ii)::T.TDot _ii2::T.TUpperIdent(_s2, _ii3)::xs ->
        tag ii (Entity (Module, Use2 fake_no_use2));
        aux_toks xs;

    | T.TUpperIdent(s, ii)::T.TDot _ii2::T.TLowerIdent(_s2, ii3)::xs ->

        (* see my .emacs *)
        if Hashtbl.mem h_builtin_modules s then begin
          tag ii BuiltinCommentColor;
          if not (Hashtbl.mem already_tagged ii3) && lexer_based_tagger
          then tag ii3 Builtin;
        end else begin
          tag ii (Entity (Module, Use2 fake_no_use2));
          (* tag ii3 (Function Use); *)
        end;
        aux_toks xs;

    (* labels *)
(* can be a def or use, no way to know
    | T.TTilde ii1::T.TLowerIdent (_s, ii2)::xs ->
        (* TODO when parser, can also have Use *)
        tag ii1 (Parameter Def);
        tag ii2 (Parameter Def);
        aux_toks xs
*)

    (* grammar rules in ocamlyacc *)
    | T.TLowerIdent (_s, ii1)::T.TColon _::xs
      when PI.col_of_info ii1 = 0 ->
        tag ii1 GrammarRule;
        aux_toks xs

    (* attributes *)
    | T.TBracketAtAt _::T.TLowerIdent (_, ii1)::
      T.TDot _::T.TLowerIdent (_, ii2)::xs ->
        tag ii1 Attribute;
        tag ii2 Attribute;
        aux_toks xs

    | T.TBracketAtAt _::T.TLowerIdent (_, ii)::xs ->
        tag ii Attribute;
        aux_toks xs

    | _x::xs ->
        aux_toks xs
  in
  let toks' = toks |> Common.exclude (function
    | T.TCommentSpace _ -> true
    | _ -> false
  )
  in
  aux_toks toks';

  (* -------------------------------------------------------------------- *)
  (* toks phase 2 (individual tokens) *)
  (* -------------------------------------------------------------------- *)

  if not disable_token_phase2 then
   toks |> List.iter (fun tok ->
    match tok with
    | T.TComment ii ->
        if not (Hashtbl.mem already_tagged ii)
        then
          (* a little bit syncweb specific *)
          let s = PI.str_of_info ii in
          (match s with
          (* yep, s e x are the syncweb markers *)
          | _ when s =~ "(\\*[sex]:"  -> tag ii CommentSyncweb
          (* normally then use of *** or ### or --- should be enough,
           * but in some files like ocamlyacc files the preceding
           * heuristic fail in which case it's useful to have those
           * rules. Moreover ocamldoc use something similar
           *)
          | _ when s =~ "(\\*1 "  -> tag ii CommentSection1
          | _ when s =~ "(\\*2 "  -> tag ii CommentSection2
          | _ when s =~ "(\\*3 "  -> tag ii CommentSection3
          | _ -> tag ii Comment
          )

    | T.TCommentMisc ii -> tag ii CppOther
    | T.TCommentNewline _ii | T.TCommentSpace _ii -> ()
    | T.TUnknown ii -> tag ii Error
    | T.EOF _ii-> ()

    | T.TSharpDirective ii -> tag ii Ifdef

    | T.TString (_s,ii) ->
        (* can have been tagged as a regexp *)
        if not (Hashtbl.mem already_tagged ii)
        then tag ii String

    | T.TChar (_s, ii) -> tag ii String
    | T.TFloat (_s,ii) | T.TInt (_s,ii) -> tag ii Number
    | T.Tfalse ii | T.Ttrue ii -> tag ii Boolean

    | T.Tlet ii | T.Tin ii | T.Tand ii | T.Trec ii
    | T.Tval ii | T.Texternal ii
        -> tag ii Keyword

    | T.Tfun ii | T.Tfunction ii -> tag ii Keyword

    | T.Ttype ii | T.Tof ii -> tag ii Keyword

    | T.Tif ii | T.Tthen ii | T.Telse ii ->
        tag ii KeywordConditional

    | T.Tmatch ii -> (* TODO: should also colorize it's with *)
        tag ii KeywordConditional
    | T.Twhen ii -> (* TODO: should also colorize it's with, when parser *)
        tag ii KeywordConditional

    | T.Ttry ii  -> tag ii KeywordExn

    | T.Twith ii ->
        if not (Hashtbl.mem already_tagged ii)
        then tag ii Keyword

    | T.Tfor ii | T.Tdo ii | T.Tdone ii | T.Twhile ii
    | T.Tto ii
    | T.Tdownto ii
      -> tag ii KeywordLoop

    | T.Tbegin ii | T.Tend ii -> tag ii KeywordLoop (* TODO: better categ ? *)

    | T.TBang ii | T.TAssign ii | T.TAssignMutable ii ->
        if not (Hashtbl.mem already_tagged ii)
        then tag ii UseOfRef

    | T.TEq ii ->
        if not (Hashtbl.mem already_tagged ii)
        then tag ii Punctuation

    | T.TSemiColon ii | T.TPipe ii | T.TComma ii
    | T.TOBracket ii | T.TCBracket ii
    | T.TOBrace ii | T.TCBrace ii
    | T.TOParen ii | T.TCParen ii
    | T.TOBracketPipe ii | T.TPipeCBracket ii

    | T.TPlus ii | T.TMinus ii
    | T.TLess ii | T.TGreater ii

    | T.TDot ii
    | T.TColon ii

    | T.TBracketAt ii | T.TBracketAtAt ii | T.TBracketAtAtAt ii
    | T.TBracketPercent ii | T.TBracketPercentPercent ii
        ->
        tag ii Punctuation

    | T.TUpperIdent (s, ii) ->
          (match s with
          | "Todo" -> tag ii BadSmell
          | _ -> () (* tag ii Constructor *)
           )

    | T.TLabelDecl (_s, ii) ->tag ii (Parameter Def)

    | T.Topen ii  -> tag ii BadSmell
    | T.LETOP (_, ii) | T.ANDOP (_, ii) -> tag ii Expanded

    | T.Tmodule ii | T.Tstruct ii | T.Tsig ii | T.Tinclude ii | T.Tfunctor ii
        -> tag ii KeywordModule

    | T.Tclass ii  | T.Tvirtual ii | T.Tprivate ii | T.Tobject ii
    | T.Tnew ii | T.Tmethod ii | T.Tinitializer ii | T.Tinherit ii
    | T.Tconstraint ii
        -> tag ii KeywordObject

    | T.Tmutable ii -> tag ii KeywordLoop
    | T.Tas ii -> tag ii Keyword
    | T.Texception ii -> tag ii KeywordExn

    | T.Tlazy ii | T.Tassert ii -> tag ii Keyword

    | T.TUnderscore ii | T.TTilde ii
    | T.TStar ii | T.TSemiColonSemiColon ii
      -> tag ii Punctuation

    | T.Tland ii  | T.Tasr ii | T.Tlxor ii | T.Tlsr ii | T.Tlsl ii
    | T.Tlor ii | T.Tmod ii | T.Tor ii ->
        tag ii Punctuation

    | T.TSharp ii  | T.TQuote ii  | T.TBackQuote ii
    | T.TQuestion ii | T.TQuestionQuestion ii

    | T.TDotDot ii  | T.TColonGreater ii | T.TColonColon ii

    | T.TAnd ii | T.TAndAnd ii
      -> tag ii Punctuation

    | T.TPrefixOperator (_, ii) | T.TInfixOperator (_, ii) ->
        tag ii Operator

    | T.TMinusDot ii | T.TPlusDot ii

    | T.TArrow ii | T.TBangEq ii
    | T.TOBracketGreater ii | T.TGreaterCBrace ii
    | T.TOBraceLess ii    | T.TGreaterCBracket ii | T.TOBracketLess ii
    | T.TDots ii | T.LDots ii | T.RDots ii
        -> tag ii Punctuation

    | T.TOptLabelUse (_, ii)
    | T.TLabelUse (_, ii)
        -> tag ii (Parameter Def) (* TODO *)

    | T.TOptLabelDecl (_, ii)
        -> tag ii (Parameter Def)

    | T.TLowerIdent (s, ii)
     ->
        match s with
        | _ when Hashtbl.mem h_pervasives_pad s ->
            tag ii BuiltinCommentColor
        | _ when Hashtbl.mem h_builtin_bool s ->
            tag ii BuiltinBoolean
        | "failwith" | "raise" ->
            tag ii KeywordExn
        | _ ->
            ()
  )
