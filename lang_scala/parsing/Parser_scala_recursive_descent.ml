(* Yoann Padioleau
 *
 * Copyright (C) 2021 R2C
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
module T = Parser_scala
module TH = Token_helpers_scala
open Parser_scala

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A recursive-descent Scala parser.
 *
 * This is mostly a port of Parsers.scala in the Scala 2 nsc compiler source
 * to OCaml.
 *
 * alt:
 *  - use Parser.scala of dotty?
 *  - use the one in scalameta?
*)

(* ok with partial match for all those accept *)
[@@@warning "-8"]

(* TODO: temporary *)
[@@@warning "-39-21-27-26-37-32"]

let debug_lexer = ref true

(*****************************************************************************)
(* Types  *)
(*****************************************************************************)

type env = {
  (* imitating the Scala implementation *)
  mutable token: T.token;

  (* not in Scala implementation *)
  mutable rest: T.token list;
}

let mk_env toks =
  match toks with
  | [] -> failwith "empty list of tokens, impossible, should at least get EOF"
  | x::xs ->
      { token = x;
        (* assume we will call first nextToken on it *)
        rest = (x::xs);
      }

let ab = Parse_info.abstract_info

(* TODO, use AST_scala something *)
let noSelfType = ()

type location =
  | Local
  | InBlock
  | InTemplate

(*****************************************************************************)
(* Error management  *)
(*****************************************************************************)
let error x in_ =
  pr2_gen in_.token;
  failwith x

(*****************************************************************************)
(* Helpers  *)
(*****************************************************************************)
(* todo: assert t1 is not a token with value (s, info) *)
let (=~=) t1 t2 =
  (TH.abstract_info_tok t1 =*= TH.abstract_info_tok t2)

let (++=) aref xs =
  ()

let (+=) aref x =
  ()

(*****************************************************************************)
(* Token helpers  *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Token lookahead *)
(* ------------------------------------------------------------------------- *)

let rec nextToken in_ =
  match in_.rest with
  | [] -> failwith "nextToken: no more tokens"
  | x::xs ->
      if !debug_lexer
      then pr2_gen x;
      in_.rest <- xs;
      (match x with
       | Space _ | Comment _ ->
           nextToken in_
       (* TODO: lots of condition on when to do that *)
       | Nl x ->
           in_.token <- NEWLINE x

       | other ->
           in_.token <- other;
      )

(* was called in.next.token *)
let rec next_next_token in_ =
  match in_.rest with
  | [] -> failwith "in_next_token: no more tokens"
  (* TODO: also skip spacing and stuff? *)
  | x::xs -> x

(* supposed to return the offset too *)
let skipToken in_ =
  nextToken in_

let init in_ =
  nextToken in_

(* supposed to return the offset too *)
let accept t in_ =
  if not (in_.token =~= t)
  then error (spf "was expecting: %s" (Common.dump t)) in_;
  (match t with
   | EOF _ -> ()
   | _ -> nextToken in_
  )

(*****************************************************************************)
(* Special parsing  *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Newline management  *)
(* ------------------------------------------------------------------------- *)

(** {{{
 *  semi = nl {nl} | `;`
 *  nl  = `\n` // where allowed
 *  }}}
*)
let acceptStatSep in_ =
  match in_.token with
  | NEWLINE _ | NEWLINES _ -> nextToken in_
  | _ -> accept (SEMI ab) in_

let acceptStatSepOpt in_ =
  if not (TH.isStatSeqEnd in_.token)
  then acceptStatSep in_


let newLineOpt in_ =
  match in_.token with
  | NEWLINE _ -> nextToken in_
  | _ -> ()

let newLineOptWhenFollowedBy token in_ =
  match in_.token, next_next_token in_ with
  | NEWLINE _, x when x =~= token -> newLineOpt in_
  | _ -> ()

let newLineOptWhenFollowing token in_ =
  failwith "newLineOptWhenFollowing"

(* ------------------------------------------------------------------------- *)
(* Trailing commas  *)
(* ------------------------------------------------------------------------- *)
(* supposed to return a boolean *)
let skipTrailingComma right in_ =
  match in_.token with
  | COMMA _ ->
      failwith "skipTrailingComma"
  | _ ->
      ()

(*****************************************************************************)
(* Grammar helpers  *)
(*****************************************************************************)

let inGroupers left right body in_ =
  accept left in_;
  let res = body in_ in
  skipTrailingComma right in_;
  accept right in_;
  res

let inBraces f in_ =
  inGroupers (LBRACE ab) (RBRACE ab) f in_
let inParens f in_ =
  inGroupers (LPAREN ab) (RPAREN ab) f in_
let inBrackets f in_ =
  inGroupers (LBRACKET ab) (RBRACKET ab) f in_

let inBracesOrNil f in_ =
  failwith "inBracesOrNil"

(** {{{ { `sep` part } }}}. *)
let separatedToken sep part in_ =
  let ts = ref [] in
  while in_.token =~= sep do
    nextToken in_;
    let x = part in_ in
    ts += x;
  done;
  !ts

(** {{{ { `sep` part } }}}. *)
let tokenSeparated separator part in_ =
  let ts = ref [] in
  let x = part in_ in
  ts += x;
  while in_.token =~= separator do
    nextToken in_;
    let x = part in_ in
    ts += x
  done;
  !ts

let makeParens body in_ =
  inParens (fun in_ ->
    match in_.token with
    | RPAREN _ -> []
    | _ -> body in_
  ) in_

(** {{{ tokenSeparated }}}, with the separator fixed to commas. *)
let commaSeparated part in_ =
  tokenSeparated (COMMA ab) part in_

(*****************************************************************************)
(* Parsing names  *)
(*****************************************************************************)

let ident in_ =
  match TH.isIdent in_.token with
  | Some (s, info) ->
      nextToken in_;
      (s, info)
  | None ->
      error "expecting ident" in_

let identForType in_ =
  let x = ident in_ in
  x

let selectors ~typeOK id in_ =
  failwith "selectors"

let selector t in_ =
  failwith "selector"

(** {{{
 *   QualId ::= Id {`.` Id}
 *   }}}
*)
let qualId in_ =
  let id = ident in_ in
  match in_.token with
  | DOT _ ->
      skipToken in_;
      selectors id ~typeOK:false in_
  | _ -> id


let pkgQualId in_ =
  let pkg = qualId in_ in
  newLineOptWhenFollowedBy (LBRACE ab) in_;
  pkg


let path ~thisOK ~typeOK in_ =
  pr2_once "TODO: path";
  ident in_

(*****************************************************************************)
(* Forward references  *)
(*****************************************************************************)
(* alt: have all functions mutually recursive, but it feels cleaner
 * to reduce those set of mutual dependencies
*)
let template_ = ref (fun _ -> failwith "forward ref not set")

(*****************************************************************************)
(* Parsing types  *)
(*****************************************************************************)

let startAnnotType in_ =
  (* TODO *)
  ident in_

let typeOrInfixType location in_ =
  failwith "typeOrInfixType"

(*****************************************************************************)
(* Parsing patterns  *)
(*****************************************************************************)

let caseClauses in_ =
  failwith "caseClauses"

(*****************************************************************************)
(* Parsing expressions  *)
(*****************************************************************************)

(** {{{
 *  Expr       ::= (Bindings | [`implicit`] Id | `_`)  `=>` Expr
 *               | Expr1
 *  ResultExpr ::= (Bindings | Id `:` CompoundType) `=>` Block
 *               | Expr1
 *  Expr1      ::= if `(` Expr `)` {nl} Expr [[semi] else Expr]
 *               | try (`{` Block `}` | Expr) [catch `{` CaseClauses `}`] [finally Expr]
 *               | while `(` Expr `)` {nl} Expr
 *               | do Expr [semi] while `(` Expr `)`
 *               | for (`(` Enumerators `)` | `{` Enumerators `}`) {nl} [yield] Expr
 *               | throw Expr
 *               | return [Expr]
 *               | [SimpleExpr `.`] Id `=` Expr
 *               | SimpleExpr1 ArgumentExprs `=` Expr
 *               | PostfixExpr Ascription
 *               | PostfixExpr match `{` CaseClauses `}`
 *  Bindings   ::= `(` [Binding {`,` Binding}] `)`
 *  Binding    ::= (Id | `_`) [`:` Type]
 *  Ascription ::= `:` CompoundType
 *               | `:` Annotation {Annotation}
 *               | `:` `_` `*`
 *  }}}
*)
let rec expr ?(location=Local) (in_: env) =
  expr0 location in_

and expr0 (location: location) (in_: env) =
  match in_.token with
  | Kif _ -> parseIf in_
  | Ktry _ -> parseTry in_
  | Kwhile _ -> parseWhile in_
  | Kdo _ -> parseDo in_
  | Kfor _ -> parseFor in_
  | Kreturn _ -> parseReturn in_
  | Kthrow _ -> parseThrow in_
  | Kimplicit _ ->
      skipToken in_;
      implicitClosure location in_
  | _ ->
      parseOther location in_

and parseOther location (in_: env) =
  let t = ref (postfixExpr in_) in
  (match in_.token with
   | EQ _ ->
       skipToken in_;
       (* ??? parsing that depends on built AST!! if Ident | Select | Apply *)
       let e = expr in_ in
       () (* mkAssign *)
   | COLON _ ->
       skipToken in_;
       (match in_.token with
        | UNDERSCORE _ ->
            skipToken in_;
            (match in_.token with
             | STAR _ (* todo was isIdent && name = "*" *) ->
                 ()
             | _ -> error "* expected" in_
            )
        | x when TH.isAnnotation x ->
            let ts = annotations ~skipNewLines:false in_ in
            (* fold around t *)
            ()
        | _ ->
            let tpt = typeOrInfixType location in_ in
            ()
       )
   | Kmatch _ ->
       skipToken in_;
       inBracesOrNil caseClauses in_
   | _ -> ()
  );
  (* // disambiguate between self types "x: Int =>" and orphan function
   * // literals "(x: Int) => ???"
   * // "(this: Int) =>" is parsed as an erroneous function literal but emits
   * // special guidance on what's probably intended.
  *)
  (* ??? another parsing depending on the AST! crazy *)
  let lhsIsTypedParamList x =
    failwith "lhsIsTypedParamList"
  in
  if (in_.token =~= (EQMORE ab) && location <> InTemplate &&
      lhsIsTypedParamList !t) then begin
    skipToken in_;
    let x =
      match location with
      | InBlock -> expr in_
      | _ -> block in_
    in
    ()
  end;
  !t


(** {{{
 *  PostfixExpr   ::= InfixExpr [Id [nl]]
 *  InfixExpr     ::= PrefixExpr
 *                  | InfixExpr Id [nl] InfixExpr
 *  }}}
*)
and postfixExpr in_ : unit =
  (* todo: opstack, reduce *)
  let rec loop top in_ =
    if not (TH.isIdentBool in_.token)
    then top
    else begin
      (* isIdentBool is true; remember that operators are identifiers and
       * that Scala allows any identifier in infix position *)
      newLineOptWhenFollowing (TH.isExprIntro) in_;
      if TH.isExprIntro in_.token then begin
        let res = prefixExpr in_ in
        match res with
        (*| None -> (* reduceExprStack *) None
          | Some *) next ->
            loop next in_
      end
      else (* finishPostfixOp *) ()
    end
  in
  let res = prefixExpr in_ in
  (* reduceExprStack *)
  loop res in_

(** {{{
 *  PrefixExpr   ::= [`-` | `+` | `~` | `!`] SimpleExpr
 *  }}}
*)
and prefixExpr in_ : unit =
  match in_.token with
  | t when TH.isUnaryOp t ->
      failwith "prefixExpr:unaryOp"
  | _ -> simpleExpr in_

(** {{{
 *  SimpleExpr    ::= new (ClassTemplate | TemplateBody)
 *                  |  BlockExpr
 *                  |  SimpleExpr1 [`_`]
 *  SimpleExpr1   ::= literal
 *                  |  xLiteral
 *                  |  Path
 *                  |  `(` [Exprs] `)`
 *                  |  SimpleExpr `.` Id
 *                  |  SimpleExpr TypeArgs
 *                  |  SimpleExpr1 ArgumentExprs
 *  }}}
*)

and simpleExpr in_ : unit =
  let canApply = ref true in
  let t =
    match in_.token with
    | x when TH.isLiteral x ->
        let x = literal in_ in
        ()
    (* TODO: XMLSTART *)
    | x when TH.isIdentBool x ->
        let x = path ~thisOK:true ~typeOK:false in_ in
        ()
    | Kthis _ | Ksuper _ ->
        let x = path ~thisOK:true ~typeOK:false in_ in
        ()
    | UNDERSCORE _ ->
        let x = freshPlaceholder in_ in
        ()
    | LPAREN _ ->
        let x = makeParens (commaSeparated expr) in_ in
        ()
    | LBRACE _ ->
        canApply := false;
        let x = blockExpr in_ in
        ()
    | Knew _ ->
        canApply := false;
        skipToken in_;
        let (parents, self, stats) = !template_ in_ in
        ()
    | _ -> error "illegal start of simple expression" in_
  in
  simpleExprRest ~canApply:!canApply t in_

(** {{{
 *  SimpleExpr    ::= literal
 *                  | symbol
 *                  | null
 *  }}}
*)
and literal ?(isNegated=false) ?(inPattern=false) in_ =
  let finish x =
    nextToken in_;
    x
  in
  match in_.token with
  | T_INTERPOLATED_START _ ->
      (* supposed to use inPattern here to different AST building *)
      interpolatedString ~inPattern in_
  (* unsupported in Scala3 *)
  | SymbolLiteral(s, ii) -> finish ()

  | CharacterLiteral(x, ii) -> finish ()
  (* supposed to use isNegated here *)
  | IntegerLiteral(x, ii) -> finish ()
  | FloatingPointLiteral(x, ii) -> finish ()

  | StringLiteral(x, ii) -> finish ()
  | BooleanLiteral(x, ii) -> finish ()
  | Knull _ -> finish ()
  | _ -> error "illegal literal" in_

and interpolatedString ~inPattern in_ =
  failwith "interpolatedString"

and simpleExprRest ~canApply t in_ =
  if canApply then newLineOptWhenFollowedBy (LBRACE ab) in_;
  match in_.token with
  | DOT _ ->
      nextToken in_;
      let x = selector t in_ in
      simpleExprRest ~canApply:true x in_
  | LBRACKET _ ->
      (* ??? parsing depending on built AST: Ident(_) | Select(_, _) | Apply(_, _) | Literal(_) *)
      failwith "simpleExprRest: LBRACKET"
  | LPAREN _ ->
      let x = argumentExprs in_ in
      t
  | LBRACE _ when canApply ->
      let x = argumentExprs in_ in
      let app = t in
      simpleExprRest ~canApply:true app in_
  | UNDERSCORE _ ->
      skipToken in_;
      t
  | _ -> t

and freshPlaceholder in_ =
  nextToken in_;
  ()


(* ------------------------------------------------------------------------- *)
(* Arguments *)
(* ------------------------------------------------------------------------- *)
and multipleArgumentExprs in_ =
  failwith "multipleArgumentExprs"

(** {{{
 *  ArgumentExprs ::= `(` [Exprs] `)`
 *                  | [nl] BlockExpr
 *  }}}
*)
and argumentExprs in_ : unit list =
  let args in_ =
    (* less: if isIdent assignmentToMaybeNamedArg *)
    commaSeparated expr in_
  in
  match in_.token with
  | LBRACE _ -> [blockExpr in_]
  | LPAREN _ ->
      (* less: could use makeParens *)
      inParens (fun in_ ->
        match in_.token with
        | RPAREN _ -> []
        | _ -> args in_
      ) in_
  | _ -> []

(* ------------------------------------------------------------------------- *)
(* Infix expr *)
(* ------------------------------------------------------------------------- *)

(* ------------------------------------------------------------------------- *)
(* Misc *)
(* ------------------------------------------------------------------------- *)
and implicitClosure location in_ =
  failwith "implicitClosure"

(*****************************************************************************)
(* Parsing statements  *)
(*****************************************************************************)
and parseIf in_ =
  failwith "parseIf"

and parseTry in_ =
  failwith "parseTry"

and parseWhile in_ =
  failwith "parseWhile"

and parseDo in_ =
  failwith "parseDo"

and parseFor in_ =
  failwith "parseFor"

and parseReturn in_ =
  failwith "parseReturn"

and parseThrow in_ =
  failwith "parseThrow"

and statement (location: location) (in_: env) =
  expr ~location in_

and block in_ =
  failwith "block"

and blockExpr in_ =
  failwith "block"

(*****************************************************************************)
(* Parsing annotations  *)
(*****************************************************************************)

and readAnnots part in_ =
  separatedToken (AT ab) part in_

(** {{{
 *  Annotations       ::= {`@` SimpleType {ArgumentExprs}}
 *  ConstrAnnotations ::= {`@` SimpleType ArgumentExprs}
 *  }}}
*)
and annotationExpr in_ =
  failwith "annotationExpr"

and annotations ~skipNewLines in_ =
  readAnnots (fun in_ ->
    let t = annotationExpr in_ in
    if skipNewLines then newLineOpt in_;
    t
  )

(*****************************************************************************)
(* Parsing directives  *)
(*****************************************************************************)
let packageObjectDef in_ =
  failwith "packageObjectDef"

let packageOrPackageObject in_ =
  failwith "packageOrPackageObject"

let importClause in_ =
  failwith "importClause"

(*****************************************************************************)
(* Parsing modifiers  *)
(*****************************************************************************)

(** {{{
 *  AccessQualifier ::= `[` (Id | this) `]`
 *  }}}
*)
let accessQualifierOpt mods in_ =
  let result = mods in
  (match in_.token with
   | LBRACKET _ ->
       nextToken in_;
       (match in_.token with
        | Kthis _ ->
            nextToken in_;
            (* Flags.Local?? *)
        | _ ->
            let x = identForType in_ in
            ()
       );
       accept (RBRACKET ab) in_
  );
  result

let addMods mods t in_ =
  nextToken in_;
  t::mods

(** {{{
 *  Modifiers ::= {Modifier}
 *  Modifier  ::= LocalModifier
 *              |  AccessModifier
 *              |  override
 *  }}}
*)
let modifiers in_ =
  let rec loop mods =
    match in_.token with
    | Kprivate _ | Kprotected _ ->
        let mods = addMods mods in_.token in_ in
        let mods = accessQualifierOpt mods in_ in
        loop mods
    | Kabstract _ | Kfinal _ | Ksealed _ | Koverride _ | Kimplicit _ | Klazy _
      ->
        let mods = addMods mods in_.token in_ in
        loop mods
    | NEWLINE _ ->
        nextToken in_;
        loop mods
    | _ -> mods
  in
  loop []


(*****************************************************************************)
(* Parsing definitions  *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Helpers *)
(* ------------------------------------------------------------------------- *)

let statSeq ?(errorMsg="illegal start of definition") stat in_ =
  let stats = ref [] in
  while not (TH.isStatSeqEnd in_.token) do
    (match stat in_ with
     | Some xs ->
         stats ++= xs
     | None ->
         if TH.isStatSep in_.token
         then ()
         else error errorMsg in_
    );
    acceptStatSepOpt in_
  done;
  !stats

(* ------------------------------------------------------------------------- *)
(* Def or Dcl *)
(* ------------------------------------------------------------------------- *)
let nonLocalDefOrDcl in_ =
  failwith "nonLocalDefOrDcl"

(* ------------------------------------------------------------------------- *)
(* "Template" *)
(* ------------------------------------------------------------------------- *)

(** {{{
 *  TemplateStats    ::= TemplateStat {semi TemplateStat}
 *  TemplateStat     ::= Import
 *                     | Annotations Modifiers Def
 *                     | Annotations Modifiers Dcl
 *                     | Expr1
 *                     | super ArgumentExprs {ArgumentExprs}
 *                     |
 *  }}}
*)
let templateStat in_ =
  match in_.token with
  | Kimport _ ->
      let x = importClause in_ in
      Some x
  | t when TH.isDefIntro t || TH.isModifier t || TH.isAnnotation t ->
      let x = nonLocalDefOrDcl in_ in
      Some x
  | t when TH.isExprIntro t ->
      let x = statement InTemplate in_ in
      Some x
  | _ -> None

let templateStats in_ =
  statSeq templateStat in_

(** {{{
 *  TemplateStatSeq  ::= [id [`:` Type] `=>`] TemplateStats
 *  }}}
 * @param isPre specifies whether in early initializer (true) or not (false)
*)

let templateStatSeq ~isPre in_ =
  let self = ref noSelfType in
  let firstOpt = ref None in
  if (TH.isExprIntro in_.token) then begin
    let first = expr ~location:InTemplate in_ in
    (match in_.token with
     | EQMORE _ ->
         (* todo: self := ... *)
         nextToken in_
     | _ ->
         firstOpt := Some first;
         acceptStatSepOpt in_
    )
  end;
  let xs = templateStats in_ in
  !self, (* TODO !firstOpt @ *) xs

(** {{{
 *  TemplateBody ::= [nl] `{` TemplateStatSeq `}`
 *  }}}
 * @param isPre specifies whether in early initializer (true) or not (false)
*)

let templateBody ~isPre in_ =
  let xs = inBraces (templateStatSeq ~isPre) in_ in
  xs

let templateBodyOpt ~parenMeansSyntaxError in_ =
  newLineOptWhenFollowedBy (LBRACE ab) in_;
  match in_.token with
  | LBRACE _ ->
      templateBody ~isPre:false in_
  | LPAREN _ ->
      if parenMeansSyntaxError
      then error "traits or objects may not have parameters" in_
      else error "unexpected opening parenthesis" in_
  | _ -> noSelfType, []


(** {{{
 *  ClassParents       ::= AnnotType {`(` [Exprs] `)`} {with AnnotType}
 *  TraitParents       ::= AnnotType {with AnnotType}
 *  }}}
*)
let templateParents in_ =
  let parents = ref [] in
  let readAppliedParent () =
    let parent = startAnnotType in_ in
    parents +=
    (match in_.token with
     | LPAREN _ ->
         let _xs = multipleArgumentExprs in_ in
         (* less: fold_left apply xs *)
         parent
     | _ -> parent
    );
  in
  readAppliedParent ();
  while in_.token =~= (Kwith ab) do
    nextToken in_;
    readAppliedParent ()
  done;
  !parents

(** {{{
 *  ClassTemplate ::= [EarlyDefs with] ClassParents [TemplateBody]
 *  TraitTemplate ::= [EarlyDefs with] TraitParents [TemplateBody]
 *  EarlyDefs     ::= `{` [EarlyDef {semi EarlyDef}] `}`
 *  EarlyDef      ::= Annotations Modifiers PatDef
 *  }}}
*)
let template in_ =
  newLineOptWhenFollowedBy (LBRACE ab) in_;
  match in_.token with
  | LBRACE _ ->
      let (self, body) = templateBody ~isPre:true in_ in
      (match in_.token with
       | Kwith _ (* less: && self eq noSelfType *) ->
           let _earlyDefs = body (* less: map ensureEarlyDef *) in
           nextToken in_;
           let parents = templateParents in_ in
           let (self1, body1) = templateBodyOpt ~parenMeansSyntaxError:false in_ in
           (parents, self1, (* earlyDefs @ *) body1)
       | _ ->
           ([], self, body)
      )
  | _ ->
      let parents = templateParents in_ in
      let (self, body) = templateBodyOpt ~parenMeansSyntaxError:false in_ in
      (parents, self, body)


(** {{{
 *  ClassTemplateOpt ::= `extends` ClassTemplate | [[`extends`] TemplateBody]
 *  TraitTemplateOpt ::= TraitExtends TraitTemplate | [[TraitExtends] TemplateBody]
 *  TraitExtends     ::= `extends` | `<:` (deprecated)
 *  }}}
*)
let templateOpt in_ =
  let (parents, self, body) =
    match in_.token with
    | Kextends _ | LESSCOLON _ (* deprecated in 2.12.5 *) ->
        nextToken in_;
        template in_
    | _ ->
        newLineOptWhenFollowedBy (LBRACE ab) in_;
        let (self, body) =
          let parenMeansSyntaxError =
            (*TODO mods.isTrait || name.isTermName *) true
          in
          templateBodyOpt ~parenMeansSyntaxError in_ in
        [], self, body
  in
  ()


(* ------------------------------------------------------------------------- *)
(* Object *)
(* ------------------------------------------------------------------------- *)

(** {{{
 *  ObjectDef       ::= Id ClassTemplateOpt
 *  }}}
*)
let objectDef in_ =
  nextToken in_; (* 'object' *)
  let name = ident in_ in
  let tmpl = templateOpt in_ in
  tmpl



(* ------------------------------------------------------------------------- *)
(* Class/trait *)
(* ------------------------------------------------------------------------- *)
let classDef in_ =
  failwith "classDef"

(* ------------------------------------------------------------------------- *)
(* TmplDef *)
(* ------------------------------------------------------------------------- *)

(** {{{
 *  TmplDef ::= [case] class ClassDef
 *            |  [case] object ObjectDef
 *            |  [override] trait TraitDef
 *  }}}
*)
let tmplDef in_ =
  match in_.token with
  | Ktrait _ -> classDef in_
  | Kclass _ -> classDef in_
  (* Caseclass -> classDef in_ *)
  | Kobject _ -> objectDef in_
  (* Caseobject -> objectDef in_ *)
  | _ -> error "expected start of definition" in_

(*****************************************************************************)
(* Toplevel  *)
(*****************************************************************************)

(** Hook for IDE, for top-level classes/objects. *)
let topLevelTmplDef in_ =
  let _annots = annotations ~skipNewLines:true in_ in
  let _mods = modifiers in_ in
  let x = tmplDef in_ in
  x

(** {{{
 *  TopStatSeq ::= TopStat {semi TopStat}
 *  TopStat ::= Annotations Modifiers TmplDef
 *            | Packaging
 *            | package object ObjectDef
 *            | Import
 *            |
 *  }}}
*)
let topStat in_ =
  match in_.token with
  | Kpackage _ ->
      skipToken in_;
      let x = packageOrPackageObject in_ in
      Some x
  | Kimport _ ->
      let x = importClause in_ in
      Some x
  | t when TH.isAnnotation t || TH.isTemplateIntro t || TH.isModifier t ->
      let x = topLevelTmplDef in_ in
      Some x
  | _ -> None

let topStatSeq in_ =
  statSeq ~errorMsg:"expected class or object definition" topStat in_

(*****************************************************************************)
(* Entry point  *)
(*****************************************************************************)

(* set the forward reference *)
let _ =
  template_ := template;
  ()

(** {{{
 *  CompilationUnit ::= {package QualId semi} TopStatSeq
 *  }}}
*)
let compilationUnit in_ =
  let rec topstats in_ =
    let ts = ref [] in
    while (match in_.token with SEMI _ -> true | _ -> false) do
      nextToken in_
    done;
    (match in_.token with
     | Kpackage _ ->
         nextToken in_;
         (match in_.token with
          | Kobject _ ->
              let xs = packageObjectDef in_ in
              ts ++= xs;
              if not (in_.token =~= (EOF ab)) then begin
                acceptStatSep in_;
                let xs = topStatSeq in_ in
                ts ++= xs
              end
          | _ ->
              let pkg = pkgQualId in_ in
              (match in_.token with
               | EOF _ ->
                   let pack = () in
                   ts += pack
               | x when TH.isStatSep x ->
                   nextToken in_;
                   let xs = topstats in_ in
                   let pack = () in
                   ts += pack
               | _ ->
                   let xs = inBraces topStatSeq in_ in
                   let pack = () in
                   ts += pack;
                   acceptStatSepOpt in_;
                   let xs = topStatSeq in_ in
                   ts ++= xs
              )
         )
     | _ ->
         let xs = topStatSeq in_ in
         ts ++= xs
    );
    !ts
  in
  let xs = topstats in_ in
  ()

let parse toks =
  let in_ = mk_env toks in
  init in_;
  let xs = compilationUnit in_ in
  accept (EOF ab) in_;
  xs
