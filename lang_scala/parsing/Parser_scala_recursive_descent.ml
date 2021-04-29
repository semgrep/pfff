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
 * This is mostly a port of Parsers.scala, found in the Scala 2 nsc
 * compiler source, to OCaml.
 *
 * alt:
 *  - use Parsers.scala of dotty?
 *  - use the one in scalameta?
 *
 * Note that parsing Scala is difficult. See the crazy: tag in this file.
 *
 * TODO:
 *  - see the AST: tag for what Parsers.scala was doing to build an AST
 * less:
 *  - see the CHECK: tag for what Parsers.scala was statically checking
 *
 * Not handled (or partially handled) on purpose for now:
 *  - XML (deprecated constructs anyway)
 *  - symbolic literals (deprecated constructs anyway)
 *  - lots of semantic AST rewriting
 *     * stuff around placeholder '_' and implicit parameters
 *       CHECK: "unbound placeholder paramter"
 *       CHECK: "unbound wildcard type"
 *  - error recovery (skipping parens, braces, etc.)
 *  - advanced error diagnosis: lots of variants, deprecationWarning,
 *    syntaxError, expecting X but got Y, etc.
 *  - position/offset management (just use position info in the token, easier)
 *
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
let empty = ()

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

let nextTokenAllow next in_ =
  pr2_once "nextTokenAllow: TODO";
  nextToken in_

(* was called in.next.token *)
let rec next_next_token in_ =
  match in_.rest with
  | [] -> failwith "in_next_token: no more tokens"
  (* TODO: also skip spacing and stuff? *)
  | x::xs -> x

let skipToken in_ =
  nextToken in_

let init in_ =
  nextToken in_

(* Consume one token of the specified type, or signal an error if it is
 * not there *)
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

(* less: do actual error recovery? *)
let inBracesOrNil = inBraces

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

(* AST: Creates an actual Parens node (only used during parsing.) *)
let makeParens body in_ =
  inParens (fun in_ ->
    match in_.token with
    | RPAREN _ -> []
    | _ -> body in_
  ) in_

(* Strip the artificial `Parens` node to create a tuple term Tree. *)
let stripParens x =
  (* AST: if Parens(ts) -> makeSafeTupleTerm(ts) *)
  x

(** {{{ tokenSeparated }}}, with the separator fixed to commas. *)
let commaSeparated part in_ =
  tokenSeparated (COMMA ab) part in_

(*****************************************************************************)
(* Parsing names  *)
(*****************************************************************************)

(* AST: Assumed to be TermNames *)
let ident in_ =
  match TH.isIdent in_.token with
  | Some (s, info) ->
      nextToken in_;
      (s, info)
  | None ->
      error "expecting ident" in_

let rawIdent in_ =
  (* AST: in.name *)
  let s = "" in
  nextToken in_;
  "", Parse_info.fake_info "RAW"

let identOrMacro in_ =
  if (TH.isMacro in_.token)
  then ident in_
  else rawIdent in_

let wildcardOrIdent in_ =
  match in_.token with
  | USCORE ii ->
      nextToken in_;
      (* AST: nme.WILDCARD *)
      ("_", ii)
  | _ -> ident in_

(* For when it's known already to be a type name. *)
let identForType in_ =
  let x = ident in_ in
  (* AST: x.toTypeName *)
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
  (* AST: Ident(id) *)
  let id = ident in_ in
  match in_.token with
  | DOT _ ->
      skipToken in_;
      selectors id ~typeOK:false in_
  | _ -> id

(* Calls `qualId()` and manages some package state. *)
let pkgQualId in_ =
  (* AST: if ... then inScalePackage = true *)
  let pkg = qualId in_ in
  newLineOptWhenFollowedBy (LBRACE ab) in_;
  (* AST: adjust currentPackage *)
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
let tmplDef_ = ref (fun _ -> failwith "forward ref not set")

(*****************************************************************************)
(* Parsing types  *)
(*****************************************************************************)

let typ in_ =
  pr2_once "typ: TODO";
  ident in_

let startAnnotType in_ =
  pr2_once "startAnnotType: TODO";
  ident in_

let typeOrInfixType location in_ =
  failwith "typeOrInfixType"

(** {{{
 *  TypedOpt ::= [`:` Type]
 *  }}}
*)
let typedOpt in_ =
  match in_.token with
  | COLON _ ->
      nextToken in_;
      let t = typ in_ in
      Some t
  | _ -> None (* AST: TypeTree *)

(*****************************************************************************)
(* Parsing patterns  *)
(*****************************************************************************)

let caseClauses in_ =
  failwith "caseClauses"

let patDefOrDcl mods in_ =
  failwith "patDefOrDcl"

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
   | EQUALS _ ->
       skipToken in_;
       (* crazy: parsing that depends on built AST!!
        * AST: if Ident | Select | Apply *)
       let e = expr in_ in
       () (* AST: mkAssign(t, e) *)
   | COLON _ ->
       t := stripParens !t;
       skipToken in_;
       (match in_.token with
        | USCORE _ ->
            skipToken in_;
            (match in_.token with
             | STAR _ (* TODO: was isIdent && name = "*" *) ->
                 nextToken in_
             (* AST: t = Typed(t, Ident(WILDCARD_STAR)) *)
             | _ -> error "* expected" in_
            )
        | x when TH.isAnnotation x ->
            let ts = annotations ~skipNewLines:false in_ in
            (* AST: fold around t makeAnnotated *)
            ()
        | _ ->
            let tpt = typeOrInfixType location in_ in
            (* AST: if isWildcard(t) ... *)
            (* AST: Typed(t, tpt) *)
            ()
       )
   | Kmatch _ ->
       skipToken in_;
       let _xs = inBracesOrNil caseClauses in_ in
       (* AST: t:= Match(stripParens(t)) *)
       ()
   | _ -> ()
  );
  (* AST: disambiguate between self types "x: Int =>" and orphan function
   * literals "(x: Int) => ???"
   * "(this: Int) =>" is parsed as an erroneous function literal but emits
   * special guidance on what's probably intended.
  *)
  (* crazy: another parsing depending on the AST! crazy *)
  let lhsIsTypedParamList x =
    (* CHECK: "self-type annotation may not be in parentheses" *)
    failwith "lhsIsTypedParamList"
  in
  if (in_.token =~= (ARROW ab) && location <> InTemplate &&
      lhsIsTypedParamList !t) then begin
    skipToken in_;
    let x =
      match location with
      | InBlock -> expr in_
      | _ -> block in_
    in
    (* AST: Function(convertToParams(t), x) *)
    ()
  end;
  (* AST: stripParens(t) *)
  !t


(** {{{
 *  PostfixExpr   ::= InfixExpr [Id [nl]]
 *  InfixExpr     ::= PrefixExpr
 *                  | InfixExpr Id [nl] InfixExpr
 *  }}}
*)
and postfixExpr in_ : unit =
  (* TODO: AST: opstack, reduce *)
  let rec loop top in_ =
    if not (TH.isIdentBool in_.token)
    then top
    else begin
      (* isIdentBool is true; remember that operators are identifiers and
       * that Scala allows any identifier in infix position
      *)
      (* AST: pushOpInfo(reduceExprStack(base, top)) *)
      newLineOptWhenFollowing (TH.isExprIntro) in_;
      if TH.isExprIntro in_.token then begin
        let res = prefixExpr in_ in
        match res with
        (*| None -> (* AST: reduceExprStack(base, top) *) None
          | Some *) next ->
            loop next in_
      end
      else (* AST: finishPostfixOp(base, popOpInfo()) *) ()
    end
  in
  let res = prefixExpr in_ in
  (* AST: reduceExprStack (res) *)
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
    (* less: XMLSTART *)
    | x when TH.isIdentBool x ->
        let x = path ~thisOK:true ~typeOK:false in_ in
        ()
    | Kthis _ | Ksuper _ ->
        let x = path ~thisOK:true ~typeOK:false in_ in
        ()
    | USCORE _ ->
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
        (* AST: gen.mkNew(parents, self, stats) *)
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
  let finish value_ =
    (* AST: newLiteral(value) *)
    nextToken in_;
    value_
  in
  match in_.token with
  | T_INTERPOLATED_START _ ->
      (* AST: if not inPattern then withPlaceholders(...) *)
      interpolatedString ~inPattern in_
  (* CHECK: unsupported in Scala3, deprecated in 2.13.0 *)
  (* AST: Apply(scalaDot(Symbol, List(finish(in.strVal)) *)
  | SymbolLiteral(s, ii) -> finish ()

  | CharacterLiteral(x, ii) -> finish () (* AST: incharVal *)
  | IntegerLiteral(x, ii) -> finish () (* AST: in.intVal(isNegated) *)
  | FloatingPointLiteral(x, ii) -> finish () (* AST: in.floatVal(isNegated) *)

  | StringLiteral(x, ii) -> finish () (* AST: in.strVal.intern() *)
  | BooleanLiteral(x, ii) -> finish () (* AST: bool *)
  | Knull _ -> finish () (* AST: null *)
  | _ -> error "illegal literal" in_

and interpolatedString ~inPattern in_ =
  failwith "interpolatedString"

and simpleExprRest ~canApply t in_ =
  (* crazy: again parsing depending on AST *)
  if canApply then newLineOptWhenFollowedBy (LBRACE ab) in_;

  let paren_or_brace () =
    let x = argumentExprs in_ in
    let app =
      (* AST: look for anonymous function application like (f _)(x) and
       *      translate to (f _).apply(x), bug #460
       * AST: let sel = ... Select(stripParens(t, nme.apply) in
       *      Apply(sel, x)
      *)

      t
    in
    simpleExprRest ~canApply:true app in_
  in
  match in_.token with
  | DOT _ ->
      nextToken in_;
      let x = selector t in_ in
      let x = stripParens t in
      simpleExprRest ~canApply:true x in_
  | LBRACKET _ ->
      (* crazy: parsing depending on built AST: Ident(_) | Select(_, _) | Apply(_, _) | Literal(_) *)
      failwith "simpleExprRest: LBRACKET"

  | LPAREN _ -> paren_or_brace ()
  | LBRACE _ when canApply -> paren_or_brace ()
  | USCORE _ ->
      skipToken in_;
      (* AST: MethodValue(stripParens(t)) *)
      t
  | _ -> t

(* AST: *)
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
    (* AST: if isIdent then assignmentToMaybeNamedArg *)
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
  failwith "blockExpr"

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

let wildImportSelector in_ =
  (* AST: val selector = ImportSelector.wildAt(in.offset) *)
  nextToken in_

(** {{{
 *  ImportSelector ::= Id [`=>` Id | `=>` `_`]
 *  }}}
*)
let importSelector in_ =
  let name = wildcardOrIdent in_ in
  let rename =
    match in_.token with
    | ARROW _ ->
        nextToken in_;
        (* CHECK: "Wildcard import cannot be renamed" *)
        wildcardOrIdent in_
    (* AST: if name = nme.WILDCARD && !bbq => null *)
    | _ -> name
  in
  (* AST: ImportSelector(name, start, rename, renameOffset) *)
  ()


(** {{{
 *  ImportSelectors ::= `{` {ImportSelector `,`} (ImportSelector | `_`) `}`
 *  }}}
*)
let importSelectors in_ =
  let selectors = inBracesOrNil (commaSeparated importSelector) in_ in
  (* CHECK: "Wildcard import must be in last position" *)
  selectors

(** {{{
 *  ImportExpr ::= StableId `.` (Id | `_` | ImportSelectors)
 *  }}}
*)
let importExpr in_ =
  let thisDotted _name in_ =
    nextToken in_;
    (* AST: val t = This(name) *)
    let t = () in
    accept (DOT ab) in_;
    let result = selector t in_ in
    accept (DOT ab) in_;
    result
  in
  (** Walks down import `foo.bar.baz.{ ... }` until it ends at
   * an underscore, a left brace, or an undotted identifier.
  *)
  let rec loop expr in_ =
    let selectors =
      match in_.token with
      | USCORE _ -> [wildImportSelector in_] (* import foo.bar._; *)
      | LBRACE _ -> importSelectors in_  (* import foo.bar.{ x, y, z } *)
      | _ ->
          let name = ident in_ in
          (match in_.token with
           | DOT _ ->
               (* import foo.bar.ident.<unknown> and so create a select node and recurse. *)
               (* AST: (Select(expr, name)) *)
               let t = expr in
               nextToken in_;
               loop t in_
           | _ ->
               (* import foo.bar.Baz; *)
               (* AST: List(makeImportSelector(name, nameOffset)) *)
               []
          )
    in
    (* reaching here means we're done walking. *)
    (* AST: Import(expr, selectors) *)
    []
  in
  let start =
    match in_.token with
    | Kthis _ -> thisDotted empty in_
    | _ ->
        (* AST: Ident() *)
        let id = ident in_ in
        (match in_.token with
         | DOT _ -> accept (DOT ab) in_
         | x when not (TH.isStatSep x) -> accept (DOT ab) in_
         | _ -> error ". expected" in_
        );
        (match in_.token with
         | Kthis _ -> thisDotted (* AST: id.name.toTypeName*) id in_
         | _ -> (*id*) ()
        )
  in
  loop start in_

(** {{{
 *  Import  ::= import ImportExpr {`,` ImportExpr}
 *  }}}
*)
let importClause in_ =
  accept (Kimport ab) in_;
  commaSeparated importExpr in_

(*****************************************************************************)
(* Parsing modifiers  *)
(*****************************************************************************)

(** {{{
 *  AccessQualifier ::= `[` (Id | this) `]`
 *  }}}
*)
let accessQualifierOpt mods in_ =
  let result = ref mods in
  (match in_.token with
   | LBRACKET _ ->
       nextToken in_;
       (* CHECK: "duplicate private/protected qualifier" *)
       (match in_.token with
        | Kthis _ ->
            nextToken in_;
            (* Flags.Local?? *)
        | _ ->
            let x = identForType in_ in
            (* AST: Modifiers(mods.flags, x) *)
            ()
       );
       accept (RBRACKET ab) in_
  );
  !result

(* AST: normalizeModifiers() *)
(* CHECK: "repeated modifier" *)
let addMod mods t in_ =
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
  (* AST: normalizeModifiers() *)
  let rec loop mods =
    match in_.token with
    | Kprivate _ | Kprotected _ ->
        let mods = addMod mods in_.token in_ in
        let mods = accessQualifierOpt mods in_ in
        loop mods
    | Kabstract _ | Kfinal _ | Ksealed _ | Koverride _ | Kimplicit _ | Klazy _
      ->
        let mods = addMod mods in_.token in_ in
        loop mods
    | NEWLINE _ ->
        nextToken in_;
        loop mods
    | _ -> mods
  in
  (* AST: NoMods *)
  loop []


(*****************************************************************************)
(* Parsing Methods/Functions  *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Parameter *)
(* ------------------------------------------------------------------------- *)

(** {{{
 *  ParamType ::= Type | `=>` Type | Type `*`
 *  }}}
*)
let paramType ?(repeatedParameterOK=true) in_ =
  match in_.token with
  | ARROW _ ->
      nextToken in_;
      let t = typ in_ in
      (* AST: byNameApplication *)
      ()
  | _ ->
      let t = typ in_ in
      if (TH.isRawStar in_.token)
      then begin
        nextToken in_;
        (* CHECK: if (!repeatedParameterOK)
         * "repeated parameters are only allowed in method signatures" *)
        (* AST: repeatedApplication t *)
        ()
      end
      else () (* AST: t *)

(** {{{
 *  ParamClauses      ::= {ParamClause} [[nl] `(` implicit Params `)`]
 *  ParamClause       ::= [nl] `(` [Params] `)`
 *  Params            ::= Param {`,` Param}
 *  Param             ::= {Annotation} Id [`:` ParamType] [`=` Expr]
 *  ClassParamClauses ::= {ClassParamClause} [[nl] `(` implicit ClassParams `)`]
 *  ClassParamClause  ::= [nl] `(` [ClassParams] `)`
 *  ClassParams       ::= ClassParam {`,` ClassParam}
 *  ClassParam        ::= {Annotation}  [{Modifier} (`val` | `var`)] Id [`:` ParamType] [`=` Expr]
 *  }}}
*)


let param owner implicitmod caseParam in_ =
  let annots = annotations ~skipNewLines:false in_ in
  let mods = ref [] (* AST: PARAM *) in
  (* AST: crazy?: if owner.isTypeName ... modifiers () *)
  let name = ident in_ in
  let bynamemod = ref 0 in
  let tpt =
    accept (COLON ab) in_;
    if in_.token =~= (ARROW ab) then begin
      (* CHECK: if owner.isTypeName && !mods.isLocalToThis
       * "var/val parameters may not be call-by-name" *)
      bynamemod := 1 (* AST: Flags.BYNAMEPARAM *)
    end;
    paramType in_
  in
  let default =
    match in_.token with
    | EQUALS _ ->
        nextToken in_;
        (* AST: mods |= FLAGS.DEFAULTPARAM *)
        expr in_
    | _ ->
        (* AST: emptyTree *)
        ()
  in
  (* AST: ValDef((mods | implicitmod | bynamemod) with annots, name.toTermName, tpt, default) *)
  ()


(* CHECK: "no by-name parameter type allowed here" *)
(* CHECK: "no * parameter type allowed here" *)
(* AST: convert tree to parameter *)
(* CHECK: Tuples cannot be directly destructured in method ... *)
(* CHECK: "identifier expected" *)
let paramClauses ~ofCaseClass owner contextBoundBuf in_ =
  let vds = ref [] in
  let caseParam = ref ofCaseClass in
  let paramClause in_ =
    if in_.token =~= (RPAREN ab)
    then []
    else
      let implicitmod =
        match in_.token with
        | Kimplicit _ ->
            nextToken in_;
            (* AST: Flags.IMPLICIT *)
            1
        | _ ->
            0
      in
      commaSeparated (param owner implicitmod !caseParam) in_
  in
  newLineOptWhenFollowedBy (LPAREN ab) in_;
  while in_.token =~= (LPAREN ab) do
    let x = inParens paramClause in_ in
    vds += x;
    caseParam := false;
    newLineOptWhenFollowedBy (LPAREN ab) in_;
  done;
  if ofCaseClass then begin
    (* AST: name(), elliptical(), *)
    (* CHECK: "case classes must have a parameter list" *)
    (* CHECK: "case classes must have a non-implicit parameter list" *)
    ()
  end;
  (* CHECK: "an implicit parameter section must be last" *)
  (* CHECK: "multiple implicit parameter sections are not allowed" *)
  (* CHECK: "parameter sections are effectively implicit" *)
  let result = !vds in
  (* AST: if owner is CONSTRUCTOR && ... *)
  (* CHECK: "no type parameters allowed here" *)
  (* CHECK: "auxiliary constructor needs non-implicit parameter list" *)
  (* AST: addEvidentParams (owner, result, contextBounds) *)
  []

(* ------------------------------------------------------------------------- *)
(* Type Parameter *)
(* ------------------------------------------------------------------------- *)
(** {{{
 *  TypeParamClauseOpt    ::= [TypeParamClause]
 *  TypeParamClause       ::= `[` VariantTypeParam {`,` VariantTypeParam} `]`]
 *  VariantTypeParam      ::= {Annotation} [`+` | `-`] TypeParam
 *  FunTypeParamClauseOpt ::= [FunTypeParamClause]
 *  FunTypeParamClause    ::= `[` TypeParam {`,` TypeParam} `]`]
 *  TypeParam             ::= Id TypeParamClauseOpt TypeBounds {`<%` Type} {`:` Type}
 *  }}}
*)
let typeParamClauseOpt owner contextBoundBuf in_ =
  let typeParam ms in_ =
    failwith "typeParam" in
  newLineOptWhenFollowedBy (LBRACKET ab) in_;
  match in_.token with
  | LBRACKET _ ->
      let x = inBrackets (fun in_ ->
        let annots = annotations ~skipNewLines:true in_ in
        let mods = (* AST: NoMods withannotation annots *) [] in
        commaSeparated (typeParam mods) in_
      )
      in
      Some x
  | _ -> None

(* ------------------------------------------------------------------------- *)
(* Def *)
(* ------------------------------------------------------------------------- *)

(** {{{
 *  FunDef ::= FunSig [`:` Type] `=` [`macro`] Expr
 *          |  FunSig [nl] `{` Block `}`
 *          |  `this` ParamClause ParamClauses
 *                 (`=` ConstrExpr | [nl] ConstrBlock)
 *  FunDcl ::= FunSig [`:` Type]
 *  FunSig ::= id [FunTypeParamClause] ParamClauses
 *  }}}
*)
let funDefRest mods name in_ =
  let newmods = ref mods in
  (* contextBoundBuf is for context bounded type parameters of the form
   * [T : B] or [T : => B]; it contains the equivalent implicit parameter type,
   * i.e. (B[T] or T => B)
  *)
  let contextBoundBuf = [] in
  let tparams = typeParamClauseOpt name contextBoundBuf in_ in
  let vparamss = paramClauses ~ofCaseClass:false name contextBoundBuf in_ in
  newLineOptWhenFollowedBy (LBRACE ab) in_;
  let restype = (* AST: fromWithinReturnType *) typedOpt in_ in

  let rhs =
    match in_.token with
    | t when TH.isStatSep t || t =~= (RBRACE ab) ->
        (* CHECK: if restype = None then deprecated missing type, use : Unit *)
        (* AST: EmptyTree, newmods |= DEFERRED *)
        ()
    | LBRACE _ when restype = None ->
        (* CHECK: missing type *)
        blockExpr in_
    | EQUALS _ ->
        nextTokenAllow TH.nme_MACROkw in_;
        if (TH.isMacro in_.token) then begin
          nextToken in_;
          (* AST: newmmods |= MACRO *)
        end;
        expr in_
    | _ -> accept (EQUALS ab) in_ (* generate error message *)
  in
  (* AST: DefDef(newmods, name.toTermName, tparams, vparamss, restype, rhs) *)
  (* CHECK: "unary prefix operator definition with empty parameter list ..."*)
  ()

let funDefOrDcl mods in_ =
  nextToken in_;
  match in_.token with
  | Kthis _ ->
      failwith "funDefOrDcl this"
  | _ ->
      let name = identOrMacro in_ in
      funDefRest mods name in_

(*****************************************************************************)
(* Parsing types definitions or declarations  *)
(*****************************************************************************)
let typeDefOrDcl mods in_ =
  failwith "typeDefOrDcl"

(*****************************************************************************)
(* Parsing Def or Dcl  *)
(*****************************************************************************)

(** {{{
 *  Def    ::= val PatDef
 *           | var PatDef
 *           | def FunDef
 *           | type [nl] TypeDef
 *           | TmplDef
 *  Dcl    ::= val PatDcl
 *           | var PatDcl
 *           | def FunDcl
 *           | type [nl] TypeDcl
 *  }}}
*)
let defOrDcl mods in_ =
  (* CHECK: "lazy not allowed here. Only vals can be lazy" *)
  match in_.token with
  | Kval _ -> patDefOrDcl mods (* AST: and VAL *) in_
  | Kvar _ -> patDefOrDcl mods (* AST: and VAR and Mutable *) in_
  | Kdef _ -> funDefOrDcl mods (* AST: and DEF *) in_
  | Ktype _ -> typeDefOrDcl mods (* AST: and TYPE *) in_
  | _ -> !tmplDef_ mods in_

let nonLocalDefOrDcl in_ =
  let annots = annotations ~skipNewLines:true in_ in
  let mods = modifiers in_ in
  (* AST: mods withAnnotations annots *)
  defOrDcl mods in_

(*****************************************************************************)
(* Parsing Template (classes/traits/objects)  *)
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
         then () (* AST: Nil *)
         else error errorMsg in_
    );
    acceptStatSepOpt in_
  done;
  !stats

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
      Some ()
  | t when TH.isDefIntro t || TH.isModifier t || TH.isAnnotation t ->
      let x = nonLocalDefOrDcl in_ in
      Some ()
  | t when TH.isExprIntro t ->
      let x = statement InTemplate in_ in
      Some ()
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
     | ARROW _ ->
         (* AST: case Typed(...) self := makeSelfDef(), convertToParam *)
         nextToken in_
     | _ ->
         firstOpt := Some first;
         acceptStatSepOpt in_
    )
  end;
  let xs = templateStats in_ in
  !self, (* AST !firstOpt @ *) xs

(** {{{
 *  TemplateBody ::= [nl] `{` TemplateStatSeq `}`
 *  }}}
 * @param isPre specifies whether in early initializer (true) or not (false)
*)

let templateBody ~isPre in_ =
  let xs = inBraces (templateStatSeq ~isPre) in_ in
  (* AST: self, EmptyTree.asList *)
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
         (* AST: fold_left Apply.apply xs *)
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
       | Kwith _ (* crazy? && self eq noSelfType *) ->
           (* CHECK: "use traint parameters instead" when scala3 *)
           let _earlyDefs = body (* CHECK: map ensureEarlyDef AST: filter *) in
           nextToken in_;
           let parents = templateParents in_ in
           let (self1, body1) = templateBodyOpt ~parenMeansSyntaxError:false in_ in
           (parents, self1, (* AST: earlyDefs @ *) body1)
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
let templateOpt mods (* AST: name, constrMods, vparams *) in_ =
  let (parents, self, body) =
    match in_.token with
    | Kextends _ | SUBTYPE _ (* CHECK: deprecated in 2.12.5 *) ->
        nextToken in_;
        template in_
    | _ ->
        newLineOptWhenFollowedBy (LBRACE ab) in_;
        let (self, body) =
          let parenMeansSyntaxError =
            (* AST: mods.isTrait || name.isTermName *) true
          in
          templateBodyOpt ~parenMeansSyntaxError in_ in
        [], self, body
  in
  (* AST: Template(parents, self, anyvalConstructor()::body))
   * CHECK: "package object inheritance is deprecated"
  *)
  ()


(* ------------------------------------------------------------------------- *)
(* Object *)
(* ------------------------------------------------------------------------- *)

(** {{{
 *  ObjectDef       ::= Id ClassTemplateOpt
 *  }}}
*)
let objectDef mods (* isPackageObject=false *) in_ =
  nextToken in_; (* 'object' *)
  let name = ident in_ in
  let tmpl = templateOpt mods (* AST: if isPackageObject ... *) in_ in
  (* AST: ModuleDef (mods, name.toTermName, template) *)
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
let tmplDef mods in_ =
  match in_.token with
  | Ktrait _ -> classDef in_
  | Kclass _ -> classDef in_
  (* Caseclass -> classDef in_ *)
  | Kobject _ -> objectDef mods in_
  (* Caseobject -> objectDef in_ *)
  | _ -> error "expected start of definition" in_

(*****************************************************************************)
(* Toplevel  *)
(*****************************************************************************)

(** Hook for IDE, for top-level classes/objects. *)
let topLevelTmplDef in_ =
  let _annots = annotations ~skipNewLines:true in_ in
  let mods = modifiers in_ in
  (* AST: mods withAnnotations annots *)
  let x = tmplDef mods in_ in
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
      Some () (* AST: x::Nil *)
  | Kimport _ ->
      let x = importClause in_ in
      Some () (* AST: x *)
  | t when TH.isAnnotation t || TH.isTemplateIntro t || TH.isModifier t ->
      let x = topLevelTmplDef in_ in
      Some () (* x :: Nil *)
  | _ -> None

let topStatSeq in_ =
  statSeq ~errorMsg:"expected class or object definition" topStat in_

(*****************************************************************************)
(* Entry point  *)
(*****************************************************************************)

(* set the forward reference *)
let _ =
  template_ := template;
  tmplDef_ := tmplDef;
  ()

(** {{{
 *  CompilationUnit ::= {package QualId semi} TopStatSeq
 *  }}}
*)
let compilationUnit in_ =
  let rec topstats in_ =
    let ts = ref [] in
    while in_.token =~= (SEMI ab) do
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
                   let pack = () (* AST: makePackaging(pkg, []) *) in
                   ts += pack
               | x when TH.isStatSep x ->
                   nextToken in_;
                   let xs = topstats in_ in
                   let pack = () (* AST: makePackaging(pkg, xs) *) in
                   ts += pack
               | _ ->
                   let xs = inBraces topStatSeq in_ in
                   let pack = ()  (* AST: makePackaging(pkg, xs) *) in
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
    (* AST: resetPAckage *)
    !ts
  in
  let xs = topstats in_ in
  (* AST:  case ... makeEmptyPAckage ... *)
  ()

let parse toks =
  let in_ = mk_env toks in
  init in_;
  let xs = compilationUnit in_ in
  accept (EOF ab) in_;
  xs
