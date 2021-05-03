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
module T = Token_scala
module TH = Token_helpers_scala
open Token_scala

let logger = Logging.get_logger [(*__MODULE__*)"Parser_scala_..."]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A recursive-descent Scala parser.
 *
 * This is mostly a port of Parsers.scala, found in the Scala 2 nsc
 * compiler source, to OCaml.
 *
 * alt:
 *  - use Parsers.scala of dotty? longer source, and most code out there
 *    is still Scala2 code, so better for semgrep to use Scala 2 first.
 *  - use the one in scalameta? The code looks cleaner, but it handles
 *    more constructs because it handles also Scala3 (so it needs to manage
 *    indent/outdent). It might have been a better basis though, because on
 *    a second look (thx to Nathan) the code is far cleaner than Parsers.scala
 *    in the Scala 2 compiler source.
 *
 * Note that parsing Scala is difficult. See the crazy: tag in this file.
 * See also the newline: tag to see all the places where newlines are handled
 * in a special way.
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
 *  - advanced error diagnosis: lots of variants (deprecationWarning,
 *    syntaxError, expecting X but got Y, etc)
 *  - position/offset management (just use position info in the token, easier)
 *  - hooks for Scaladoc or IDEs
*)

(* TODO: temporary *)
[@@@warning "-27-26"]

let debug_lexer = ref false
let debug_newline = ref false

(*****************************************************************************)
(* Types  *)
(*****************************************************************************)

type env = {
  (* imitating the Scala implementation *)
  mutable token: T.token;

  (* not in Scala implementation *)
  mutable rest: T.token list;
  mutable passed: T.token list;

  mutable last_nl: Parse_info.t option;
  (* for logging *)
  mutable depth: int;
}

let mk_env toks =
  match toks with
  | [] ->
      (* should at least get EOF from the lexer *)
      raise Impossible
  | x::xs ->
      { token = x;
        (* assume we will call first nextToken on it *)
        rest = (x::xs);
        passed = [];
        last_nl = None;
        depth = 0;
      }
(* https://stackoverflow.com/questions/47688111/copy-construction-in-ocaml*)
let copy_env env =
  { env with token = env.token }

let ab = Parse_info.abstract_info

(* TODO, use AST_scala something *)
let noSelfType = ()
let empty = ()

type location =
  | Local
  | InBlock
  | InTemplate
[@@deriving show {with_path = false}]

(*****************************************************************************)
(* Logging/Dumpers  *)
(*****************************************************************************)
let dump_token tok =
  T.show tok

let n_dash n = Common2.repeat "-" n |> Common.join ""

let with_logging funcname f in_ =
  let save = in_.depth in
  in_.depth <- in_.depth + 1;
  let depth = n_dash in_.depth in
  logger#info "%s>%s: %s" depth funcname (dump_token in_.token);
  let res = f () in (* less: pass in_ ? *)
  in_.depth <- save;
  res

(*****************************************************************************)
(* Error management  *)
(*****************************************************************************)
let error x in_ =
  pr2 (dump_token in_.token);
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
(* newline: Newline management part1  *)
(* ------------------------------------------------------------------------- *)
(* pad: newlines are skipped in fetchToken but reinserted in insertNL
 * in certain complex conditions.
*)

let adjustSepRegions lastToken in_ =
  pr2_once "adjustSepRegions"

(* newline: pad: I've added the ~newlines param to differentiante adding
 * a NEWLINE or NEWLINES *)
let insertNL ?(newlines=false) in_ =
  if !debug_newline then begin
    logger#info "%s: %s" "insertNL" (dump_token in_.token);
    logger#info "inserting back a newline:%s" (Common.dump in_.last_nl);
  end;
  match in_.last_nl with
  | None -> failwith "no last newline to insert back"
  | Some x ->
      in_.rest <- in_.token::in_.rest;
      in_.token <- if newlines then NEWLINES x else NEWLINE x;
      ()

(** Is current token first one after a newline? *)
let afterLineEnd in_ =
  let rec loop passed =
    match passed with
    | x::xs ->
        (match x with
         | Nl _ | NEWLINE _ | NEWLINES _ -> true
         | Space _ | Comment _ -> loop xs
         | _ ->
             if !debug_newline
             then logger#info "%s: false because %s" "afterLineEnd" (dump_token x);
             false
        )
    | [] -> false
  in
  loop in_.passed
  |> (fun b ->
    if !debug_newline
    then logger#info "%s: %s, result = %b" "afterLineEnd" (dump_token in_.token) b;
    b)

(* ------------------------------------------------------------------------- *)
(* Lexing tricks *)
(* ------------------------------------------------------------------------- *)
let postProcessToken in_ =
  pr2_once "postProcessToken";
  ()

(* ------------------------------------------------------------------------- *)
(* fetchToken *)
(* ------------------------------------------------------------------------- *)
let fetchToken in_ =

  let rec loop aux =
    match in_.rest with
    | [] -> failwith "fetchToken: no more tokens"
    | x::xs ->
        if !debug_lexer then logger#info "fetchToken: %s" (dump_token x);

        in_.rest <- xs;

        (match x with
         | Space _ | Comment _ ->
             loop (x::aux)
         (* pad: the newline is skipped here, but reinserted conditionally in
          * insertNL() *)
         | Nl info ->
             in_.last_nl <- Some info;
             loop (x::aux)

         (* those tokens are inserted post tokenization *)
         | NEWLINE _ | NEWLINES _ ->
             raise Impossible

         | other ->
             in_.passed <- aux @ in_.passed;
             in_.token <- other;
        )
  in
  loop [in_.token]

(* ------------------------------------------------------------------------- *)
(* nextToken *)
(* ------------------------------------------------------------------------- *)
(** Consume and discard the next token. *)
let nextToken in_ =
  let lastToken = in_.token in
  adjustSepRegions lastToken in_;
  (* TODO: if inStringInterpolation fetchStringPart else *)
  fetchToken in_;
  (* Insert NEWLINE or NEWLINES if
   * - we are after a newline
   * - we are within a { ... } or on toplevel (wrt sepRegions)
   * - the current token can start a statement and the one before can end it
   * insert NEWLINES if we are past a blank line, NEWLINE otherwise
  *)
  (* newline: *)
  if afterLineEnd in_ &&
     TH.inLastOfStat lastToken &&
     TH.inFirstOfStat in_.token &&
     (* TODO: sepRegions stuff *)
     (* TODO: not applyBracePatch *)
     true
  then begin
    match () with
    (* TODO: | _ when pastBlankLine in_ -> insertNL ~newlines:true in_ *)
    (* TODO  | _ when TH.isLeadingInfixOperator *)
    (* CHECK: scala3: "Line starts with an operator that in future" *)
    | _ ->
        insertNL in_
  end;
  postProcessToken in_;
  ()

let nextTokenAllow next in_ =
  pr2 "nextTokenAllow: TODO";
  nextToken in_

let skipToken in_ =
  nextToken in_

let init in_ =
  nextToken in_

(* Consume one token of the specified type, or signal an error if it is
 * not there *)
let accept t in_ =
  if not (in_.token =~= t)
  then error (spf "was expecting: %s" (dump_token t)) in_;
  (match t with
   | EOF _ -> ()
   | _ -> nextToken in_
  )

(* ------------------------------------------------------------------------- *)
(* looking Ahead *)
(* ------------------------------------------------------------------------- *)

(* was called in.next.token *)
let (*rec*) next_next_token in_ =
  match in_.rest with
  | [] -> None
  (* TODO: also skip spacing and stuff? *)
  | x::xs -> Some x

let lookingAhead body in_ =
  (* CHECK: allowLeadingInfixOperators *)
  let in_' = copy_env in_ in
  nextToken in_';
  let res = body in_' in
  res



(*****************************************************************************)
(* Special parsing  *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* newline: Newline management part2  *)
(* ------------------------------------------------------------------------- *)

(** {{{
 *  semi = nl {nl} | `;`
 *  nl  = `\n` // where allowed
 *  }}}
*)
let acceptStatSep in_ =
  in_ |> with_logging "acceptStatSep" (fun () ->
    match in_.token with
    | NEWLINE _ | NEWLINES _ -> nextToken in_
    | _ -> accept (SEMI ab) in_
  )

let acceptStatSepOpt in_ =
  if not (TH.isStatSeqEnd in_.token)
  then acceptStatSep in_


let newLineOpt in_ =
  match in_.token with
  | NEWLINE _ -> nextToken in_
  | _ -> ()

let newLineOptWhenFollowedBy token in_ =
  match in_.token, next_next_token in_ with
  | NEWLINE _, Some x when x =~= token -> newLineOpt in_
  | _ -> ()

let newLineOptWhenFollowing token in_ =
  pr2 "newLineOptWhenFollowing: TODO";
  newLineOpt in_

(* ------------------------------------------------------------------------- *)
(* Trailing commas  *)
(* ------------------------------------------------------------------------- *)
(* pad: trailing commas are _detected_ in separatedToken to not cause
 * separatedToken to call part another time, but they are _skipped_ in
 * inGroupers.
*)

(* used by parser to distinguish pattern P(_*, p) from trailing comma.
 * EOF is accepted for REPL, which can't look ahead past the current line.
*)
let isTrailingComma right in_ =
  in_.token =~= (COMMA ab) && lookingAhead (fun in_ ->
    afterLineEnd in_ && in_.token =~= right (* REPL: || token =~= EOF *)
  ) in_

(* advance past COMMA NEWLINE RBRACE (to whichever token is the matching
 * close bracket)
*)
(* supposed to return a boolean *)
let skipTrailingComma right in_ =
  match in_.token with
  | COMMA _ when isTrailingComma right in_ ->
      (* SIP-27 Trailing Comma (multi-line only) support
       * If a comma is followed by a new line & then a closing paren,
       * bracket or brace
       * then it is a trailing comma and is ignored
      *)
      (* pad: why not skipToken? don't want side effects in nextToken? *)
      fetchToken in_
  | _ ->
      ()


(* ------------------------------------------------------------------------- *)
(* Context sensitive parsing  *)
(* ------------------------------------------------------------------------- *)
let noSeq f in_ =
  pr2 "noSeq: TODO";
  f in_

let outPattern f in_ =
  pr2 "outPattern: TODO";
  f in_

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
  in_ |> with_logging "inBraces" (fun () ->
    inGroupers (LBRACE ab) (RBRACE ab) f in_
  )
let inParens f in_ =
  in_ |> with_logging "inParens" (fun () ->
    inGroupers (LPAREN ab) (RPAREN ab) f in_
  )
let inBrackets f in_ =
  in_ |> with_logging "inBrackets" (fun () ->
    inGroupers (LBRACKET ab) (RBRACKET ab) f in_
  )

(* less: do actual error recovery? *)
let inBracesOrNil = inBraces

(** {{{ { `sep` part } }}}. *)
let separatedToken sep part in_ =
  (* CHECK: "separator cannot be a comma" *)
  in_ |> with_logging (spf "separatedTopen(%s)" (dump_token sep)) (fun () ->
    let ts = ref [] in
    while in_.token =~= sep do
      nextToken in_;
      let x = part in_ in
      ts += x;
    done;
    !ts
  )

(** {{{ part { `sep` part } }}}. *)
let tokenSeparated separator part in_ =
  in_ |> with_logging (spf "tokenSeparated(%s)" (dump_token separator)) (fun () ->
    let ts = ref [] in
    let x = part in_ in
    ts += x;
    let done_ = ref (not (in_.token =~= separator)) in
    while not !done_  do
      let skippable = separator =~= (COMMA ab) &&
                      (* TODO: in.sepRegions nonEmpty and head *)
                      isTrailingComma (RPAREN ab) in_ in
      if not skippable then begin
        nextToken in_;
        let x = part in_ in
        ts += x
      end;
      done_ := not (in_.token =~= separator) || skippable;
    done;
    !ts
    (* old:
       while in_.token =~= separator do
        nextToken in_;
        let x = part in_ in
        ts += x
       done
    *)
  )

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

let selector t in_ =
  let id = ident in_ in
  (* AST: Select(t, id) *)
  ()

let rec selectors ~typeOK t in_ =
  match in_.token with
  | Ktype _ when typeOK ->
      nextToken in_;
      (* AST: SingletonTypeTree(t) *)
  | _ ->
      let t1 = selector t in_ in
      if in_.token =~= (DOT ab)
      then begin
        skipToken in_;
        selectors ~typeOK t1 in_
      end
      else t1


(** {{{
 *   QualId ::= Id {`.` Id}
 *   }}}
*)
let qualId in_ =
  (* AST: Ident(id) *)
  let id = ident in_ in
  let id = () in (* TODO: AST Ident?) *)
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



(** {{{
 *  Path       ::= StableId
 *              |  [Ident `.`] this
 *  AnnotType ::= Path [`.` type]
 *  }}}
*)
let path ~thisOK ~typeOK in_ =
  in_ |> with_logging (spf "path(thisOK:%b, typeOK:%b)" thisOK typeOK) (fun()->
    let t = ref () in
    match in_.token with
    | Kthis _ -> failwith "path.this"
    | Ksuper _ -> failwith "path.super"
    | _ ->
        let name = ident in_ in
        (* AST: t := Ident(name) and special stuff in BACKQUOTED_IDENT *)
        if in_.token =~= (DOT ab) then begin
          skipToken in_;
          match in_.token with
          | Kthis _ -> failwith "path.this 2"
          | Ksuper _ -> failwith "path.super 2"
          | _ ->
              let x = selectors ~typeOK !t in_ in
              t := x
        end;
        !t
  )
(*****************************************************************************)
(* Forward references  *)
(*****************************************************************************)
(* alt: have all functions mutually recursive, but it feels cleaner
 * to reduce those set of mutual dependencies
*)
(* less: move after types? after expr? *)
let template_ = ref (fun _ -> failwith "forward ref not set")
let tmplDef_ = ref (fun _ -> failwith "forward ref not set")
let blockStatSeq_ = ref (fun _ -> failwith "forward ref not set")
let topLevelTmplDef_ = ref (fun _ -> failwith "forward ref not set")
(* less: for types, maybe could move literal up *)
let literal_  =
  ref (fun ?(isNegated=false) ?(inPattern=false) _ ->
    failwith "forward ref not set")

(*****************************************************************************)
(* Parsing types  *)
(*****************************************************************************)

let wildcardType in_ =
  failwith "wildcardType"

(* ------------------------------------------------------------------------- *)
(* TODO: in PatternContextSensitive *)
(* ------------------------------------------------------------------------- *)

let typ in_ =
  pr2 "typ: TODO";
  ident in_

(** {{{
 *  SimpleType       ::=  SimpleType TypeArgs
 *                     |  SimpleType `#` Id
 *                     |  StableId
 *                     |  Path `.` type
 *                     |  Literal
 *                     |  `(` Types `)`
 *                     |  WildcardType
 *  }}}
*)
let rec simpleType in_ =
  in_ |> with_logging "simpleType" (fun () ->
    match in_.token with
    | t when TH.isLiteral t && not (t =~= (Knull ab)) ->
        let x = !literal_ in_ in
        (* AST: SingletonTypeTree(x) *)
        ()
    | MINUS _ when lookingAhead (fun in_ -> TH.isNumericLit in_.token) in_ ->
        nextToken in_;
        let x = !literal_ ~isNegated:true in_ in
        (* AST: SingletonTypeTree(x) *)
        ()
    | _ ->
        let start =
          match in_.token with
          | LPAREN _ ->
              (* CHECK: "Illegal literal type (), use Unit instead" *)
              let xs = inParens types in_ in
              (* AST: makeSafeTupleType *)
              ()
          | t when TH.isWildcardType t ->
              skipToken in_;
              wildcardType in_
          | _ ->
              let x = path ~thisOK:false ~typeOK:true in_ in
              (* AST: convertToTypeId if not SingletonTypeTree *)
              ()
        in
        simpleTypeRest start in_
  )

and simpleTypeRest t in_ =
  match in_.token with
  | HASH _ ->
      let x = typeProjection t in_ in
      simpleTypeRest x in_
  | LBRACKET _ ->
      let xs = typeArgs in_ in
      let x = () in (* AST: AppliedTypeTree(t, xs) *)
      simpleTypeRest x in_
  | _ -> t

and typeProjection t in_ =
  failwith "typeProjection"

(** {{{
 *  TypeArgs    ::= `[` ArgType {`,` ArgType} `]`
 *  }}}
*)
and typeArgs in_ =
  inBrackets types in_

(** {{{
 *  Types ::= Type {`,` Type}
 *  }}}
*)
and types in_ =
  failwith "types"


(* ------------------------------------------------------------------------- *)
(* Outside PatternContextSensitive *)
(* ------------------------------------------------------------------------- *)

let typ x = outPattern typ x
let exprSimpleType x = outPattern simpleType x

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

(* ------------------------------------------------------------------------- *)
(* Inside ??? *)
(* ------------------------------------------------------------------------- *)

let pattern in_ =
  in_ |> with_logging "pattern" (fun () ->
    failwith "pattern"
  )

(** {{{
 *  Pattern3    ::= SimplePattern
 *                |  SimplePattern {Id [nl] SimplePattern}
 *  }}}
*)
let pattern3 in_ =
  pr2 "pattern3: TODO";
  ident in_

(** {{{
 *  Pattern2    ::=  id  `@` Pattern3
 *                |  `_` `@` Pattern3
 *                |   Pattern3
 *  }}}
*)
let pattern2 in_ =
  let x = pattern3 in_ in
  match in_.token with
  | AT _ -> (* TODO: case p @ Ident(name) *)
      nextToken in_;
      let body = pattern3 in_ in
      (* AST: Bind(name, body), if WILDCARD then ... *)
      body
  | _ -> x

let caseClauses in_ =
  failwith "caseClauses"

(* ------------------------------------------------------------------------- *)
(* Outside ??? *)
(* ------------------------------------------------------------------------- *)
let _pattern in_ =
  noSeq pattern in_

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
  in_ |> with_logging(spf "expr0(location = %s)"(show_location location))(fun() ->
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
  )

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
  in_ |> with_logging "postfixExpr" (fun () ->
    (* TODO: AST: opstack, reduce *)
    let rec loop top in_ =
      in_ |> with_logging "postfixExpr:loop" (fun () ->
        if not (TH.isIdentBool in_.token)
        then
          in_ |> with_logging "postfixExpr:loop: noIdentBool, stop" (fun () ->
            top
          )
        else begin
          (* isIdentBool is true; remember that operators are identifiers and
           * that Scala allows any identifier in infix position
          *)
          (* AST: pushOpInfo(reduceExprStack(base, top)) *)
          pushOpInfo top in_;
          newLineOptWhenFollowing (TH.isExprIntro) in_;
          if TH.isExprIntro in_.token then begin
            let res = prefixExpr in_ in
            match res with
            (*| None -> (* AST: reduceExprStack(base, top) *) None
              | Some *) next ->
                loop next in_
          end
          else
            in_ |> with_logging "postfixExpr:loop: noExprIntro, stop" (fun () ->
              (* AST: finishPostfixOp(base, popOpInfo()) *)
              ()
            )
        end
      )
    in
    let res = prefixExpr in_ in
    (* AST: reduceExprStack (res) *)
    loop res in_
  )

and pushOpInfo top in_ =
  let x = ident in_ in
  let targs =
    if in_.token =~= (LBRACKET ab)
    then exprTypeArgs in_
    else () (* AST: Nil *)
  in
  (* AST: OpInfo(top, name, targs) *)
  ()

and exprTypeArgs in_ =
  failwith "exprTypeArgs"

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
  in_ |> with_logging "simpleExpr" (fun () ->
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
  )

and simpleExprRest ~canApply t in_ =
  in_ |> with_logging (spf "simpleExprRest(canApply:%b)" canApply) (fun () ->
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
    | _ ->
        in_ |> with_logging "simpleExprRest: nothing to consume" (fun () ->
          t
        )
  )


(** {{{
 *  SimpleExpr    ::= literal
 *                  | symbol
 *                  | null
 *  }}}
*)
and literal ?(isNegated=false) ?(inPattern=false) in_ =
  in_ |> with_logging (spf "literal(isNegated:%b, inPattern:%b)"
                         isNegated inPattern) (fun () ->
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
  )

and interpolatedString ~inPattern in_ =
  failwith "interpolatedString"


(* AST: *)
and freshPlaceholder in_ =
  nextToken in_;
  ()


(* ------------------------------------------------------------------------- *)
(* Arguments *)
(* ------------------------------------------------------------------------- *)
and multipleArgumentExprs in_ =
  in_ |> with_logging "multipleArgumentExprs" (fun () ->
    match in_.token with
    | LPAREN _ ->
        let x = argumentExprs in_ in
        let xs = multipleArgumentExprs in_ in
        x::xs
    | _ -> []
  )

(** {{{
 *  ArgumentExprs ::= `(` [Exprs] `)`
 *                  | [nl] BlockExpr
 *  }}}
*)
and argumentExprs in_ : unit list =
  in_ |> with_logging "argumentExprs" (fun () ->
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
  )

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

(** {{{
 *  Block ::= BlockStatSeq
 *  }}}
 *  @note  Return tree does not carry position.
*)

and block in_ =
  let xs = !blockStatSeq_ in_ in
  (* AST: makeBlock(xs) *)
  ()

(** {{{
  *  BlockExpr ::= `{` (CaseClauses | Block) `}`
  *  }}}
*)
and blockExpr in_ =
  inBraces (fun in_ ->
    match in_.token with
    | Kcase _ ->
        let xs = caseClauses in_ in
        (* AST: Match(EmptyTree, xs *)
        ()
    | _ -> block in_
  ) in_


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
  let t = exprSimpleType in_ in
  if in_.token =~= (LPAREN ab)
  then
    let xs = multipleArgumentExprs in_ in
    (* AST: New (t, xs) *)
    ()
  else () (* AST: New(t, Nil) *)

and annotations ~skipNewLines in_ =
  in_ |> with_logging (spf "annotations(skipNewLines:%b)" skipNewLines)(fun()->
    readAnnots (fun in_ ->
      let t = annotationExpr in_ in
      if skipNewLines then newLineOpt in_;
      t
    ) in_
  )


(** {{{
 *  AnnotType        ::=  SimpleType {Annotation}
 *  }}}
*)
let annotTypeRest t in_ =
  let xs = annotations ~skipNewLines:false in
  (* AST: fold around t makeAnnotated *)
  t

(* TODO? was in PatternContextSensitive trait *)
let annotType in_ =
  (* CHECK: placeholderTypeBoundary *)
  let x = simpleType in_ in
  annotTypeRest x in_


let startAnnotType in_ =
  outPattern annotType in_

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
  in_ |> with_logging "importExpr" (fun () ->
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
  )

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
   | _ -> ()
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

(** {{{
 *  LocalModifiers ::= {LocalModifier}
 *  LocalModifier  ::= abstract | final | sealed | implicit | lazy
 *  }}}
*)
let localModifiers in_ =
  let rec loop mods in_ =
    if TH.isLocalModifier in_.token then
      let mods = addMod mods in_.token in_ in
      loop mods in_
    else mods
  in
  loop [] in_


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
  in_ |> with_logging "param" (fun () ->
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
  )

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
  *  PatDef ::= Pattern2 {`,` Pattern2} [`:` Type] `=` Expr
  *  ValDcl ::= Id {`,` Id} `:` Type
  *  VarDef ::= PatDef | Id {`,` Id} `:` Type `=` `_`
  *  }}}
*)
let patDefOrDcl mods in_ =
  let newmods = ref mods in
  nextToken in_;
  let lhs =
    commaSeparated (fun in_ ->
      let x = noSeq pattern2 in_ in
      stripParens x
    ) in_ in
  let tp = typedOpt in_ in
  let rhs =
    match in_.token with
    | EQUALS _ ->
        nextToken in_;
        let e = expr in_ in
        (* CHECK: "default initialization prohibited for literal-typed vars"*)
        (* AST: newmods |= DEFAULTINIT *)
        Some e
    | _ ->
        (* CHECK: if tp.isEmpty then accept EQUALS (* raise error *) *)
        (* AST: newmods |= DEFERRED *)
        None (* AST: EmptyTree *)
  in
  (* CHECK: ensureNonOverlapping *)
  (* CHECK: "lazy values may not be abstract" *)
  (* CHECK: "pattern definition may not be abstract" *)
  (* AST: mkDefs (...) *)
  ()


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
  in_ |> with_logging "defOrDcl" (fun () ->
    (* CHECK: "lazy not allowed here. Only vals can be lazy" *)
    match in_.token with
    | Kval _ -> patDefOrDcl mods (* AST: and VAL *) in_
    | Kvar _ -> patDefOrDcl mods (* AST: and VAR and Mutable *) in_
    | Kdef _ -> funDefOrDcl mods (* AST: and DEF *) in_
    | Ktype _ -> typeDefOrDcl mods (* AST: and TYPE *) in_
    | _ -> !tmplDef_ mods in_
  )

let nonLocalDefOrDcl in_ =
  in_ |> with_logging "nonLocalDefOrDcl" (fun () ->
    let annots = annotations ~skipNewLines:true in_ in
    let mods = modifiers in_ in
    (* AST: mods withAnnotations annots *)
    defOrDcl mods in_
  )

let localDef implicitMod in_ =
  in_ |> with_logging "localDef" (fun () ->
    let annots = annotations ~skipNewLines:true in_ in
    let mods = localModifiers in_ in
    (* AST: let mods = mods | implicitMod withAnnotations annots *)
    (* crazy? parsing depends on AST *)
    let defs =
      (* TODO !(mods hasFlag ~(Flags.IMPLICIT | Flags.LAZY))) *)
      defOrDcl mods in_
      (* TODO: else tmplDef mods in_ *)
    in
    defs
    (* AST: if RBRACE | CASE defs :+ literalUnit *)
  )

(*****************************************************************************)
(* Parsing XxxStat and XxxStats  *)
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
(* BlockStat *)
(* ------------------------------------------------------------------------- *)
(** {{{
  *  BlockStatSeq ::= { BlockStat semi } [ResultExpr]
  *  BlockStat    ::= Import
  *                 | Annotations [implicit] [lazy] Def
  *                 | Annotations LocalModifiers TmplDef
  *                 | Expr1
  *                 |
  *  }}}
*)

let blockStatSeq in_ =
  let acceptStatSepOptOrEndCase in_ =
    if not (TH.isCaseDefEnd in_.token)
    then acceptStatSepOpt in_
  in
  let stats = ref [] in
  while not (TH.isStatSeqEnd in_.token) && not (TH.isCaseDefEnd in_.token) do
    match in_.token with
    | Kimport _ ->
        let xs = importClause in_ in
        stats ++= xs;
        acceptStatSepOptOrEndCase in_
    | t when TH.isDefIntro t || TH.isLocalModifier t || TH.isAnnotation t ->
        (match in_.token with
         | Kimplicit _ ->
             skipToken in_;
             let xs =
               if TH.isIdentBool in_.token
               then implicitClosure InBlock in_
               else localDef ()(*AST: Flags.IMPLICIT*) in_
             in
             stats ++= xs
         | _ ->
             let xs = localDef () in_ in
             stats ++= xs
        );
        acceptStatSepOptOrEndCase in_
    | t when TH.isExprIntro t ->
        let x = statement InBlock in_ in
        stats += x;
        acceptStatSepOptOrEndCase in_
    | t when TH.isStatSep t ->
        nextToken in_
    | t when TH.isModifier t ->
        error "no modifiers allowed here" in_
    | _ ->
        error "illegal start of statement" in_
  done;
  !stats

(* ------------------------------------------------------------------------- *)
(* TemplateStat *)
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

(* ------------------------------------------------------------------------- *)
(* TopStat *)
(* ------------------------------------------------------------------------- *)

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
      let x = !topLevelTmplDef_ in_ in
      Some () (* x :: Nil *)
  | _ -> None

let topStatSeq in_ =
  statSeq ~errorMsg:"expected class or object definition" topStat in_

(*****************************************************************************)
(* Parsing Template (classes/traits/objects)  *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* "Template" *)
(* ------------------------------------------------------------------------- *)

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
  in_ |> with_logging "template" (fun () ->
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
  )

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
let classDef mods in_ =
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
  (* CHECK: "classes cannot be lazy" *)
  match in_.token with
  | Ktrait _ -> classDef mods (* AST: | TRAIT | ABSTRACT *) in_
  | Kclass _ -> classDef mods in_
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

(*****************************************************************************)
(* Entry point  *)
(*****************************************************************************)

(* set the forward reference *)
let _ =
  template_ := template;
  tmplDef_ := tmplDef;
  blockStatSeq_ := blockStatSeq;
  topLevelTmplDef_ := topLevelTmplDef;
  literal_ := literal;
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
               (* newline: needed here otherwise parsed as package def *)
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
  (* AST:  case ... makeEmptyPackage ... *)
  ()

let parse toks =
  let in_ = mk_env toks in
  init in_;
  let xs = compilationUnit in_ in
  accept (EOF ab) in_;
  xs
