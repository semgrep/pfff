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
module Flag = Flag_parsing

open Token_scala
open AST_scala

let logger = Logging.get_logger [(*__MODULE__*)"Parser_scala_..."]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A recursive-descent Scala parser.
 *
 * This is mostly an OCaml port of the Scala2 parser found in
 * https://github.com/scala/scala/src/compiler/scala/tools/nsc/ast/parser/Parsers.scala
 *
 * alt:
 *  - use Parsers.scala of dotty? but the source is longer, and most of
 *    the code out there is still Scala2 code, so it's better for semgrep
 *    to focus on Scala 2 first.
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
 *    (I use ast: when I return something from AST_scala.ml to cover the
 *     construct)
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

(* See also Flag_parsing.debug_parser and Flag_parsing.debug_lexer *)
let debug_newline = ref false

(*****************************************************************************)
(* Types  *)
(*****************************************************************************)

type env = {
  (* imitating the Scala implementation *)
  mutable token: T.token;

  (** a stack of tokens which indicates whether line-ends can be statement
   *  separators also used for keeping track of nesting levels.
   *  We keep track of the closing symbol of a region. This can be
   *  RPAREN    if region starts with '('
   *  RBRACKET  if region starts with '['
   *  RBRACE    if region starts with '{'
   *  ARROW     if region starts with 'case'
   *  STRINGLIT if region is a string interpolation expression starting
   *   with '${' (the STRINGLIT appears twice in succession on the stack iff
   *              the expression is a multiline string literal).
  *)
  mutable sepRegions: T.token list;

  (* not in Scala implementation *)
  mutable rest: T.token list;
  mutable passed: T.token list;

  (* newline: last newline token *)
  mutable last_nl: Parse_info.t option;

  (* for logging *)
  mutable depth: int;
}

let mk_env toks =
  match toks with
  | [] ->
      (* we should at least get an EOF token from the lexer *)
      raise Impossible
  | x::xs ->
      { token = x;
        (* this assumes we will call first nextToken on it *)
        rest = (x::xs);
        passed = [];
        last_nl = None;
        depth = 0;
        sepRegions = [];
      }

(* We sometimes need to lookahead tokens, and call fetchToken and revert;
 * To do that we copy the environment to run fetchToken on a fresh copy. See
 * https://stackoverflow.com/questions/47688111/copy-construction-in-ocaml
*)
let copy_env env =
  { env with token = env.token }

(* Trick to use = (called =~= below) to compare tokens *)
let ab = Parse_info.abstract_info
let fb = Parse_info.fake_bracket

(* AST: use AST_scala something *)
let noSelfType = ()
let noMods = []

(* crazy? context-sensitive parsing? *)
type location =
  | Local
  | InBlock
  | InTemplate
[@@deriving show {with_path = false}]

(* to correctly handle infix operators (in expressions, patterns, and types)*)
type mode =
  | FirstOp
  | LeftOp
  | RightOp
[@@deriving show {with_path = false}]

(*****************************************************************************)
(* Logging/Dumpers  *)
(*****************************************************************************)
let n_dash n =
  Common2.repeat "--" n |> Common.join ""

let with_logging funcname f in_ =
  if !Flag.debug_parser then begin
    let save = in_.depth in
    in_.depth <- in_.depth + 1;
    let depth = n_dash in_.depth in
    logger#info "%s>%s: %s" depth funcname (T.show in_.token);
    let res = f () in (* less: pass in_ ? *)
    logger#info "%s<%s: %s" depth funcname (T.show in_.token);
    in_.depth <- save;
    res
  end
  else f ()

(*****************************************************************************)
(* Error management  *)
(*****************************************************************************)
let error x in_ =
  let tok = in_.token in
  let info = TH.info_of_tok tok in
  if !Flag.debug_parser then begin
    pr2 (T.show tok);
    pr2 x;
  end;
  raise (Parse_info.Parsing_error info)

let todo x in_ =
  error ("TODO:" ^x) in_
let warning s =
  if !Flag.debug_parser
  then pr2 ("WARNING: " ^ s)

(*****************************************************************************)
(* Helpers  *)
(*****************************************************************************)
(* less: assert t1 is not a token with value (s, info) *)
let (=~=) t1 t2 =
  (TH.abstract_info_tok t1 =*= TH.abstract_info_tok t2)

(* to imitate Parsers.scala *)
let (++=) aref xs =
  ()

let (+=) aref x =
  ()

(*****************************************************************************)
(* Token helpers  *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* looking ahead part 1 *)
(* ------------------------------------------------------------------------- *)

(* was called in.next.token *)
let rec in_next_token xs =
  match xs with
  | [] -> None
  | x::xs ->
      (match x with
       | Space _  | Comment _ -> in_next_token xs
       | _ ->  Some x
      )

(* ------------------------------------------------------------------------- *)
(* newline: Newline management part1  *)
(* ------------------------------------------------------------------------- *)
(* pad: newlines are skipped in fetchToken but reinserted in insertNL
 * in certain complex conditions.
*)

(** Adapt sepRegions according to last token.
  * pad: called just at the beginning of nextToken so
  * in_.token = lastToken
*)
let adjustSepRegions lastToken in_ =
  let newRegions =
    match lastToken, in_.sepRegions with
    | LPAREN info, xs -> (RPAREN info)::xs
    | LBRACKET info, xs -> (RBRACKET info)::xs
    | LBRACE info, xs -> (RBRACE info)::xs

    | Kcase info, xs ->
        (* pad: the original code generate different tokens for
         * 'case object' and 'case class' in postProcessToken, I guess
         * to simplify code here. Instead I lookahead here and have
         * a simpler postProcessToken.
        *)

        (* ugly: if 'case' is the first token of the file, it will actually
         * still be in in_.rest because of the way mk_env currently works.
         * so we double check this initial condition here with =*=
         * (on purpose not =~=)
        *)
        let rest =
          match in_.rest with
          | x::xs when x =*= lastToken -> xs
          | xs -> xs
        in
        (match in_next_token (rest) with
         | Some (Kobject _ | Kclass _) -> xs
         | Some x ->
             if !debug_newline
             then logger#info "found case for arrow, next = %s" (T.show x);
             (ARROW info)::xs
         | None -> xs
        )

    (* pad: the original code does something different for RBRACE,
     * I think for error recovery, and it does not raise an
     * error for mismatch.
    *)
    | (RPAREN _ | RBRACKET _ | RBRACE _), [] ->
        (* stricter: original code just does nothing *)
        error "unmatched closing token" in_
    | (RPAREN _ | RBRACKET _ | RBRACE _) as x, y::ys ->
        if x =~= y
        then ys
        else
          (* stricter: original code just does nothing *)
          error "unmatched closing token" in_
    (* pad: not that an arrow can also be used outside of a case, so
     * here we dont raise an error if we don't find a match.
    *)
    | ARROW _ as x, y::ys when x =~= y ->
        ys
    | _, xs -> xs
  in
  in_.sepRegions <- newRegions;
  ()

(* newline: pad: I've added the ~newlines param to differentiante adding
 * a NEWLINE or NEWLINES *)
let insertNL ?(newlines=false) in_ =
  if !debug_newline then begin
    logger#info "%s: %s" "insertNL" (T.show in_.token);
    logger#info "inserting back a newline:%s" (Common.dump in_.last_nl);
  end;
  match in_.last_nl with
  | None -> todo "no last newline to insert back" in_
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
             then logger#info "%s: false because %s" "afterLineEnd" (T.show x);
             false
        )
    | [] -> false
  in
  loop in_.passed
  |> (fun b ->
    if !debug_newline
    then logger#info "%s: %s, result = %b" "afterLineEnd" (T.show in_.token) b;
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
    | [] -> todo "fetchToken: no more tokens" in_
    | x::xs ->
        if !Flag.debug_lexer then logger#info "fetchToken: %s" (T.show x);

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
     (match in_.sepRegions with [] | (RBRACE _)::_ -> true | _ -> false) &&
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
  warning "nextTokenAllow: TODO";
  nextToken in_

let skipToken in_ =
  nextToken in_

let init in_ =
  nextToken in_

(* Consume one token of the specified type, or signal an error if it is
 * not there *)
let accept t in_ =
  if not (in_.token =~= t)
  then error (spf "was expecting: %s" (T.show t)) in_;
  (match t with
   | EOF _ -> ()
   | _ -> nextToken in_
  )

(* ------------------------------------------------------------------------- *)
(* looking ahead part 2 *)
(* ------------------------------------------------------------------------- *)

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
let newLinesOpt in_ =
  match in_.token with
  | NEWLINE _ | NEWLINES _ -> nextToken in_
  | _ -> ()

let newLineOptWhenFollowedBy token in_ =
  match in_.token, in_next_token in_.rest with
  | NEWLINE _, Some x when x =~= token -> newLineOpt in_
  | _ -> ()
let newLineOptWhenFollowing ftok in_ =
  match in_.token, in_next_token in_.rest with
  | NEWLINE _, Some x when ftok x -> newLineOpt in_
  | _ -> ()

(* ------------------------------------------------------------------------- *)
(* Trailing commas  *)
(* ------------------------------------------------------------------------- *)
(* pad: trailing commas are _detected_ in separatedToken to not cause
 * separatedToken to call part another time, but they are _skipped_ in
 * inGroupers.
*)

(** used by parser to distinguish pattern P(_*, p) from trailing comma.
 *  EOF is accepted for REPL, which can't look ahead past the current line.
*)
let isTrailingComma right in_ =
  in_.token =~= (COMMA ab) && lookingAhead (fun in_ ->
    afterLineEnd in_ && in_.token =~= right (* REPL: || token =~= EOF *)
  ) in_

(* advance past COMMA NEWLINE RBRACE (to whichever token is the matching
 * close bracket)
*)
(* pad: this was returning a boolean in the original code *)
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
(* The implementation for parsing inside of patterns at points where
 * sequences are disallowed.
*)
let noSeq f in_ =
  warning "noSeq: TODO";
  f in_

(* The implementation for parsing inside of patterns at points where
 * sequences are allowed.
*)
let seqOK f in_ =
  warning "seqOK: TODO";
  f in_

(* The implementation of the context sensitive methods for parsing
 * outside of patterns.
*)
let outPattern f in_ =
  warning "outPattern: TODO";
  f in_

(*****************************************************************************)
(* Grammar helpers  *)
(*****************************************************************************)

let inGroupers left right body in_ =
  let lp = TH.info_of_tok in_.token in
  accept left in_;
  let res = body in_ in
  skipTrailingComma right in_;
  let rp = TH.info_of_tok in_.token in
  accept right in_;
  lp, res, rp

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
  in_ |> with_logging (spf "separatedTopen(%s)" (T.show sep)) (fun () ->
    let ts = ref [] in
    while in_.token =~= sep do
      let ii = TH.info_of_tok in_.token in
      nextToken in_;
      let x = part ii in_ in
      ts += x;
    done;
    !ts
  )

(** {{{ part { `sep` part } }}}. *)
let tokenSeparated separator part in_ =
  in_ |> with_logging (spf "tokenSeparated(%s)" (T.show separator)) (fun () ->
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

let caseSeparated part in_ =
  separatedToken (Kcase ab) part in_

(*****************************************************************************)
(* Parsing names  *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Idents *)
(* ------------------------------------------------------------------------- *)

(* AST: Assumed to be TermNames *)
let ident in_ : ident =
  match TH.isIdent in_.token with
  | Some (s, info) ->
      nextToken in_;
      (s, info)
  | None ->
      error "expecting ident" in_

let rawIdent in_ : ident =
  (* AST: in.name *)
  let s = "" in
  nextToken in_;
  "", Parse_info.fake_info "RAW"

let identOrMacro in_ : ident =
  if (TH.isMacro in_.token)
  then ident in_
  else rawIdent in_

let wildcardOrIdent in_ : ident =
  match in_.token with
  | USCORE ii ->
      nextToken in_;
      (* AST: nme.WILDCARD *)
      ("_", ii)
  | _ -> ident in_

(* For when it's known already to be a type name. *)
let identForType in_ : ident =
  let x = ident in_ in
  (* AST: x.toTypeName *)
  x

(* ------------------------------------------------------------------------- *)
(* Selectors *)
(* ------------------------------------------------------------------------- *)

let selector in_ : ident =
  let id = ident in_ in
  (* ast: Select(t, id) *)
  id

let rec selectors ~typeOK in_ : dotted_ident =
  match in_.token with
  | Ktype ii when typeOK ->
      nextToken in_;
      (* AST: SingletonTypeTree(t) *)
      ["type", ii]
  | _ ->
      let t1 = selector in_ in
      if in_.token =~= (DOT ab)
      then begin
        skipToken in_;
        t1::selectors ~typeOK in_
      end
      else [t1]

(* ------------------------------------------------------------------------- *)
(* Paths *)
(* ------------------------------------------------------------------------- *)

(** {{{
 *   QualId ::= Id {`.` Id}
 *   }}}
*)
let qualId in_ : dotted_ident =
  (* ast: Ident(id) *)
  let id = ident in_ in
  match in_.token with
  | DOT _ ->
      skipToken in_;
      id::selectors ~typeOK:false in_
  | _ -> [id]

(* Calls `qualId()` and manages some package state. *)
let pkgQualId in_ : dotted_ident =
  (* AST: if ... then inScalePackage = true *)
  let pkg = qualId in_ in
  newLineOptWhenFollowedBy (LBRACE ab) in_;
  (* AST: adjust currentPackage *)
  pkg


(** {{{
 *   MixinQualifier ::= `[` Id `]`
 *   }}}
*)
let mixinQualifierOpt in_ =
  if in_.token =~= (LBRACKET ab)
  then let xs = inBrackets identForType in_ in ()
  else () (* AST: tpnme.EMPTY *)


(** {{{
 *  Path       ::= StableId
 *              |  [Ident `.`] this
 *  AnnotType ::= Path [`.` type]
 *  }}}
*)
let path ~thisOK ~typeOK in_ : path =
  in_ |> with_logging (spf "path(thisOK:%b, typeOK:%b)" thisOK typeOK) (fun()->
    match in_.token with
    | Kthis ii ->
        nextToken in_;
        (* AST: t := This(tpnme.Empty) *)
        let t = "this", ii in
        if not thisOK || in_.token =~= (DOT ab) then begin
          accept (DOT ab) in_;
          t::selectors ~typeOK in_
        end
        else [t]

    | Ksuper ii ->
        nextToken in_;
        let x = mixinQualifierOpt in_ in
        let t = "super", ii in
        (* AST: t := Super(This(tpnme.EMPTY), x *)
        accept (DOT ab) in_;
        let x = selector in_ in
        if in_.token =~= (DOT ab)
        then begin
          skipToken in_;
          t::x::selectors ~typeOK in_
        end
        else [t;x]
    | _ ->
        let name = ident in_ in
        (* AST: t := Ident(name) and special stuff in BACKQUOTED_IDENT *)
        if in_.token =~= (DOT ab) then begin
          skipToken in_;
          match in_.token with
          | Kthis ii ->
              nextToken in_;
              (* ast: t = This(name.toTypeName) *)
              let t = "this", ii in
              if not thisOK || in_.token =~= (DOT ab) then begin
                accept (DOT ab) in_;
                name::t::selectors ~typeOK in_
              end
              else [name;t]
          | Ksuper ii ->
              (* pad: factorize with above Ksuper case *)
              nextToken in_;
              let x = mixinQualifierOpt in_ in
              (* AST: Super(This(name.toTypeName), x) *)
              let t = "super", ii in
              accept (DOT ab) in_;
              let x = selector in_ in
              if in_.token =~= (DOT ab)
              then begin
                skipToken in_;
                t::x::selectors ~typeOK in_
              end
              else [name;t;x]
          | _ ->
              name::selectors ~typeOK in_
        end
        else [name]
  )

(** {{{
 *  StableId ::= Id
 *            |  Path `.` Id
 *            |  [id `.`] super [`[` id `]`]`.` id
 *  }}}
*)
let stableId in_ : stable_id =
  path ~thisOK:false ~typeOK:false in_

(*****************************************************************************)
(* Forward references  *)
(*****************************************************************************)
(* The Scala constructs are mutually recursive, like in most languages.
 * We should use a long list of let rec xxx ... and yyy  ... but
 * but it helps a bit to structure the parser to reduce those set of
 * mutual dependencies by using forward references.
 * alt: have all functions mutually recursive
*)
let interpolatedString_ =
  ref (fun ~inPattern _ -> failwith "forward ref not set")
let exprTypeArgs_ =
  ref (fun _ -> failwith "forward ref not set")

let annotTypeRest_ =
  ref (fun _ -> failwith "forward ref not set")

let template_ = ref (fun _ -> failwith "forward ref not set")
let defOrDcl_ = ref (fun _ _ -> failwith "forward ref not set")
let tmplDef_ = ref (fun _ -> failwith "forward ref not set")
let blockStatSeq_ = ref (fun _ -> failwith "forward ref not set")
let topLevelTmplDef_ = ref (fun _ -> failwith "forward ref not set")
let packageOrPackageObject_ = ref (fun _ -> failwith "forward ref not set")

(*****************************************************************************)
(* Literal  *)
(*****************************************************************************)

(** {{{
 *  SimpleExpr    ::= literal
 *                  | symbol
 *                  | null
 *  }}}
*)
let literal ?(isNegated=false) ?(inPattern=false) in_
  : literal_or_interpolated  =
  in_ |> with_logging (spf "literal(isNegated:%b, inPattern:%b)"
                         isNegated inPattern) (fun () ->
    let finish value_ =
      (* ast: newLiteral(value) *)
      nextToken in_;
      value_
    in
    match in_.token with
    | T_INTERPOLATED_START (_s, ii) ->
        (* AST: if not inPattern then withPlaceholders(...) *)
        let x = !interpolatedString_ ~inPattern in_ in
        Right (ExprTodo ("interpolated", ii))
    (* scala3: unsupported, deprecated in 2.13.0 *)
    (* AST: Apply(scalaDot(Symbol, List(finish(in.strVal)) *)
    | SymbolLiteral(s, ii) -> finish (Right (ExprTodo ("Symbol", ii)))

    | CharacterLiteral(x, ii) ->
        (* ast: incharVal *)
        finish (Left (Char (x, ii)))
    | IntegerLiteral(x, ii) ->
        (* AST: in.intVal(isNegated) *)
        finish (Left (Int (x, ii)))
    | FloatingPointLiteral(x, ii) ->
        (* AST: in.floatVal(isNegated)*)
        finish (Left (Float (x, ii)))

    | StringLiteral(x, ii) ->
        (* ast: in.strVal.intern() *)
        finish (Left (String (x, ii)))

    | BooleanLiteral(x, ii) ->
        (* ast: bool *)
        finish (Left (Bool (x, ii)))

    | Knull ii ->
        (* ast: null *)
        finish (Left (Null ii))

    | _ -> error "illegal literal" in_
  )

(*****************************************************************************)
(* Infix Expr/Types/pattern management  *)
(*****************************************************************************)

let pushOpInfo top in_ : ident =
  let x = ident in_ in
  let targs =
    if in_.token =~= (LBRACKET ab)
    then !exprTypeArgs_ in_
    else fb [] (* ast: Nil *)
  in
  (* AST: OpInfo(top, name, targs) *)
  x

(*****************************************************************************)
(* Parsing types  *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* TODO: in PatternContextSensitive *)
(* ------------------------------------------------------------------------- *)

(** {{{
 *  Type ::= InfixType `=>` Type
 *         | `(` [`=>` Type] `)` `=>` Type
 *         | InfixType [ExistentialClause]
 *  ExistentialClause ::= forSome `{` ExistentialDcl {semi ExistentialDcl} `}`
 *  ExistentialDcl    ::= type TypeDcl | val ValDcl
 *  }}}
*)
let rec typ in_ : type_ =
  in_ |> with_logging "typ" (fun () ->
    (* CHECK: placeholderTypeBoundary *)
    let t =
      match in_.token with
      | LPAREN _ -> tupleInfixType in_
      | _ -> infixType FirstOp in_
    in
    match in_.token with
    | ARROW ii ->
        skipToken in_;
        let t2 = typ in_ in
        (* AST: makeFunctionTypeTree t t2 *)
        TyFunction1 (t, ii, t2)

    | KforSome ii ->
        skipToken in_;
        makeExistentialTypeTree t in_
    | _ -> t
  )

(** {{{
 *  InfixType ::= CompoundType {id [nl] CompoundType}
 *  }}}
*)
(* pad: similar to infixExpr, but seems very rarely used in Scala projects.
*)
and infixType mode in_ : type_ =
  in_ |> with_logging (spf "infixType(%s)" (show_mode mode)) (fun () ->
    (* CHECK: placeholderTypeBoundary *)
    let x = compoundType in_ in
    infixTypeRest x mode in_
  )

and infixTypeRest t mode in_ : type_ =
  in_ |> with_logging "infixTypeRest" (fun () ->
    (* Detect postfix star for repeated args.
     * Only RPAREN can follow, but accept COMMA and EQUALS for error's sake.
     * Take RBRACE as a paren typo.
    *)
    let checkRepeatedParam in_ =
      if TH.isRawStar in_.token
      then
        lookingAhead (fun in_ ->
          match in_.token with
          | RPAREN ii (* the good one *)
          | COMMA ii | EQUALS ii (* error recovery *)
          | RBRACE ii (* probably typo *)
            -> Some ii
          | _ -> None
        ) in_
      else None
    in
    let asInfix in_ =
      (* AST: let leftAssoc = nme.isLeftAssoc(in.name) *)
      let leftAssoc = false in
      warning ("InfixTypeRest: leftAssoc??");
      (* AST: if (mode != InfixMode.FirstOp) checkAssoc ... *)
      let tycon = identForType in_ in
      newLineOptWhenFollowing TH.isTypeIntroToken in_;
      (* AST: let mkOp t1 = AppliedTypeTree(tycon, List(t, t1)) *)
      if leftAssoc
      then
        let x = compoundType in_ in
        (* AST: mkOp(x) *)
        infixTypeRest x LeftOp in_
      else
        let x = infixType RightOp in_ in
        (* AST: mkOp(x) *)
        TyTodo("InfixType Right", snd tycon)
    in
    if TH.isIdentBool in_.token
    then
      (match checkRepeatedParam in_ with
       | None ->
           asInfix in_
       | Some ii ->
           TyTodo ("RepeatedParam", ii)
      )
    else t
  )

(* () must be () => R; (types) could be tuple or (types) => R *)
and tupleInfixType in_ : type_ =
  in_ |> with_logging "tupleInfixType" (fun () ->
    if not (in_.token =~= (LPAREN ab))
    then error "first token must be a left parenthesis" in_;
    let ts =
      inParens (fun in_ ->
        match in_.token with
        | RPAREN _ -> []
        | _ -> functionTypes in_
      ) in_ in
    (match in_.token with
     | ARROW ii ->
         skipToken in_;
         let t = typ in_ in
         (* AST: makeSafeFunctionType(ts, t) *)
         TyTodo ("Arrow", ii)
     (* CHECK: if is.isEmpty "Illegal literal type (), use Unit instead" *)
     | _ ->
         (* CHECK: ts foreach checkNotByNameOrVarargs *)
         (* AST: makeSafeTupleType(ts) *)
         let tuple =  TyTuple ts in
         warning "tupleInfixType:TODO infixTypeRest(compoundTypeRest(...))";
         tuple
    )
  )


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
and simpleType in_ : type_ =
  in_ |> with_logging "simpleType" (fun () ->
    match in_.token with
    | t when TH.isLiteral t && not (t =~= (Knull ab)) ->
        let ii = TH.info_of_tok in_.token in
        let x = literal in_ in
        (* AST: SingletonTypeTree(x) *)
        (match x with
         | Left lit -> TyLiteral lit
         | Right interpolated -> TyTodo ("TyInterpolated", ii)
        )
    | MINUS ii when lookingAhead (fun in_ -> TH.isNumericLit in_.token) in_ ->
        nextToken in_;
        let x = literal ~isNegated:true in_ in
        (* AST: SingletonTypeTree(x) *)
        (match x with
         | Left lit -> TyLiteral lit
         | Right interpolated -> TyTodo ("TyInterpolated", ii)
        )
    | _ ->
        let start =
          match in_.token with
          | LPAREN ii ->
              (* CHECK: "Illegal literal type (), use Unit instead" *)
              let xs = inParens types in_ in
              (* AST: makeSafeTupleType *)
              TyTodo ("LPAREN", ii)
          | t when TH.isWildcardType t ->
              let ii = TH.info_of_tok in_.token in
              skipToken in_;
              let bounds = wildcardType in_ in
              (* TODO: bounds *)
              TyTodo ("_ and bounds", ii)
          | _ ->
              let x = path ~thisOK:false ~typeOK:true in_ in
              (* AST: convertToTypeId if not SingletonTypeTree *)
              TyName x
        in
        simpleTypeRest start in_
  )

and simpleTypeRest t in_ : type_ =
  match in_.token with
  | HASH _ ->
      let x = typeProjection t in_ in
      simpleTypeRest x in_
  | LBRACKET _ ->
      let xs = typeArgs in_ in
      (* AST: AppliedTypeTree(t, xs) *)
      let x = () in
      simpleTypeRest t in_
  | _ -> t

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
  commaSeparated argType in_

and functionTypes in_ =
  commaSeparated functionArgType in_

(** {{{
 *  CompoundType ::= AnnotType {with AnnotType} [Refinement]
 *                |  Refinement
 *  }}}
*)
and compoundType in_ : type_ =
  (* pad: can't call typ() here, which allows function type 'int => float'
   * because this is used in a pattern context as in case p: ... =>
   * where the arrow mark the start of the caseBlock, not a type.
  *)
  let t =
    match in_.token with
    | (LBRACE ii) ->
        TyTodo ("scalaAnyRefConstr", ii)  (* AST: scalaAnyRefConstr *)
    | _ -> annotType in_
  in
  compoundTypeRest t in_

and compoundTypeRest t in_ =
  let ts = ref [] in
  while in_.token =~= (Kwith ab) do
    nextToken in_;
    let x = annotType in_ in
    ts += x;
  done;
  newLineOptWhenFollowedBy (LBRACE ab) in_;
  let types = !ts in
  let hasRefinement = (in_.token =~= (LBRACE ab)) in
  let refinements =
    if hasRefinement
    then Some (refinement in_)
    else None
  in
  (* CHECK: "Detected apparent refinement of Unit" *)
  (* AST: CompoundTypeTree(Template(tps, noSelfType, refinements) *)
  let ii = TH.info_of_tok in_.token in
  TyTodo ("compoundTypeRest", ii)

(** {{{
 *  AnnotType        ::=  SimpleType {Annotation}
 *  }}}
*)
(* was in PatternContextSensitive trait *)
and annotType in_ =
  (* CHECK: placeholderTypeBoundary *)
  let x = simpleType in_ in
  !annotTypeRest_ x in_

(* ------------------------------------------------------------------------- *)
(* Still in PatternContextSensitive, but more obscure type constructs *)
(* ------------------------------------------------------------------------- *)
and makeExistentialTypeTree t in_ =
  todo "makeExistentialTypeTree" in_

(* pad: https://stackoverflow.com/questions/6676048/why-does-one-select-scala-type-members-with-a-hash-instead-of-a-dot *)
and typeProjection t in_ : type_ =
  skipToken in_;
  let name = identForType in_ in
  (* AST: SelectFromTypeTree(t, name) *)
  t

(** {{{
 *  Refinement ::= [nl] `{` RefineStat {semi RefineStat} `}`
 *  }}}
*)
and refinement in_ =
  inBraces refineStatSeq in_

(** {{{
 *  RefineStatSeq    ::= RefineStat {semi RefineStat}
 *  RefineStat       ::= Dcl
 *                     | type TypeDef
 *                     |
 *  }}}
*)
and refineStatSeq in_ =
  (* CHECK: checkNoEscapingPlaceholders *)
  (* less: pad: reuse statSeq? *)
  let stats = ref [] in
  while not (TH.isStatSeqEnd in_.token) do
    let xs = refineStat in_ in
    stats ++= xs;
    if not (in_.token =~= (RBRACE ab))
    then acceptStatSep in_;
  done;
  !stats

and refineStat in_ =
  match in_.token with
  | t when TH.isDclIntro t ->
      let xs = !defOrDcl_ noMods in_ in
      []
  | t when not (TH.isStatSep t) ->
      error "illegal start of declaration" in_
  | _ -> []

(* ------------------------------------------------------------------------- *)
(* Abstract in PatternContextSensitive *)
(* ------------------------------------------------------------------------- *)
(** {{{
 *  ArgType       ::=  Type
 *  }}}
*)
and argType in_ =
  warning "argType: TODO typ() and wildcard or typ()";
  typ in_

and functionArgType in_ =
  warning "functionArgType TODO argType or paramType";
  argType in_

(* ------------------------------------------------------------------------- *)
(* Outside PatternContextSensitive but mutually recursive *)
(* ------------------------------------------------------------------------- *)

(** {{{
 *  WildcardType ::= `_` TypeBounds
 *  }}}
*)
and wildcardType in_ =
  (* AST: freshTypeName("_$"), Ident(...) *)
  let bounds = typeBounds in_ in
  (* AST: makeSyntheticTypeParam(pname, bounds) *)
  (* CHECK: placeholderTypes = ... *)
  ()

(** {{{
 *  TypeBounds ::= [`>:` Type] [`<:` Type]
 *  }}}
*)
and typeBounds in_ =
  (* CHECK: checkNoEscapingPlaceholders *)
  let lo = bound (SUPERTYPE ab) in_ in
  let hi = bound (SUBTYPE ab) in_ in
  (* AST: TypeBoundsTree(lo, hi) *)
  ()

and bound tok in_ =
  if in_.token =~= tok then begin
    let ii = TH.info_of_tok tok in
    nextToken in_;
    Some (ii, typ in_)
  end
  (* ast: EmptyTree *)
  else None

(* ------------------------------------------------------------------------- *)
(* Outside PatternContextSensitive *)
(* ------------------------------------------------------------------------- *)

(* These are default entry points into the pattern context sensitive methods:
 *  they are all initiated from non-pattern context.
*)
let typ x : type_ =
  outPattern typ x
let startInfixType x : type_ =
  outPattern (infixType FirstOp) x
let startAnnotType in_ : type_ =
  outPattern annotType in_
let exprSimpleType x : type_ =
  outPattern simpleType x

let typeOrInfixType location in_ : type_ =
  in_ |> with_logging "typeOrInfixType" (fun () ->
    if location = Local
    then typ in_
    else startInfixType in_
  )

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
  | _ -> None (* ast: TypeTree *)

(*****************************************************************************)
(* Parsing patterns  *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Inside SeqContextSensitive *)
(* ------------------------------------------------------------------------- *)
(* TODO: functionArgType, argType different here *)

(** {{{
 *  Pattern  ::=  Pattern1 { `|` Pattern1 }
 *  SeqPattern ::= SeqPattern1 { `|` SeqPattern1 }
 *  }}}
*)
let rec pattern in_ : pattern =
  in_ |> with_logging "pattern" (fun () ->
    let rec loop () =
      let p1 = pattern1 in_ in
      (if TH.isRawBar in_.token
       then begin
         let ii = TH.info_of_tok in_.token in
         nextToken in_;
         let p2 = loop () in
         PatDisj (p1, ii, p2)
       end else p1
      )
    in
    let res = loop () in
    (* ast: makeAlternative if many elements *)
    res
  )

(** {{{
 *  Pattern1    ::= boundvarid `:` TypePat
 *                |  `_` `:` TypePat
 *                |  Pattern2
 *  SeqPattern1 ::= boundvarid `:` TypePat
 *                |  `_` `:` TypePat
 *                |  [SeqPattern2]
 *  }}}
*)
and pattern1 in_ : pattern =
  let p = pattern2 in_ in
  (* crazy? depending whether Ident(name) && if nme.isVariableName(name)? *)
  (match in_.token with
   | COLON ii ->
       (* AST: p.removeAttachment[BackquotedIdentifierAttachment.type] *)
       skipToken in_;
       let x = compoundType in_ in
       (* AST: Typed(p, x) *)
       PatTodo ("PatTypedVarid", ii)
   (* CHECK: "Pattern variables must start with a lower-case letter." *)
   | _ -> p
  )

(** {{{
 *  Pattern2    ::=  id  `@` Pattern3
 *                |  `_` `@` Pattern3
 *                |   Pattern3
 *  }}}
*)
and pattern2 in_ : pattern =
  let x = pattern3 in_ in
  match in_.token with
  | AT ii -> (* crazy? TODO: case p @ Ident(name) *)
      nextToken in_;
      let body = pattern3 in_ in
      (* AST: Bind(name, body), if WILDCARD then ... *)
      PatTodo ("PatBind", ii)
  | _ -> x

(** {{{
 *  Pattern3    ::= SimplePattern
 *                |  SimplePattern {Id [nl] SimplePattern}
 *  }}}
*)
(* pad: similar to infixExpr/infixType, rename infixPattern? *)
and pattern3 in_ : pattern =
  in_ |> with_logging "pattern3" (fun () ->
    (* CHECK: badPattern3 *)
    let top = simplePattern in_ in
    (* AST: let base = opstack *)
    let checkWildStar in_ =
      warning "checkWildStar, incomplete";
      (* crazy: if top = USCORE && sequenceOK && peekingAhead ... *)
      (match in_.token with
       | STAR ii ->
           nextToken in_;
           Some ii
       | _ -> None
      )
    in
    let rec loop top in_ =
      in_ |> with_logging "pattern3: loop" (fun () ->
        (* AST: let next = reducePatternStack(base, top) *)
        let next = () in
        if TH.isIdentBool in_.token && not (TH.isRawBar in_.token) then begin
          let id = pushOpInfo next in_ in
          (* no postfixPattern, so always go for right part of infix op *)
          let x = simplePattern in_ in
          (* TODO: combine top, infixop, x *)
          loop x in_
        end else
          in_ |> with_logging "pattern3: loop noIsIdent stop" (fun () ->
            top
          )
      )
    in
    (match checkWildStar in_ with
     | None ->
         let x = loop top in_ in
         (* AST: stripParens(x) *)
         x
     | Some ii ->
         (* AST: x *)
         PatTodo ("PatUnderscoreStar?", ii)
    )
  )

(** {{{
 *  Patterns ::= Pattern { `,` Pattern }
 *  SeqPatterns ::= SeqPattern { `,` SeqPattern }
 *  }}}
*)
and patterns in_ =
  commaSeparated pattern in_

(* ------------------------------------------------------------------------- *)
(* Outside SeqContextSensitive *)
(* ------------------------------------------------------------------------- *)

(** {{{
 *  SimplePattern    ::= varid
 *                    |  `_`
 *                    |  literal
 *                    |  XmlPattern
 *                    |  StableId  /[TypeArgs]/ [`(` [Patterns] `)`]
 *                    |  StableId  [`(` [Patterns] `)`]
 *                    |  StableId  [`(` [Patterns] `,` [varid `@`] `_` `*` `)`]
 *                    |  `(` [Patterns] `)`
 *  }}}
 *
 * XXX: Hook for IDE
*)
and simplePattern in_ : pattern =
  in_ |> with_logging "simplePattern" (fun () ->
    match in_.token with
    (* pad: the code was written in a very different way by doing that
     * below, given isIdentBool will say yes for MINUS
    *)
    | MINUS ii ->
        nextToken in_;
        let x = literal ~isNegated:true ~inPattern:true in_ in
        (match x with
         | Left lit -> PatLiteral lit
         | Right interpolated -> PatTodo ("NegativeLiteral", ii)
        )
    | x when TH.isIdentBool x || x =~= (Kthis ab) ->
        let ii = TH.info_of_tok x in
        let t = stableId in_ in
        (* less: if t = Ident("-") literal isNegated:true inPattern:true *)
        let typeAppliedTree =
          match in_.token with
          | LBRACKET ii ->
              let xs = typeArgs in_ in
              (* AST: AppliedTypeTree(convertToTypeId(t), xs) *)
              PatTodo ("AppliedTypeTree", ii)
          | _ -> PatTodo ("PatName", ii)
        in
        (match in_.token with
         | LPAREN ii ->
             let xs = argumentPatterns in_ in
             (* AST: Apply(typeAppliedTree, t) *)
             PatTodo ("PatCall", ii)
         | _ -> (* AST: typeAppliedTree *) typeAppliedTree
        )
    | USCORE ii ->
        nextToken in_;
        (* AST: Ident(nme.WILDCARD) *)
        PatVarid ("_", ii)
    | LPAREN ii ->
        let xs = makeParens (noSeq patterns) in_ in
        PatTodo ("PatCall", ii)
    | x when TH.isLiteral x ->
        let ii = TH.info_of_tok x in
        let x = literal ~inPattern:true in_ in
        (match x with
         | Left lit -> PatLiteral lit
         | Right interpolated -> PatTodo ("interpolated", ii)
        )
    (* scala3: deprecated XMLSTART *)
    | _ ->
        error "illegal start of simple pattern" in_
  )

and argumentPatterns in_ =
  in_ |> with_logging "argumentPatterns" (fun () ->
    let xs = inParens (fun in_ ->
      if in_.token =~= (RPAREN ab)
      then []
      else seqPatterns in_
    ) in_
    in
    () (* AST: xs *)
  )

(* ------------------------------------------------------------------------- *)
(* Context sensitive choices *)
(* ------------------------------------------------------------------------- *)

and seqPatterns in_ =
  seqOK patterns in_

let pattern in_ : pattern =
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
let rec expr ?(location=Local) (in_: env) : expr =
  expr0 location in_

and expr0 (location: location) (in_: env) : expr =
  in_ |> with_logging(spf "expr0(location = %s)"(show_location location))(fun() ->
    match in_.token with
    | Kif _ -> S (parseIf in_)
    | Ktry _ -> S (parseTry in_)
    | Kwhile _ -> S (parseWhile in_)
    | Kdo _ -> S (parseDo in_)
    | Kfor _ -> S (parseFor in_)
    | Kreturn _ -> S (parseReturn in_)
    | Kthrow _ -> S (parseThrow in_)
    | Kimplicit _ ->
        skipToken in_;
        implicitClosure location in_
    | _ ->
        parseOther location in_
  )

and parseOther location (in_: env) : expr =
  in_ |> with_logging (spf "parseOther(location = %s)"
                         (show_location location))(fun() ->
    let t = ref (postfixExpr in_) in
    (match in_.token with
     | EQUALS _ ->
         skipToken in_;
         (* crazy? parsing that depends on built AST!!
          * AST: if Ident | Select | Apply *)
         let e = expr in_ in
         () (* AST: mkAssign(t, e) *)
     (* pad: we may actually be inside a binding, and not an expression here,
      * for example in 'foo((x: Path) => (...))' we can have parsed
      * x and seeing the colon.
     *)
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
      warning "lhsIsTypedParamList";
      false
    in
    if in_.token =~= (ARROW ab) &&
       (location <> InTemplate || lhsIsTypedParamList !t) then begin
      skipToken in_;
      let x =
        if location <> InBlock
        then expr in_
        else S (Block (fb (block in_ )))
      in
      (* AST: Function(convertToParams(t), x) *)
      ()
    end;
    (* AST: stripParens(t) *)
    !t
  )

(** {{{
 *  PostfixExpr   ::= InfixExpr [Id [nl]]
 *  InfixExpr     ::= PrefixExpr
 *                  | InfixExpr Id [nl] InfixExpr
 *  }}}
*)
and postfixExpr in_ : expr =
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
          let id = pushOpInfo top in_ in
          newLineOptWhenFollowing (TH.isExprIntro) in_;
          if TH.isExprIntro in_.token then begin
            let res = prefixExpr in_ in
            match res with
            (*| None -> (* AST: reduceExprStack(base, top) *) None
              | Some *) next ->
                loop next in_
          end
          else
            in_ |> with_logging "postfixExpr:loop: noExprIntro, stop" (fun()->
              (* AST: finishPostfixOp(base, popOpInfo()) *)
              Postfix (top, id)
            )
        end
      )
    in
    let res = prefixExpr in_ in
    (* AST: reduceExprStack (res) *)
    loop res in_
  )

(** {{{
 *  PrefixExpr   ::= [`-` | `+` | `~` | `!`] SimpleExpr
 *  }}}
*)
and prefixExpr in_ : expr =
  match in_.token with
  | t when TH.isUnaryOp t ->
      if lookingAhead (fun in_ -> TH.isExprIntro in_.token) in_
      then begin
        let uname = rawIdent in_ in (* AST: toUnaryName ... *)
        match t, in_.token with
        | MINUS ii, x when TH.isNumericLit x  (* uname == nme.UNARY_- ... *)->
            (* start at the -, not the number *)
            let x = literal ~isNegated:true in_ in
            let x' =
              match x with
              | Left lit -> L lit
              | Right e -> ExprTodo ("ExprInterpolated", ii)
            in
            simpleExprRest ~canApply:true x' in_
        | _ ->
            let x = simpleExpr in_ in
            (* AST: Select(stripParens(x), uname) *)
            x
      end
      else simpleExpr in_
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
and simpleExpr in_ : expr =
  in_ |> with_logging "simpleExpr" (fun () ->
    let canApply = ref true in
    let t =
      match in_.token with
      | x when TH.isLiteral x ->
          let ii = TH.info_of_tok x in
          let x = literal in_ in
          (match x with
           | Left lit -> L lit
           | Right xx -> ExprTodo ("interpolated", ii)
          )
      (* scala3: deprecated XMLSTART *)
      | x when TH.isIdentBool x ->
          let x = path ~thisOK:true ~typeOK:false in_ in
          Name x
      | Kthis _ | Ksuper _ ->
          let x = path ~thisOK:true ~typeOK:false in_ in
          Name x
      | USCORE ii ->
          let x = freshPlaceholder in_ in
          ExprUnderscore ii
      (* pad: this may actually be a binding, not a tuple of
       * expressions when part of a short lambda (arrow).
      *)
      | LPAREN ii ->
          let x = makeParens (commaSeparated expr) in_ in
          ExprTodo ("LPAREN", ii)
      | LBRACE ii ->
          canApply := false;
          let x = blockExpr in_ in
          ExprTodo ("LBRACE", ii)
      | Knew ii ->
          canApply := false;
          skipToken in_;
          let (parents, self, stats) = !template_ in_ in
          (* AST: gen.mkNew(parents, self, stats) *)
          New (ii)
      | _ -> error "illegal start of simple expression" in_
    in
    simpleExprRest ~canApply:!canApply t in_
  )

and simpleExprRest ~canApply t in_ : expr =
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
        let x = selector (*t*) in_ in
        let x = stripParens t in
        simpleExprRest ~canApply:true x in_
    | LBRACKET _ ->
        let t1 = t in (* AST: stripParens(t) *)
        (* crazy: parsing depending on built AST: Ident(_) | Select(_, _) | Apply(_, _) | Literal(_) *)
        let app = ref t1 in
        while in_.token =~= (LBRACKET ab) do
          let xs = exprTypeArgs in_ in
          (* AST: app := TypeApply(app, xs) *)
          ()
        done;
        simpleExprRest ~canApply:true !app in_

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

(* and literal in_ = ...
 *  is now at the top because it's also used by SimplePattern
*)

and exprTypeArgs in_ =
  outPattern typeArgs in_

(* AST: *)
and freshPlaceholder in_ =
  nextToken in_;
  ()

(* ------------------------------------------------------------------------- *)
(* Interpolated strings *)
(* ------------------------------------------------------------------------- *)

and interpolatedString ~inPattern in_ =
  (* AST: let interpolater = in.name.encoded,  *)
  (* AST: let partsBuf = ref [] in let exprsBuf = ref [] in *)
  nextToken in_; (* T_INTERPOLATED_START(s,info) *)
  while TH.is_stringpart in_.token do
    (* pad: in original code, but the interpolated string can start with
     * a non string literal like $f, so I've commented this code.
     * let x = literal in_ in
     * if inPattern
     * then todo "interpolatedString: inPattern (dropAnyBraces(pattern))" in_
     * else
    *)
    match in_.token with
    (* pad: the original code  uses IDENTIFIER but Lexer_scala.mll
     * introduces a new token for $xxx.
     * TODO? the original code also allow 'this', but ID_DOLLAR should cover
     * that?
    *)
    | ID_DOLLAR (s, ii) ->
        let x = ident in_ in
        () (* AST: Ident(x) *)
    (* actually a ${, but using LBRACE allows to reuse blockExpr *)
    | LBRACE _ ->
        let x = expr in_ in
        (* AST: x *)
        ()
    (* pad: not in original code, but the way Lexer_scala.mll is written
     * we can have multiple consecutive StringLiteral *)
    | StringLiteral _ ->
        nextToken in_
    | _ ->
        error "error in interpolated string: identifier or block expected"
          in_
  done;
  (* pad: not in original code *)
  accept (T_INTERPOLATED_END ab) in_;
  (* AST: InterpolatedString(...) *)
  ()

(* ------------------------------------------------------------------------- *)
(* Arguments *)
(* ------------------------------------------------------------------------- *)
(* A succession of argument lists. *)
and multipleArgumentExprs in_ : arguments list =
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
and argumentExprs in_ : arguments =
  in_ |> with_logging "argumentExprs" (fun () ->
    let args in_ =
      (* AST: if isIdent then assignmentToMaybeNamedArg *)
      commaSeparated expr in_
    in
    match in_.token with
    | LBRACE _ -> ArgBlock (blockExpr in_)
    | LPAREN _ ->
        (* less: could use makeParens *)
        inParens (fun in_ ->
          match in_.token with
          | RPAREN _ -> []
          | _ -> args in_
        ) in_
        |> (fun x -> Args x)
    (* stricter: *)
    | _ -> error "was expecting a ( or { for an argumentExprs" in_
  )

(* ------------------------------------------------------------------------- *)
(* Case clauses *)
(* ------------------------------------------------------------------------- *)

(** {{{
 *  Guard ::= if PostfixExpr
 *  }}}
*)
and guard in_ : guard option =
  in_ |> with_logging "guard" (fun () ->
    match in_.token with
    | Kif ii ->
        nextToken in_;
        let x = postfixExpr in_ in
        (* AST: stripParens(x) *)
        Some (ii, x)
    | _ -> None (* ast: Nil *)
  )

and caseBlock in_ : tok * block =
  let ii = TH.info_of_tok in_.token in
  accept (ARROW ab) in_;
  let x = block in_ in
  ii, x

and caseClause icase in_ : case_clause =
  let p = pattern in_ in
  let g = guard in_ in
  let (iarrow, block) = caseBlock in_ in
  (* ast: makeCaseDef *)
  icase, p, g, iarrow, block

(** {{{
 *  CaseClauses ::= CaseClause {CaseClause}
 *  CaseClause  ::= case Pattern [Guard] `=>` Block
 *  }}}
*)
and caseClauses in_ : case_clauses =
  (* CHECK: empty cases *)
  let xs = caseSeparated caseClause in_ in
  xs

(* ------------------------------------------------------------------------- *)
(* Misc *)
(* ------------------------------------------------------------------------- *)
(** {{{
 *  Expr ::= implicit Id `=>` Expr
 *  }}}
*)
and implicitClosure location in_ =
  let expr0 =
    let id = ident in_ in
    if in_.token =~= (COLON ab)
    then begin
      nextToken in_;
      let t = typeOrInfixType location in_ in
      (* AST: Typed(Ident(id, t)) *)
      ()
    end else () (* AST: Ident(id) *)
  in
  (* AST: convertToParam expr0; copyValDef(param0, mods|Flags.IMPLICIT)  *)
  let param = expr0 in
  accept (ARROW ab) in_;
  let x =
    if location <> InBlock
    then expr in_
    else S (Block (fb (block in_ )))
  in
  (* AST: Function(List(param), x) *)
  x

(*****************************************************************************)
(* Parsing statements (which are expressions in Scala) *)
(*****************************************************************************)
and parseIf in_ : stmt =
  let ii = TH.info_of_tok in_.token in
  skipToken in_;
  let cond = condExpr in_ in
  newLinesOpt in_;
  let thenp = expr in_ in
  let elsep =
    match in_.token with
    | Kelse ii ->
        nextToken in_;
        let e = expr in_ in
        Some (ii, e)
    | _ ->
        (* ast: literalUnit *)
        None
  in
  (* ast: If(cond, thenp, elsep) *)
  If (ii, cond, thenp, elsep)

and condExpr in_ : expr bracket =
  let lp = TH.info_of_tok in_.token in
  accept (LPAREN ab) in_;
  let r = expr in_ in
  let rp = TH.info_of_tok in_.token in
  accept (RPAREN ab) in_;
  (* AST: if isWildcard(r) *)
  (lp, r, rp)

and parseWhile in_ : stmt =
  let ii = TH.info_of_tok in_.token in
  skipToken in_;
  let cond = condExpr in_ in
  newLinesOpt in_;
  let body = expr in_ in
  (* ast: makeWhile(cond, body) *)
  While (ii, cond, body)

and parseDo in_ : stmt =
  let ido = TH.info_of_tok in_.token in
  skipToken in_;
  let lname = () (* AST: freshTermName(nme.DO_WHILE_PREFIX) *) in
  let body = expr in_ in
  if TH.isStatSep in_.token then nextToken in_;
  let iwhile = TH.info_of_tok in_.token in
  accept (Kwhile ab) in_;
  let cond = condExpr in_ in
  (* ast: makeDoWhile(lname.toTermName, body, cond) *)
  DoWhile (ido, body, iwhile, cond)

and parseFor in_ : stmt =
  let ii = TH.info_of_tok in_.token in
  skipToken in_;
  let enums =
    if in_.token =~= (LBRACE ab)
    then inBraces enumerators in_
    else inParens enumerators in_
  in
  let enums = fb [] in (* TODO *)
  newLinesOpt in_;
  let body =
    match in_.token with
    | Kyield ii ->
        nextToken in_;
        let e = expr in_ in
        (* ast: gen.mkFor(enums, gen.Yield(expr())) *)
        Yield (ii, e)
    | _ ->
        let e = expr in_ in
        (* ast: gen.mkFor(enums, expr()) *)
        NoYield e
  in
  For (ii, enums, body)

and parseReturn in_ : stmt =
  let ii = TH.info_of_tok in_.token in
  skipToken in_;
  let x =
    if TH.isExprIntro in_.token
    then Some (expr in_)
    else None (* ast: literalUnit *)
  in
  (* ast: Return(x) *)
  Return (ii, x)


and parseTry in_ : stmt =
  let itry = TH.info_of_tok in_.token in
  skipToken in_;
  let body = expr in_ in
  let handler =
    match in_.token with
    | Kcatch ii ->
        nextToken in_;
        let e = expr in_ in
        (* AST: makeMatchFromExpr(e) *)
        Some (ii, e)
    | _ -> None (* ast: Nil *)
  in
  let finalizer =
    match in_.token with
    | Kfinally ii ->
        nextToken in_;
        Some (ii, expr in_)
    | _ ->
        (* ast: EmptyTree *)
        None
  in
  (* ast: Try(body, handler, finalizer) *)
  Try (itry, body, handler, finalizer)


and parseThrow in_ : stmt =
  let ii = TH.info_of_tok in_.token in
  skipToken in_;
  let e = expr in_ in
  (* ast: Throw(e) *)
  Throw (ii, e)

and statement (location: location) (in_: env) : expr =
  expr ~location in_

(** {{{
 *  Block ::= BlockStatSeq
 *  }}}
 *  @note  Return tree does not carry position.
*)

and block in_ : block =
  in_ |> with_logging "block" (fun () ->
    let xs = !blockStatSeq_ in_ in
    (* ast: makeBlock(xs) *)
    xs
  )

(** {{{
  *  BlockExpr ::= `{` (CaseClauses | Block) `}`
  *  }}}
*)
and blockExpr in_ : block_expr =
  inBraces (fun in_ ->
    match in_.token with
    | Kcase _ ->
        let xs = caseClauses in_ in
        (* AST: Match(EmptyTree, xs *)
        BECases xs
    | _ ->
        let xs  = block in_ in
        BEBlock xs
  ) in_

(* ------------------------------------------------------------------------- *)
(* Enumerator/generator *)
(* ------------------------------------------------------------------------- *)
(** {{{
 *  Enumerators ::= Generator {semi Enumerator}
 *  Enumerator  ::=  Generator
 *                |  Guard
 *                |  Pattern1 `=` Expr
 *  }}}
*)
and enumerators in_ =
  in_ |> with_logging "enumerators" (fun () ->
    let enums = ref [] in
    let xs = enumerator ~isFirst:true in_ in
    enums ++= xs;
    while TH.isStatSep in_.token do
      nextToken in_;
      let xs = enumerator ~isFirst:false in_ in
      enums ++= xs;
    done;
    !enums
  )

(* pad: this was duplicated in enumerator and generator in the original code*)
and guard_loop in_ : guard list =
  if not (in_.token =~= (Kif ab))
  then [] (* ast: Nil *)
  else
    let g = guard in_ in
    let xs = guard_loop in_ in
    (* ast: makeFilter (g)::xs *)
    Common.opt_to_list g @ xs

and enumerator ~isFirst ?(allowNestedIf=true) in_ =
  in_ |> with_logging "enumerator" (fun () ->
    match in_.token with
    | Kif _ when not isFirst -> let xs = guard_loop in_ in () (* TODO *)
    | _ ->
        let g = generator ~eqOK:(not isFirst) ~allowNestedIf in_ in
        ()
  )

(** {{{
 *  Generator ::= Pattern1 (`<-` | `=`) Expr [Guard]
 *  }}}
*)
and generator ~eqOK ~allowNestedIf in_ : generator =
  in_ |> with_logging "generator" (fun () ->
    let hasVal = in_.token =~= (Kval ab) in
    if hasVal then nextToken in_;
    let pat = noSeq pattern1 in_ in
    let hasEq = in_.token =~= (EQUALS ab) in
    (* CHECK: scala3: "`val` keyword in for comprehension is" *)
    let ieq = TH.info_of_tok in_.token in
    (if hasEq && eqOK
     then nextToken in_
     else accept (LARROW ab) in_
    );
    let rhs = expr in_ in
    let tail =
      if allowNestedIf
      then guard_loop in_
      else [] (* ast: Nil *)
    in
    (* ast: gen.mkGenerator(genPos, pat, hasEq, rhs) :: tail *)
    (pat, ieq, rhs, tail)
  )

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
  let args =
    if in_.token =~= (LPAREN ab)
    then multipleArgumentExprs in_ (* ast: New (t, xs) *)
    else [] (* ast: New(t, Nil) *)
  in
  t, args

and annotations ~skipNewLines in_ : annotation list =
  in_ |> with_logging (spf "annotations(skipNewLines:%b)" skipNewLines)(fun()->
    readAnnots (fun iat in_ ->
      let (t, args) = annotationExpr in_ in
      if skipNewLines then newLineOpt in_;
      iat, t, args
    ) in_
  )

let annotTypeRest t in_ : type_ =
  let xs = annotations ~skipNewLines:false in
  (* AST: fold around t makeAnnotated *)
  t

(*****************************************************************************)
(* Parsing directives  *)
(*****************************************************************************)

let wildImportSelector in_ =
  (* AST: val selector = ImportSelector.wildAt(in.offset) *)
  nextToken in_

(** {{{
 *  ImportSelector ::= Id [`=>` Id | `=>` `_`]
 *  }}}
*)
let importSelector in_ : import_selector =
  let name = wildcardOrIdent in_ in
  let rename =
    match in_.token with
    | ARROW ii ->
        nextToken in_;
        (* CHECK: "Wildcard import cannot be renamed" *)
        let alias = wildcardOrIdent in_ in
        Some (ii, alias)
    (* AST: if name = nme.WILDCARD && !bbq => null *)
    | _ -> None
  in
  (* ast: ImportSelector(name, start, rename, renameOffset) *)
  name, rename

(** {{{
 *  ImportSelectors ::= `{` {ImportSelector `,`} (ImportSelector | `_`) `}`
 *  }}}
*)
let importSelectors in_ : import_selector list bracket =
  (* CHECK: "Wildcard import must be in last position" *)
  inBracesOrNil (commaSeparated importSelector) in_

(** {{{
 *  ImportExpr ::= StableId `.` (Id | `_` | ImportSelectors)
 *  }}}
*)
let importExpr in_ : import_expr =
  in_ |> with_logging "importExpr" (fun () ->
    let thisDotted (*AST:name*) in_ : dotted_ident =
      let ii = TH.info_of_tok in_.token in
      nextToken in_;
      (* AST: val t = This(name) *)
      accept (DOT ab) in_;
      let result = selector (*t*) in_ in
      accept (DOT ab) in_;
      [("this", ii); result]
    in
    (** Walks down import `foo.bar.baz.{ ... }` until it ends at
     * an underscore, a left brace, or an undotted identifier.
    *)
    let rec loop (expr: dotted_ident) in_ =
      (* AST: let selectors = *)
      match in_.token with
      (* import foo.bar._; *)
      | USCORE ii -> let _ = wildImportSelector in_ in
          expr, ImportWildcard ii
      (* import foo.bar.{ x, y, z } *)
      | LBRACE _ ->
          let xs = importSelectors in_ in
          expr, ImportSelectors xs
      | _ ->
          let name = ident in_ in
          (match in_.token with
           | DOT _ ->
               (* import foo.bar.ident.<unknown> and so create a select node and recurse. *)
               (* AST: (Select(expr, name)) *)
               let t = expr @ [name] in
               nextToken in_;
               loop t in_
           | _ ->
               (* import foo.bar.Baz; *)
               (* AST: List(makeImportSelector(name, nameOffset)) *)
               expr, ImportId name
          )
          (* reaching here means we're done walking. *)
          (* AST: Import(expr, selectors) *)
    in
    let start : dotted_ident =
      match in_.token with
      | Kthis _ -> thisDotted (*AST: empty*) in_
      | _ ->
          (* AST: Ident() *)
          let id = ident in_ in
          (match in_.token with
           | DOT _ -> accept (DOT ab) in_
           | x when not (TH.isStatSep x) -> accept (DOT ab) in_
           | _ -> error ". expected" in_
          );
          (match in_.token with
           | Kthis _ ->
               let x = thisDotted (* AST: id.name.toTypeName*) in_ in
               id::x
           | _ -> [id]
          )
    in
    loop start in_
  )

(** {{{
 *  Import  ::= import ImportExpr {`,` ImportExpr}
 *  }}}
*)
let importClause in_ : import =
  let ii = TH.info_of_tok in_.token in
  accept (Kimport ab) in_;
  let xs = commaSeparated importExpr in_ in
  ii, xs

(*****************************************************************************)
(* Parsing modifiers  *)
(*****************************************************************************)

(* coupling: TH.isLocalModifier *)
let modifier_of_isLocalModifier_opt = function
  | Kabstract ii -> Some (Abstract, ii)
  | Kfinal ii -> Some (Final, ii)
  | Ksealed ii -> Some (Sealed, ii)
  | Kimplicit ii -> Some (Implicit, ii)
  | Klazy ii -> Some (Lazy, ii)
  | _ -> None

(** {{{
 *  AccessQualifier ::= `[` (Id | this) `]`
 *  }}}
*)
let accessQualifierOpt in_ : ident_or_this bracket option =
  match in_.token with
  | LBRACKET lb ->
      nextToken in_;
      (* CHECK: "duplicate private/protected qualifier" *)
      let id =
        match in_.token with
        | Kthis ii ->
            nextToken in_;
            (* Flags.Local?? *)
            "this", ii
        | _ ->
            let x = identForType in_ in
            (* ast: Modifiers(mods.flags, x) *)
            x
      in
      let rb = TH.info_of_tok in_.token in
      accept (RBRACKET ab) in_;
      Some (lb, id, rb)
  | _ -> None

(** {{{
 *  AccessModifier ::= (private | protected) [AccessQualifier]
 *  }}}
*)
let accessModifierOpt in_ : modifier option =
  (* ast: normalizeModifiers *)
  match in_.token with
  | Kprivate ii ->
      nextToken in_;
      (* ast: flagToken(in_.token) *)
      let x = accessQualifierOpt in_ in
      Some (Private x, ii)
  | Kprotected ii ->
      nextToken in_;
      (* ast: flagToken(in_.token) *)
      let x = accessQualifierOpt in_ in
      Some (Protected x, ii)
  | _ -> None (* ast: noMods *)

(** {{{
 *  LocalModifiers ::= {LocalModifier}
 *  LocalModifier  ::= abstract | final | sealed | implicit | lazy
 *  }}}
*)
let localModifiers in_ : modifier list =
  let rec loop mods in_ =
    (* old: if TH.isLocalModifier in_.token
     * then let mods = addMod mods in_.token in_ in loop mods in_
     * else mods
    *)
    match modifier_of_isLocalModifier_opt in_.token with
    | Some x -> nextToken in_; loop (x::mods) in_
    | None -> List.rev mods
  in
  loop noMods in_

(* ast: normalizeModifiers() *)
(* CHECK: "repeated modifier" *)
(* old: let addMod mods t in_ = nextToken in_; t::mods *)

(** {{{
 *  Modifiers ::= {Modifier}
 *  Modifier  ::= LocalModifier
 *              |  AccessModifier
 *              |  override
 *  }}}
*)
let modifiers in_ =
  (* ast: normalizeModifiers() *)
  let rec loop (mods: modifier list) =
    match in_.token with
    | Kprivate _ | Kprotected _ ->
        let mopt = accessModifierOpt in_ in
        loop (Common.opt_to_list mopt @ mods)
    (* old: let mods = addMod mods in_.token in_ in
     * let mods_bis = accessQualifierOpt in_ in
     * loop mods
    *)
    | Koverride ii ->
        loop ((Override, ii)::mods)
    | Kabstract _ | Kfinal _ | Ksealed _ | Kimplicit _ | Klazy _ ->
        (* old: let mods = addMod mods in_.token in_ in loop mods *)
        loop (List.rev (localModifiers in_) @ mods)
    | NEWLINE _ ->
        nextToken in_;
        loop mods
    | _ -> List.rev mods
  in
  loop noMods

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
let paramType ?(repeatedParameterOK=true) in_ : param_type =
  in_ |> with_logging "paramType" (fun () ->
    match in_.token with
    | ARROW ii ->
        nextToken in_;
        let t = typ in_ in
        (* ast: byNameApplication *)
        PTByNameApplication (ii, t)
    | _ ->
        let t = typ in_ in
        if (TH.isRawStar in_.token)
        then begin
          let ii = TH.info_of_tok in_.token in
          nextToken in_;
          (* CHECK: if (!repeatedParameterOK)
           * "repeated parameters are only allowed in method signatures" *)
          (* ast: repeatedApplication t *)
          PTRepeatedApplication (t, ii)
        end
        else PT t (* ast: t *)
  )

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
    (* crazy?: if owner.isTypeName *)
    begin
      let xs = modifiers in_ in
      (* AST: mods = xs |= Flags.PARAMACCESSOR *)
      (* CHECK: "lazy modifier not allowed here. " *)
      (match in_.token with
       | Kval _ |  Kvar _ ->
           nextToken in_;
           (* AST: if (v == VAR) mods |= Flags.MUTABLE *)
       | _ ->
           (* CHECK: if (mods.flags != Flags.PARAMACCESSOR) accept(VAL) *)
           (* AST: if (!caseParam) mods |= Flags.PrivateLocal *)
           ()
      );
      (* AST: if (caseParam) mods |= Flags.CASEACCESSOR *)
    end;

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
          let x = expr in_ in
          ()
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
let rec typeParamClauseOpt owner contextBoundBuf in_ =
  in_ |> with_logging "typeParamClauseOpt" (fun () ->

    let typeParam ms in_ =
      in_ |> with_logging "typeParam" (fun () ->
        let mods = ref ms in (* AST: ms | Flags.PARAM *)
        (match in_.token with
         (* TODO? is supposed to be only if isTypeName owner *)
         | PLUS _ ->
             nextToken in_;
             (* AST: mods |= Flags.COVARIANT *)
         | MINUS _ ->
             nextToken in_;
             (* AST: mods |= Flags.CONTRAVARIANT *)
         | _ -> ()
        );
        let pname = wildcardOrIdent in_ in (* AST: toTypeName *)
        let param =
          let tparams = typeParamClauseOpt pname None in_ in
          let xs = typeBounds in_ in
          (* AST: TypeDef(mods, pname, tparams, xs) *)
          ()
        in
        if contextBoundBuf <> None
        then begin
          while in_.token =~= (VIEWBOUND ab) do
            (* CHECK: scala3: "view bounds are unsupported" *)
            skipToken in_;
            let t = typ in_ in
            (* AST: contextBoundBuf +=
             *  makeFunctionTypeTree(List(Ident(pname)), t)
            *)
            ()
          done;
          while in_.token =~= (COLON ab) do
            skipToken in_;
            let t = typ in_ in
            (* AST: contextBoundBuf += AppliedTypeTree(t, List(Ident(pname)))*)
            ()
          done
        end;
        param
      )
    in
    newLineOptWhenFollowedBy (LBRACKET ab) in_;
    match in_.token with
    | LBRACKET _ ->
        let x = inBrackets (fun in_ ->
          let annots = annotations ~skipNewLines:true in_ in
          let mods = noMods (* AST: withannotation annots *) in
          commaSeparated (typeParam mods) in_
        ) in_
        in
        Some x
    | _ -> None
  )

(* ------------------------------------------------------------------------- *)
(* Constructor body (as in 'def this(...) = { <body> }') *)
(* ------------------------------------------------------------------------- *)
(** {{{
 *  SelfInvocation  ::= this ArgumentExprs {ArgumentExprs}
 *  }}}
*)
let selfInvocation vparamss in_ =
  accept (Kthis ab) in_;
  newLineOptWhenFollowedBy (LBRACE ab) in_;
  let xs = argumentExprs in_ in
  let t = ref () in
  (* AST: t = Apply(Ident(nme.CONSTRUCTOR), xs) *)
  newLineOptWhenFollowedBy (LBRACE ab) in_;
  while (match in_.token with LPAREN _ | LBRACE _ -> true | _ -> false) do
    let xs = argumentExprs in_ in
    (* AST: t = Apply(t, xs) *)
    newLineOptWhenFollowedBy (LBRACE ab) in_;
  done;
  (* AST: if classContextBounds is empty then t else
   *  Apply(t, vparamss.last.map(vp => Ident(vp.name)))
  *)
  ()

(** {{{
 *  ConstrBlock    ::=  `{` SelfInvocation {semi BlockStat} `}`
 *  }}}
*)
let constrBlock vparamss in_ =
  skipToken in_;
  let x = selfInvocation vparamss in_ in
  let xs =
    if TH.isStatSep in_.token then begin
      nextToken in_;
      let xs = !blockStatSeq_ in_ in
      [] (* TODO: xs *)
    end
    else [](* AST: Nil *)
  in
  let stats = x::xs in
  accept (RBRACE ab) in_;
  (* AST: Block(stats, literalUnit) *)
  ()

(** {{{
 *  ConstrExpr      ::=  SelfInvocation
 *                    |  ConstrBlock
 *  }}}
*)
let constrExpr vparamss in_ =
  if in_.token =~= (LBRACE ab)
  then constrBlock vparamss in_
  else
    let x = selfInvocation vparamss in_ in
    (* AST: Block(x :: Nil, literalUnit) *)
    ()

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
  in_ |> with_logging "funDefRest" (fun () ->
    let newmods = ref mods in
    (* contextBoundBuf is for context bounded type parameters of the form
     * [T : B] or [T : => B]; it contains the equivalent implicit parameter type,
     * i.e. (B[T] or T => B)
    *)
    let contextBoundBuf = ref [] in
    let tparams = typeParamClauseOpt name (Some contextBoundBuf) in_ in
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
          let x = blockExpr in_ in
          ()
      | EQUALS _ ->
          nextTokenAllow TH.nme_MACROkw in_;
          if (TH.isMacro in_.token) then begin
            nextToken in_;
            (* AST: newmmods |= MACRO *)
          end;
          let x = expr in_ in
          ()
      | _ -> accept (EQUALS ab) in_ (* generate error message *)
    in
    (* AST: DefDef(newmods, name.toTermName, tparams, vparamss, restype, rhs) *)
    (* CHECK: "unary prefix operator definition with empty parameter list ..."*)
    ()
  )

let funDefOrDcl mods in_ =
  in_ |> with_logging "funDefOrDcl" (fun () ->
    nextToken in_;
    match in_.token with
    | Kthis _ ->
        skipToken in_;
        let classcontextBoundBuf = ref [] in (* AST: TODO? *)
        let name = () (* AST: nme.CONSTRUCTOR *) in
        (* pad: quite similar to funDefRest *)
        let vparamss =
          paramClauses ~ofCaseClass:false name classcontextBoundBuf in_ in
        newLineOptWhenFollowedBy (LBRACE ab) in_;
        let rhs =
          match in_.token with
          | LBRACE _ ->
              (* CHECK: "procedure syntax is deprecated for constructors" *)
              constrBlock vparamss in_
          | _ ->
              accept (EQUALS ab) in_;
              constrExpr vparamss in_
        in
        (* AST: DefDef(mods, nme.CONSTRUCTOR, [], vparamss, TypeTree(), rhs)*)
        ()

    | _ ->
        let name = identOrMacro in_ in
        funDefRest mods name in_
  )

(*****************************************************************************)
(* Parsing types definitions or declarations  *)
(*****************************************************************************)
(** {{{
 *  TypeDef ::= type Id [TypeParamClause] `=` Type
 *            | FunSig `=` Expr
 *  TypeDcl ::= type Id [TypeParamClause] TypeBounds
 *  }}}
*)
let typeDefOrDcl mods in_ =
  nextToken in_;
  newLinesOpt in_;
  let name = identForType in_ in
  (* a type alias as well as an abstract type may declare type parameters *)
  let tparams = typeParamClauseOpt name (None) in_ in
  match in_.token with
  | EQUALS _ ->
      nextToken in_;
      let t = typ in_ in
      (* AST: TypeDef(mods, name, tparams, t) *)
      ()
  | SEMI _ | NEWLINE _ | NEWLINES _
  | SUPERTYPE _ | SUBTYPE _
  | RBRACE _ | EOF _ ->
      let xs = typeBounds in_ in
      (* AST: TypeDef(mods | Flags.DEFERRED, name, tparams, typeBounds()) *)
      ()

  | _ -> error "`=`, `>:`, or `<:` expected" in_

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
    let mods = [] in
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
               then let x = implicitClosure InBlock in_ in ()
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
      let x = !packageOrPackageObject_ in_ in
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
  let (_, xs, _) = inBraces (templateStatSeq ~isPre) in_ in
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
(* pad: I've added isCase, it was passed via mods in the original code *)
let objectDef ?(isCase=false) ?(isPackageObject=false) mods in_ =
  in_ |> with_logging "objectDef" (fun () ->
    nextToken in_; (* 'object' *)
    let name = ident in_ in
    let tmpl = templateOpt mods (* AST: if isPackageObject ... *) in_ in
    (* AST: ModuleDef (mods, name.toTermName, template) *)
    tmpl
  )

(* ------------------------------------------------------------------------- *)
(* Package object *)
(* ------------------------------------------------------------------------- *)

(** Create a tree representing a package object, converting
 *  {{{
 *    package object foo { ... }
 *  }}}
 *  to
 *  {{{
 *    package foo {
 *      object `package` { ... }
 *    }
 *  }}}
*)
(* scala3: deprecated *)
let packageObjectDef in_ =
  let defn = objectDef noMods ~isPackageObject:true in_ in
  (* AST: gen.mkPackageObject(defn, pidPos, pkgPos) *)
  ()

let packageOrPackageObject in_ =
  if in_.token =~= (Kobject ab) then
    let x = packageObjectDef in_ in
    (* AST: joinComment(x::Nil).head *)
    ()
  else
    let x = pkgQualId in_ in
    let body = inBracesOrNil topStatSeq in_ in
    (* AST: makePackaging(x, body) *)
    ()

(* ------------------------------------------------------------------------- *)
(* Class/trait *)
(* ------------------------------------------------------------------------- *)
let constructorAnnotations in_ : annotation list =
  in_ |> with_logging "constructorAnnotations" (fun () ->
    readAnnots (fun iat in_ ->
      let t = exprSimpleType in_ in
      let (es: arguments) = argumentExprs in_ in
      (* ast: New(t, List(es)) *)
      iat, t, [es]
    ) in_
  )

(** {{{
 *  ClassDef ::= Id [TypeParamClause] ConstrAnnotations
 *               [AccessModifier] ClassParamClauses RequiresTypeOpt ClassTemplateOpt
 *  TraitDef ::= Id [TypeParamClause] RequiresTypeOpt TraitTemplateOpt
 *  }}}
*)

(* pad: I added isTrait and isCase instead of abusing mods *)
let classDef ?(isTrait=false) ?(isCase=false) mods in_ =
  in_ |> with_logging "classDef" (fun () ->
    nextToken in_;
    let name = identForType in_ in
    (* AST: savingClassContextBounds *)
    let contextBoundBuf = ref [] in
    let tparams = typeParamClauseOpt name (Some contextBoundBuf) in_ in
    let classContextBounds = !contextBoundBuf in
    (* CHECK: "traits cannot have type parameters with context bounds" *)
    let constrAnnots =
      if not isTrait then constructorAnnotations in_ else [] in
    let constrMods, vparamss =
      if isTrait
      then [], [] (* AST: (Modifiers(Flags.TRAIT), List()) *)
      else begin
        let constrMods =
          accessModifierOpt in_ in
        let vparamss =
          paramClauses ~ofCaseClass:isCase name classContextBounds in_ in
        Common.opt_to_list constrMods, vparamss
      end
    in
    let tmpl =
      templateOpt mods (* AST: name ... constrMods withAnnotations...*) in_ in
    (* AST: gen.mkClassDef(mods, name, tparams, template) *)
    (* CHECK: Context bounds generate implicit parameters (part of the template)
     *  with types from tparams: we need to ensure these don't overlap
     * ensureNonOverlapping(template, tparams)
    *)
    ()
  )

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
  | Ktrait _ -> classDef ~isTrait:true mods (* AST: | TRAIT | ABSTRACT *) in_
  | Kclass _ -> classDef mods in_
  | Kobject _ -> objectDef mods in_
  (* pad: was done via a lexing trick in postProcessToken in the original
   * code; not sure you needed that
  *)
  | Kcase _ ->
      nextToken in_;
      (match in_.token with
       | Kclass _ -> classDef ~isCase:true mods in_
       | Kobject _ -> objectDef ~isCase:true mods in_
       (* pad: my error message *)
       | _ -> error "expecting class or object after a case" in_
      )
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
  defOrDcl_ := defOrDcl;
  tmplDef_ := tmplDef;
  blockStatSeq_ := blockStatSeq;
  topLevelTmplDef_ := topLevelTmplDef;
  packageOrPackageObject_ := packageOrPackageObject;

  exprTypeArgs_ := exprTypeArgs;
  interpolatedString_ := interpolatedString;

  annotTypeRest_ := annotTypeRest;
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
