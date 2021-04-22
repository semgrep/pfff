(* Yoann Padioleau
 *
 * Copyright (C) 2021 R2C
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

open Parser_scala
module PI = Parse_info

(*****************************************************************************)
(* Token Helpers *)
(*****************************************************************************)

let is_eof = function
  | EOF _ -> true
  | _ -> false

let is_comment = function
  | Comment _ | Space _ -> true
  (* newline has a meaning in the parser, so should not skip *)
  (* old: | Nl _ -> true *)
  | _ -> false

let token_kind_of_tok t =
  match t with
  | LBRACE _ -> PI.LBrace
  | RBRACE _ -> PI.RBrace
  | LPAREN _ -> PI.LPar
  | RPAREN _ -> PI.RPar
  | LBRACKET _ -> PI.LBracket
  | RBRACKET _ -> PI.RBracket

  | Comment _ -> PI.Esthet PI.Comment
  | Space _ -> PI.Esthet PI.Space
  | Nl _ -> PI.Esthet PI.Newline

  | _ -> PI.Other

(*****************************************************************************)
(* Visitors *)
(*****************************************************************************)

let visitor_info_of_tok f = function
  | Unknown(ii) -> Unknown(f ii)
  | EOF(ii) -> EOF(f ii)

  | Space(ii) -> Space(f ii)
  | Nl(ii) -> Nl(f ii)
  | Comment(ii) -> Comment(f ii)

  (* old:
     | InterpolatedString(ii) -> InterpolatedString(f ii)
     | InterpStart(ii) -> InterpStart(f ii)
  *)
  | T_INTERPOLATED_START(s, ii) -> T_INTERPOLATED_START(s, f ii)
  | T_INTERPOLATED_END(ii) -> T_INTERPOLATED_END(f ii)
  | T_INTERPOLATED_STRING(s, ii) -> T_INTERPOLATED_STRING(s, f ii)
  | T_DOLLAR_LBRACE(ii) -> T_DOLLAR_LBRACE(f ii)

  | ID_DOLLAR(s, ii) -> ID_DOLLAR(s, f ii)
  | ID_LOWER(s, ii) -> ID_LOWER(s, f ii)
  | ID_UPPER(s, ii) -> ID_UPPER(s, f ii)
  | ID_BACKQUOTED(s, ii) -> ID_BACKQUOTED(s, f ii)
  | OP(s, ii) -> OP(s, f ii)

  | IntegerLiteral(x, ii) -> IntegerLiteral(x, f ii)
  | FloatingPointLiteral(x, ii) -> FloatingPointLiteral(x, f ii)
  | CharacterLiteral(x, ii) -> CharacterLiteral(x, f ii)
  | BooleanLiteral(x, ii) -> BooleanLiteral(x, f ii)
  | SymbolLiteral(s, ii) -> SymbolLiteral(s, f ii)
  | StringLiteral(x, ii) -> StringLiteral(x, f ii)

  | UNDERSCORE(ii) -> UNDERSCORE(f ii)
  | TILDE(ii) -> TILDE(f ii)
  | STAR(ii) -> STAR(f ii)
  | SHARP(ii) -> SHARP(f ii)
  | SEMI(ii) -> SEMI(f ii)

  | LPAREN(ii) -> LPAREN(f ii)
  | LBRACKET(ii) -> LBRACKET(f ii)
  | LBRACE(ii) -> LBRACE(f ii)
  | RPAREN(ii) -> RPAREN(f ii)
  | RBRACKET(ii) -> RBRACKET(f ii)
  | RBRACE(ii) -> RBRACE(f ii)
  | RDots(ii) -> RDots(f ii)
  | LDots(ii) -> LDots(f ii)

  | PLUS(ii) -> PLUS(f ii)
  | PIPE(ii) -> PIPE(f ii)
  | MORECOLON(ii) -> MORECOLON(f ii)
  | MINUS(ii) -> MINUS(f ii)
  | LESSPERCENT(ii) -> LESSPERCENT(f ii)
  | LESSMINUS(ii) -> LESSMINUS(f ii)
  | LESSCOLON(ii) -> LESSCOLON(f ii)
  | EQMORE(ii) -> EQMORE(f ii)
  | EQ(ii) -> EQ(f ii)
  | BANG(ii) -> BANG(f ii)
  | AT(ii) -> AT(f ii)
  | DOT(ii) -> DOT(f ii)
  | COMMA(ii) -> COMMA(f ii)
  | COLON(ii) -> COLON(f ii)

  | Kyield(ii) -> Kyield(f ii)
  | Kwith(ii) -> Kwith(f ii)
  | Kwhile(ii) -> Kwhile(f ii)
  | Kvar(ii) -> Kvar(f ii)
  | Kval(ii) -> Kval(f ii)
  | Ktype(ii) -> Ktype(f ii)
  | Ktry(ii) -> Ktry(f ii)
  | Ktrait(ii) -> Ktrait(f ii)
  | Kthrow(ii) -> Kthrow(f ii)
  | Kthis(ii) -> Kthis(f ii)
  | Ksuper(ii) -> Ksuper(f ii)
  | Ksealed(ii) -> Ksealed(f ii)
  | Kreturn(ii) -> Kreturn(f ii)
  | Kprotected(ii) -> Kprotected(f ii)
  | Kprivate(ii) -> Kprivate(f ii)
  | Kpackage(ii) -> Kpackage(f ii)
  | Koverride(ii) -> Koverride(f ii)
  | Kobject(ii) -> Kobject(f ii)
  | Knull(ii) -> Knull(f ii)
  | Knew(ii) -> Knew(f ii)
  | Kmatch(ii) -> Kmatch(f ii)
  | Klazy(ii) -> Klazy(f ii)
  | Kimport(ii) -> Kimport(f ii)
  | Kimplicit(ii) -> Kimplicit(f ii)
  | Kif(ii) -> Kif(f ii)
  | KforSome(ii) -> KforSome(f ii)
  | Kfor(ii) -> Kfor(f ii)
  | Kfinally(ii) -> Kfinally(f ii)
  | Kfinal(ii) -> Kfinal(f ii)
  | Kextends(ii) -> Kextends(f ii)
  | Kelse(ii) -> Kelse(f ii)
  | Kdo(ii) -> Kdo(f ii)
  | Kdef(ii) -> Kdef(f ii)
  | Kclass(ii) -> Kclass(f ii)
  | Kcatch(ii) -> Kcatch(f ii)
  | Kcase(ii) -> Kcase(f ii)
  | Kabstract(ii) -> Kabstract(f ii)


  | Ellipsis(ii) -> Ellipsis(f ii)



let info_of_tok tok =
  let res = ref None in
  visitor_info_of_tok (fun ii -> res := Some ii; ii) tok |> ignore;
  Common2.some !res
