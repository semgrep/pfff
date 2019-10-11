%{
(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
 * Copyright (C) 2011-2015 Tomohiro Matsuyama
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

(* This file contains a grammar for Python (2 and 3).
 *
 * original src: 
 *  https://github.com/m2ym/ocaml-pythonlib/blob/master/src/python2_parser.mly
 * reference:
 *  - https://docs.python.org/3/reference/grammar.html
 *  - http://docs.python.org/release/2.5.2/ref/grammar.txt
 * old src: 
 *  - http://inst.eecs.berkeley.edu/~cs164/sp10/python-grammar.html
 *)
open Ast_python

type single_or_tuple =
  | Single of expr
  | Tup of expr list

let cons e = function
  | Single e' -> Tup (e::[e'])
  | Tup l -> Tup (e::l)

let tuple_expr = function
  | Single e -> e
  | Tup l -> Tuple (l, Load)

let to_list = function
  | Single e -> [e]
  | Tup l -> l


let rec set_expr_ctx ctx = function
  | Attribute (value, attr, _) ->
      Attribute (value, attr, ctx)
  | Subscript (value, slice, _) ->
      Subscript (value, slice, ctx)
  | Name (id, _, x, y) ->
      Name (id, ctx, x, y)
  | List (elts, _) ->
      List (List.map (set_expr_ctx ctx) elts, ctx)
  | Tuple (elts, _) ->
      Tuple (List.map (set_expr_ctx ctx) elts, ctx)
  | e -> e

let expr_store = set_expr_ctx Store
and expr_del = set_expr_ctx Del

let tuple_expr_store l =
  let e = tuple_expr l in
    match Ast_python.context_of_expr e with
    | Some Param -> e
    | _ -> expr_store e

%}

/*(*************************************************************************)*/
/*(*1 Tokens *)*/
/*(*************************************************************************)*/
%token <Ast_python.tok> TUnknown  /*(* unrecognized token *)*/
%token <Ast_python.tok> EOF

/*(*-----------------------------------------*)*/
/*(*2 The space/comment tokens *)*/
/*(*-----------------------------------------*)*/
/*(* coupling: Token_helpers.is_comment *)*/
%token <Ast_python.tok> TCommentSpace TComment
/*(* see the extra token below NEWLINE instead of TCommentNewline *)*/

/*(*-----------------------------------------*)*/
/*(*2 The normal tokens *)*/
/*(*-----------------------------------------*)*/

/*(* tokens with "values" *)*/
%token <string * Ast_python.tok> NAME
%token <int    * Ast_python.tok> INT LONGINT
%token <float  * Ast_python.tok> FLOAT
%token <string * Ast_python.tok> IMAG
%token <string * Ast_python.tok> STR

/*(*-----------------------------------------*)*/
/*(*2 Keyword tokens *)*/
/*(*-----------------------------------------*)*/
%token <Ast_python.tok> 
 IF ELSE ELIF 
 WHILE FOR
 RETURN CONTINUE BREAK PASS
 DEF LAMBDA CLASS GLOBAL
 TRY FINALLY EXCEPT RAISE
 AND NOT OR
 IMPORT FROM AS
 DEL IN IS WITH YIELD
 PRINT EXEC ASSERT

/*(*-----------------------------------------*)*/
/*(*2 Punctuation tokens *)*/
/*(*-----------------------------------------*)*/
 
/*(* syntax *)*/
%token <Ast_python.tok> 
 LPAREN         /* ( */ RPAREN         /* ) */
 LBRACK         /* [ */ RBRACK         /* ] */
 LBRACE         /* { */ RBRACE         /* } */
 COLON          /* : */
 SEMICOL        /* ; */
 DOT            /* . */
 COMMA          /* , */
 BACKQUOTE      /* ` */
 AT             /* @ */

/*(* operators *)*/
%token <Ast_python.tok> 
  ADD            /* + */  SUB            /* - */
  MULT           /* * */  DIV            /* / */
  MOD            /* % */
  POW            /* ** */  FDIV           /* // */
  BITOR          /* | */  BITAND         /* & */  BITXOR         /* ^ */
  BITNOT         /* ~ */  LSHIFT         /* << */  RSHIFT         /* >> */

%token <Ast_python.tok> 
  EQ             /* = */
  ADDEQ          /* += */ SUBEQ          /* -= */
  MULTEQ         /* *= */ DIVEQ          /* /= */
  MODEQ          /* %= */
  POWEQ          /* **= */ FDIVEQ         /* //= */
  ANDEQ          /* &= */ OREQ           /* |= */ XOREQ          /* ^= */
  LSHEQ          /* <<= */ RSHEQ          /* >>= */

  EQUAL          /* == */ NOTEQ          /* !=, <> */
  LT             /* < */ GT             /* > */
  LEQ            /* <= */ GEQ            /* >= */

/*(*-----------------------------------------*)*/
/*(*2 Extra tokens: *)*/
/*(*-----------------------------------------*)*/

/* layout */
%token INDENT DEDENT 
%token <Ast_python.tok>NEWLINE

/*(*************************************************************************)*/
/*(*1 Rules type declaration *)*/
/*(*************************************************************************)*/

%start main
%type <Ast_python.program> main
%%

/*(*************************************************************************)*/
/*(*1 Toplevel *)*/
/*(*************************************************************************)*/

main: file_input EOF { $1 }

file_input: nl_or_stmt_list { Module $1 }

nl_or_stmt:
 | NEWLINE { [] }
 | stmt    { $1 }

/*(*************************************************************************)*/
/*(*1 Import *)*/
/*(*************************************************************************)*/
/*(* In Python, imports can actually appear not just at the toplevel *)*/
import_stmt:
  | import_name { $1 }
  | import_from { $1 }


import_name: IMPORT dotted_as_name_list { Import ($2) }

dotted_as_name:
  | dotted_name         { $1, None }
  | dotted_name AS NAME { $1, Some $3 }

dotted_name:
  | NAME { [$1] }
  | NAME DOT dotted_name { $1::$3 }


import_from:
  | FROM name_and_level IMPORT MULT
      { ImportFrom (fst $2, [("*", $1(*TODO*)), None], snd $2) }
  | FROM name_and_level IMPORT LPAREN import_as_name_list RPAREN
      { ImportFrom (fst $2, $5, snd $2) }
  | FROM name_and_level IMPORT import_as_name_list
      { ImportFrom (fst $2, $4, snd $2) }

name_and_level:
  |           dotted_name { $1, Some 0 }
  | dot_level dotted_name { $2, Some $1 }
  | DOT dot_level         { [("",$1(*TODO*))], Some (1 + $2) }

dot_level:
  | /*(*empty *)*/ { 0 }
  | DOT dot_level  { 1 + $2 }

import_as_name:
  | NAME         { $1, None }
  | NAME AS NAME { $1, Some $3 }

/*(*************************************************************************)*/
/*(*1 Variable definition *)*/
/*(*************************************************************************)*/

expr_stmt:
  | testlist
    { ExprStmt (tuple_expr $1) }
 /*(* name -> expr_stmt_lhs *)*/
  | NAME COLON test
    { ExprStmt (Name ($1, Store, Some $3, ref NotResolved))} 
  | NAME COLON test EQ test
    { Assign ([Name ($1, Store, Some $3, ref NotResolved)], $5)} 
  | expr_stmt_lhs augassign expr_stmt_rhs 
    { AugAssign ($1, fst $2, $3) }
  | expr_stmt_lhs EQ        expr_stmt_rhs_list 
    { Assign ($1::(fst $3), snd $3) }
      
expr_stmt_lhs:
  | testlist { tuple_expr_store $1 }

expr_stmt_rhs:
  | yield_expr { $1 }
  | testlist { tuple_expr $1 }

expr_stmt_rhs_list:
  | expr_stmt_rhs { [], $1 }
  | expr_stmt_lhs EQ expr_stmt_rhs_list { $1::(fst $3), snd $3 }

augassign:
  | ADDEQ   { Add, $1 }
  | SUBEQ   { Sub, $1 }
  | MULTEQ  { Mult, $1 }
  | DIVEQ   { Div, $1 }
  | POWEQ   { Pow, $1 }
  | MODEQ   { Mod, $1 }
  | LSHEQ   { LShift, $1 }
  | RSHEQ   { RShift, $1 }
  | OREQ    { BitOr, $1 }
  | XOREQ   { BitXor, $1 }
  | ANDEQ   { BitAnd, $1 }
  | FDIVEQ  { FloorDiv, $1 }

/*(*************************************************************************)*/
/*(*1 Function definition *)*/
/*(*************************************************************************)*/

funcdef: decorator_list DEF NAME parameters return_type_opt COLON suite
    { FunctionDef ($3, $4, $5, $7, $1) }

return_type_opt: 
  | /*(* empty *)*/ { None }
  | SUB GT test     { Some $3 }

/*(*----------------------------*)*/
/*(*2 parameters *)*/
/*(*----------------------------*)*/

parameters: LPAREN varargslist RPAREN { $2 }

varargslist:
  | /*(*empty*)*/
     { [], None, None, [] }

  | fpdef 
     { [$1], None, None, [] }
  | fpdef COMMA varargslist
      { let (args, varargs, kwargs, defaults) = $3 in
        $1::args, varargs, kwargs, defaults }

  | fpdef EQ test
      { (* TODO check default arguments come after
           variable arguments with semantic analysis. *)
        [$1], None, None, [$3] }
  | fpdef EQ test COMMA varargslist
      { let (args, varargs, kwargs, defaults) = $5 in
        $1::args, varargs, kwargs, $3::defaults }

  | fpvarargs
      { [], fst $1, snd $1, [] }

fpdef:
  | NAME { Name ($1, Param, None, ref Parameter) }
  | LPAREN fpdef_list RPAREN { tuple_expr_store $2 }
  | NAME COLON test { Name ($1, Param, Some $3, ref Parameter) }


fpvarargs:
  | MULT NAME { Some $2, None }
  | MULT NAME COMMA fpkwargs { Some $2, $4 }
  | fpkwargs { None, $1 }

fpkwargs:
  | POW NAME { Some $2 }
  | POW NAME COLON test { Some $2 }

/*(*************************************************************************)*/
/*(*1 Class definition *)*/
/*(*************************************************************************)*/

classdef: decorator_list CLASS NAME testlist_paren_opt COLON suite 
   { ClassDef ($3, $4, $6, $1) }

testlist_paren_opt: 
 | /*(* empty *)*/ { [] }
 | LPAREN RPAREN   { [] }
 | LPAREN testlist RPAREN { to_list $2 }

/*(*************************************************************************)*/
/*(*1 Annotations *)*/
/*(*************************************************************************)*/

decorator:
  | AT decorator_name arglist_paren_opt NEWLINE { $3 $2 }

decorator_name:
  | atom_name { $1 }
  | atom_name DOT NAME { Attribute ($1, $3, Load) }

arglist_paren_opt:
  | /*(* empty*)*/        { fun x -> x }
  | LPAREN RPAREN         { fun x -> Call (x, [], [], None, None) }
  | LPAREN arglist RPAREN { fun x -> 
        let (args, keywords, starargs, kwargs) = $2 in
        Call (x, args, keywords, starargs, kwargs) }

/*(*************************************************************************)*/
/*(*1 Statement *)*/
/*(*************************************************************************)*/

stmt:
  | simple_stmt { $1 }
  | compound_stmt { [$1] }

suite:
  | simple_stmt { $1 }
  | NEWLINE INDENT stmt_list DEDENT { $3 }

simple_stmt:
  | small_stmt NEWLINE { [$1] }
  | small_stmt SEMICOL NEWLINE { [$1] }
  | small_stmt SEMICOL simple_stmt { $1::$3 }


small_stmt:
  | expr_stmt   { $1 }
  | print_stmt  { $1 }
  | del_stmt    { $1 }
  | pass_stmt   { $1 }
  | flow_stmt   { $1 }
  | import_stmt { $1 }
  | global_stmt { $1 }
  | assert_stmt { $1 }

print_stmt:
  | PRINT                     { Print (None, [], true) }
  | PRINT test print_testlist { Print (None, $2::(fst $3), snd $3) }
  | PRINT RSHIFT test { Print (Some $3, [], true) }
  | PRINT RSHIFT test COMMA test print_testlist { Print (Some $3, $5::(fst $6), snd $6) }

print_testlist:
  | /*(* empty *)*/  { [], true }
  | COMMA test COMMA { [$2], false }
  | COMMA test print_testlist { $2::(fst $3), snd $3 }


del_stmt: DEL exprlist { Delete (List.map expr_del (to_list $2)) }

pass_stmt: PASS { Pass }


flow_stmt:
  | break_stmt    { $1 }
  | continue_stmt { $1 }
  | return_stmt   { $1 }
  | raise_stmt    { $1 }
  | yield_stmt    { $1 }

break_stmt:    BREAK    { Break  }
continue_stmt: CONTINUE { Continue }

return_stmt:
  | RETURN          { Return (None) }
  | RETURN testlist { Return (Some (tuple_expr $2)) }

yield_stmt: yield_expr { ExprStmt ($1) }

raise_stmt:
  | RAISE                            { Raise (None, None, None) }
  | RAISE test                       { Raise (Some $2, None, None) }
  | RAISE test COMMA test            { Raise (Some $2, Some $4, None) }
  | RAISE test COMMA test COMMA test { Raise (Some $2, Some $4, Some $6) }


global_stmt: GLOBAL name_list { Global ($2) }

assert_stmt:
  | ASSERT test            { Assert ($2, None) }
  | ASSERT test COMMA test { Assert ($2, Some $4) }



compound_stmt:
  | if_stmt     { $1 }
  | while_stmt  { $1 }
  | for_stmt    { $1 }
  | try_stmt    { $1 }
  | with_stmt   { $1 }
  | funcdef     { $1 }
  | classdef    { $1 }


if_stmt: IF test COLON suite elif_stmt_list { If ($2, $4, $5) }

elif_stmt_list:
  | /*(*empty *)*/  { [] }
  | ELIF test COLON suite elif_stmt_list { [If ($2, $4, $5)] }
  | ELSE COLON suite { $3 }


while_stmt:
  | WHILE test COLON suite { While ($2, $4, []) }
  | WHILE test COLON suite ELSE COLON suite { While ($2, $4, $7) }


for_stmt:
  | FOR exprlist IN testlist COLON suite
      { For (tuple_expr_store $2, tuple_expr $4, $6, []) }
  | FOR exprlist IN testlist COLON suite ELSE COLON suite
      { For (tuple_expr_store $2, tuple_expr $4, $6, $9) }


try_stmt:
  | TRY COLON suite excepthandler_list
      { TryExcept ($3, $4, []) }
  | TRY COLON suite excepthandler_list ELSE COLON suite
      { TryExcept ($3, $4, $7) }
  | TRY COLON suite excepthandler_list ELSE COLON suite FINALLY COLON suite
      { TryFinally ([TryExcept ($3, $4, $7)], $10) }
  | TRY COLON suite excepthandler_list FINALLY COLON suite
      { TryFinally ([TryExcept ($3, $4, [])], $7) }
  | TRY COLON suite FINALLY COLON suite
      { TryFinally ($3, $6) }

excepthandler:
  | EXCEPT COLON suite { ExceptHandler (None, None, $3) }
  | EXCEPT test COLON suite { ExceptHandler (Some $2, None, $4) }
  | EXCEPT test AS test COLON suite { ExceptHandler (Some $2, Some $4, $6) }
  | EXCEPT test COMMA test COLON suite { ExceptHandler (Some $2, Some (expr_store $4), $6) }

with_stmt:
  | WITH test         COLON suite { With ($2, None, $4) }
  | WITH test AS expr COLON suite { With ($2, Some $4, $6) }

/*(*----------------------------*)*/
/*(*2 auxillary statements *)*/
/*(*----------------------------*)*/

/*(*************************************************************************)*/
/*(*1 Expressions *)*/
/*(*************************************************************************)*/

exprlist:
  | expr                { Single $1 }
  | expr COMMA          { Tup [$1] }
  | expr COMMA exprlist { cons $1 $3 }

expr:
  | xor_expr            { $1 }
  | expr BITOR xor_expr { BinOp ($1, BitOr, $3) }


xor_expr:
  | and_expr                 { $1 }
  | xor_expr BITXOR and_expr { BinOp ($1, BitXor, $3) }

and_expr:
  | shift_expr                 { $1 }
  | shift_expr BITAND and_expr { BinOp ($1, BitAnd, $3) }


shift_expr:
  | arith_expr                   { $1 }
  | shift_expr LSHIFT arith_expr { BinOp ($1, LShift, $3) }
  | shift_expr RSHIFT arith_expr { BinOp ($1, RShift, $3) }

arith_expr:
  | term                { $1 }
  | arith_expr ADD term { BinOp ($1, Add, $3) }
  | arith_expr SUB term { BinOp ($1, Sub, $3) }


term:
  | factor              { $1 }
  | factor term_op term { BinOp ($1, fst $2, $3) }

term_op:
  | MULT    { Mult, $1 }
  | DIV     { Div, $1 }
  | MOD     { Mod, $1 }
  | FDIV    { FloorDiv, $1 }

factor:
  | ADD factor { UnaryOp (UAdd, $2) }
  | SUB factor
      { (* CPython converts
             UnaryOp (op=Sub (), operand=Num (n=x))
           to
             Num (n=-x)
           if possible. *)
        match $2 with
        | Num (Int (n, x))        -> Num (Int (-n, x))
        | Num (LongInt (n, x))    -> Num (LongInt (-n, x))
        | Num (Float (n, x))      -> Num (Float (-.n, x))
        | Num (Imag (n, x))       -> Num (Imag ("-" ^ n, x))
        | _                       -> UnaryOp (USub, $2) }
  | BITNOT factor { UnaryOp (Invert, $2) }
  | power { $1 }

power:
  | atom_trailer            { $1 }
  | atom_trailer POW factor { BinOp ($1, Pow, $3) }

/*(*----------------------------*)*/
/*(*2 Atom trailer *)*/
/*(*----------------------------*)*/

atom_trailer:
  | atom { $1 }

  | atom_trailer LPAREN         RPAREN { Call ($1, [], [], None, None) }
  | atom_trailer LPAREN arglist RPAREN
      { let args, keywords, starargs, kwargs = $3 in
        Call ($1, args, keywords, starargs, kwargs) }

  | atom_trailer LBRACK subscript_list RBRACK
      { match $3 with
          (* TODO test* => Index (Tuple (elts)) *)
        | [s] -> Subscript ($1, s, Load)
        | l -> Subscript ($1, ExtSlice (l), Load) }

  | atom_trailer DOT NAME { Attribute ($1, $3, Load) }

/*(*----------------------------*)*/
/*(*2 Atom *)*/
/*(*----------------------------*)*/

atom:
  | atom_tuple  { $1 }
  | atom_list   { $1 }
  | atom_dict   { $1 }
  | atom_repr   { $1 }
  | atom_name   { $1 }

  | INT         { Num (Int ($1)) }
  | LONGINT     { Num (LongInt ($1)) }
  | FLOAT       { Num (Float ($1)) }
  | IMAG        { Num (Imag ($1)) }

  | string_list { 
     let xs = $1 in
     let s = xs |> List.map fst |> String.concat "" in
     Str (s, List.map snd xs) 
     }

atom_repr: BACKQUOTE testlist1 BACKQUOTE { Repr (tuple_expr $2) }

atom_name: NAME { Name ($1, Load, None, ref NotResolved) }


string_list:
  | STR { [$1] }
  | STR string_list { $1::$2 }

/*(*----------------------------*)*/
/*(*2 containers *)*/
/*(*----------------------------*)*/

atom_tuple:
  | LPAREN              RPAREN { Tuple ([], Load) }
  | LPAREN testlist     RPAREN { tuple_expr $2 }
  | LPAREN yield_expr   RPAREN { $2 }
  | LPAREN test gen_for RPAREN { GeneratorExp ($2, $3) }

atom_list:
  | LBRACK               RBRACK { List ([], Load) }
  | LBRACK testlist      RBRACK { List (to_list $2, Load) }
  | LBRACK test list_for RBRACK { ListComp ($2, $3) }

atom_dict:
  | LBRACE           RBRACE { Dict ([], []) }
  | LBRACE dictmaker RBRACE { Dict (fst $2, snd $2) }

dictmaker:
  | test COLON test { [$1], [$3] }
  | test COLON test COMMA { [$1], [$3] }
  | test COLON test COMMA dictmaker { $1::(fst $5), $3::(snd $5) }

  | POW expr { [], [] }
  | POW expr COMMA { [], [] }


/*(*----------------------------*)*/
/*(*2 Array access *)*/
/*(*----------------------------*)*/

subscript:
  | DOT DOT DOT { Ellipsis }
  | test { Index ($1) }
  | test_opt COLON test_opt { Slice ($1, $3, None) }
  | test_opt COLON test_opt COLON test_opt { Slice ($1, $3, $5) }

/*(*----------------------------*)*/
/*(*2 test *)*/
/*(*----------------------------*)*/

testlist:
  | test                { Single $1 }
  | test COMMA          { Tup [$1] }
  | test COMMA testlist { cons $1 $3 }

test:
  | or_test                      { $1 }
  | or_test IF or_test ELSE test { IfExp ($3, $1, $5) }
  | lambdadef                    { $1 }


or_test:
  | and_test                  { $1 }
  | and_test OR and_test_list { BoolOp (Or, $1::$3) }

and_test:
  | not_test                   { $1 }
  | not_test AND not_test_list { BoolOp (And, $1::$3) }

and_test_list:
  | and_test                  { [$1] }
  | and_test OR and_test_list { $1::$3 }

not_test:
  | NOT not_test { UnaryOp (Not, $2) }
  | comparison   { $1 }

not_test_list:
  | not_test                   { [$1] }
  | not_test AND not_test_list { $1::$3 }

comparison:
  | expr                         { $1 }
  | expr comp_op comparison_list { Compare ($1, (fst $2)::(fst $3), snd $3) }

comparison_list:
  | expr                         { [], [$1] }
  | expr comp_op comparison_list { (fst $2)::(fst $3), $1::(snd $3) }

comp_op:
  | EQUAL   { Eq, $1 }
  | NOTEQ   { NotEq, $1 }
  | LT      { Lt, $1 }
  | LEQ     { LtE, $1 }
  | GT      { Gt, $1 }
  | GEQ     { GtE, $1 }
  | IS      { Is, $1 }
  | IS NOT  { IsNot, $1 }
  | IN      { In, $1 }
  | NOT IN  { NotIn, $1 }

/*(*----------------------------*)*/
/*(*2 Advanced features *)*/
/*(*----------------------------*)*/

yield_expr:
  | YIELD          { Yield (None) }
  | YIELD testlist { Yield (Some (tuple_expr $2)) }

lambdadef: LAMBDA varargslist COLON test { Lambda ($2, $4) }

/*(*----------------------------*)*/
/*(*2 Comprehensions *)*/
/*(*----------------------------*)*/

list_for:
  | list_for1 { [$1] }
  | list_for1 list_for { $1::$2 }

list_for1: FOR exprlist IN old_test_list list_if_list 
  { tuple_expr_store $2, tuple_expr $4, $5 }
list_if: IF old_test { $2 }


old_test:
  | or_test { $1 }
  | old_lambdadef { $1 }

old_lambdadef: LAMBDA varargslist COLON old_test { Lambda ($2, $4) }

/*(*----------------------------*)*/
/*(*2 Generators *)*/
/*(*----------------------------*)*/

gen_for:
  | gen_for1 { [$1] }
  | gen_for1 gen_for { $1::$2 }

gen_for1: FOR exprlist IN or_test gen_if_list { tuple_expr_store $2, $4, $5 }
gen_if: IF old_test { $2 }

/*(*----------------------------*)*/
/*(*2 Arguments *)*/
/*(*----------------------------*)*/

arglist:
  | argument                { [$1], [], None, None }
  | argument COMMA          { [$1], [], None, None }
  | argument COMMA arglist  { let args, keywords, starargs, kwargs = $3 in
                              $1::args, keywords, starargs, kwargs }

  | starargs { $1 }

argument:
  | test         { $1 }
  | test gen_for { GeneratorExp ($1, $2) }

starargs:
  | MULT test                 { [], [], Some $2, None }
  | MULT test COMMA keywords  { let args, keywords, _, kwargs = $4 in
                                 args, keywords, Some $2, kwargs }

  | keywords                  { $1 }

keywords:
  | keyword                 { [], [$1], None, None }
  | keyword COMMA           { [], [$1], None, None }
  | keyword COMMA keywords  { let args, keywords, starargs, kwargs = $3 in
                              args, $1::keywords, starargs, kwargs }

  | POW test                 { [], [], None, Some $2 }

keyword: test EQ test
      { match $1 with
        | Name (id, _, _, _) -> (id, $3)
        | _ -> raise Parsing.Parse_error 
      }

/*(*************************************************************************)*/
/*(*1 xxx_opt, xxx_list *)*/
/*(*************************************************************************)*/

/*(* basic lists, 0 element allowed *)*/
nl_or_stmt_list:
  | /*(*empty*)*/               { [] }
  | nl_or_stmt  nl_or_stmt_list { $1 @ $2 }

stmt_list:
  | /*(* empty *)*/ { [] }
  | stmt stmt_list  { $1 @ $2 }

list_if_list:
  | /*(*empty*)*/        { [] }
  | list_if list_if_list { $1::$2 }

gen_if_list:
  | /*(*empty*)*/      { [] }
  | gen_if gen_if_list { $1::$2 }

decorator_list:
  | /*(* empty *)*/          { [] }
  | decorator decorator_list { $1::$2 }

/*(* basic lists, at least one element *)*/
excepthandler_list:
  | excepthandler                    { [$1] }
  | excepthandler excepthandler_list { $1::$2 }


/*(* list with commans and trailing comma *)*/
import_as_name_list:
  | import_as_name                           { [$1] }
  | import_as_name COMMA                     { [$1] }
  | import_as_name COMMA import_as_name_list { $1::$3 }

fpdef_list:
  | fpdef                  { Single $1 }
  | fpdef COMMA            { Tup [$1] }
  | fpdef COMMA fpdef_list { cons $1 $3 }

/*(* was called testlife_safe originally *)*/
old_test_list:
  | old_test                     { Single $1 }
  | old_test COMMA               { Tup [$1] }
  | old_test COMMA old_test_list { cons $1 $3 }

subscript_list:
  | subscript                      { [$1] }
  | subscript COMMA                { [$1] }
  | subscript COMMA subscript_list { $1::$3 }


/*(* list with commas, but without trailing comma *)*/
dotted_as_name_list:
  | dotted_as_name                           { [$1] }
  | dotted_as_name COMMA dotted_as_name_list { $1::$3 }

name_list:
  | NAME                 { [$1] }
  | NAME COMMA name_list { $1::$3 }

testlist1:
  | test                 { Single $1 }
  | test COMMA testlist1 { cons $1 $3 }

/*(* opt *)*/
test_opt:
  | /*(*empty*)*/ { None }
  | test          { Some $1 }
