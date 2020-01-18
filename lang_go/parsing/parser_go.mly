%{
(* // Copyright 2009 The Go Authors. All rights reserved.
 * // Use of this source code is governed by a BSD-style
 * // license that can be found in the LICENSE file.
 *
 * // Go language grammar.
 * // 
 * // The Go semicolon rules are:
 * // 
 * //  1. all statements and declarations are terminated by semicolons.
 * //  2. semicolons can be omitted before a closing ) or }.
 * //  3. semicolons are inserted by the lexer before a newline
 * //      following a specific list of tokens.
 * // 
 * // Rules #1 and #2 are accomplished by writing the lists as
 * // semicolon-separated lists with an optional trailing semicolon.
 * // Rule #3 is implemented in yylex.
 *
 * src: this is mostly an ocamlyacc port of the Go yacc grammar in
 *  src/cmd/compile/internal/gc/go.y in commit
 *  b5fe07710f4a31bfc100fbc2e344be11e4b4d3fc^ in the golang source code
 *  at https://github.com/golang/go
 *)
open Ast_go

%}

/*(*************************************************************************)*/
/*(*1 Tokens *)*/
/*(*************************************************************************)*/
%token <Ast_go.tok> TUnknown  /*(* unrecognized token *)*/
%token <Ast_go.tok> EOF

/*(*-----------------------------------------*)*/
/*(*2 The space/comment tokens *)*/
/*(*-----------------------------------------*)*/
/*(* coupling: Token_helpers.is_comment *)*/
%token <Ast_go.tok> TCommentSpace TComment TCommentNewline

/*(*-----------------------------------------*)*/
/*(*2 The normal tokens *)*/
/*(*-----------------------------------------*)*/

/*(* tokens with "values" (was LLITERAL before) *)*/
%token  <string * Ast_go.tok> LINT LFLOAT  LIMAG  LRUNE LSTR
%token  <string * Ast_go.tok> LASOP 
%token  <string * Ast_go.tok> LNAME 

/*(*-----------------------------------------*)*/
/*(*2 Keyword tokens *)*/
/*(*-----------------------------------------*)*/

%token  <Ast_go.tok> 
  LIF LELSE 
  LFOR
  LRETURN LBREAK LCONTINUE LFALL
  LSWITCH LCASE LDEFAULT
  LGOTO
  LFUNC LCONST LVAR LTYPE LSTRUCT LINTERFACE 
  LGO LCHAN LSELECT 
  LDEFER 
  LPACKAGE LIMPORT  
  LMAP 
  LRANGE   

/*(*-----------------------------------------*)*/
/*(*2 Punctuation tokens *)*/
/*(*-----------------------------------------*)*/
/*(* syntax *)*/
%token <Ast_go.tok>
  LPAREN RPAREN 
  LBRACE RBRACE
  LBRACKET RBRACKET
  LCOLON LEQ LDOT LCOMMA
  LCOLAS /* := */
  LDDD
  
/*(* operators *)*/
%token <Ast_go.tok> 
  LPLUS LMINUS LMULT LDIV LPERCENT
  LPIPE LAND LHAT 
  LANDAND LOROR
  LANDNOT 
  LINC LDEC 
  LEQEQ LNE  
  LGE LGT LLE LLT 
  LLSH LRSH
  LBANG LTILDE
  LCOMM 

/*(*-----------------------------------------*)*/
/*(*2 Extra tokens: *)*/
/*(*-----------------------------------------*)*/
%token <Ast_go.tok>
  LBODY /* LBRACE parsing hack */
  LSEMICOLON /* sometimes implicitely inserted, see Parsing_hacks_go.ml */ 

/*(*************************************************************************)*/
/*(*1 Priorities *)*/
/*(*************************************************************************)*/

%left       LCOMM /*(*outside the usual hierarchy; here for good error msg*)*/

%left       LOROR
%left       LANDAND
%left       LEQEQ LNE LLE LGE LLT LGT
%left       LPLUS LMINUS LPIPE LHAT
%left       LMULT LDIV LPERCENT LAND LLSH LRSH LANDNOT

/*(*
 // manual override of shift/reduce conflicts.
 // the general form is that we assign a precedence
 // to the token being shifted and then introduce
 // NotToken with lower precedence or PreferToToken with higher
 // and annotate the reducing rule accordingly.
 *)*/
%left       NotParen
%left       LPAREN

%left       RPAREN
%left       PreferToRightParen

/*(*************************************************************************)*/
/*(*1 Rules type declaration *)*/
/*(*************************************************************************)*/

%start file sgrep_spatch_pattern
%type <Ast_go.program> file
%type <Ast_go.any>     sgrep_spatch_pattern

%%

/*(*************************************************************************)*/
/*(*1 Toplevel *)*/
/*(*************************************************************************)*/

file: package imports xdcl_list EOF 
  { { package = $1; imports = List.rev $2; decls = List.rev $3 } }

package: LPACKAGE sym LSEMICOLON { $2 }


sgrep_spatch_pattern: EOF { }

/*(*************************************************************************)*/
/*(*1 Import *)*/
/*(*************************************************************************)*/

import:
|   LIMPORT import_stmt { [] }
|   LIMPORT LPAREN import_stmt_list osemi RPAREN { [] }
|   LIMPORT LPAREN RPAREN { [] }

import_stmt: import_here  { }

import_here:
|   LSTR        { (*// import with original name*) }
|   sym LSTR    { (*// import with given name*)  }
|   LDOT LSTR   { (*// import into my name space *) }

/*(*************************************************************************)*/
/*(*1 Declarations *)*/
/*(*************************************************************************)*/

xdcl:
|   common_dcl { $1 }
|   xfndcl     { $1 }

common_dcl:
|   LVAR vardcl  { [] }
|   LVAR LPAREN vardcl_list osemi RPAREN { [] }
|   LVAR LPAREN RPAREN { [] }

|   LCONST constdcl { [] }
|   LCONST LPAREN constdcl osemi RPAREN { [] }
|   LCONST LPAREN constdcl LSEMICOLON constdcl1_list osemi RPAREN { [] }
|   LCONST LPAREN RPAREN { [] }

|   LTYPE typedcl { [] }
|   LTYPE LPAREN typedcl_list osemi RPAREN { [] }
|   LTYPE LPAREN RPAREN { [] }


vardcl:
|   dcl_name_list ntype { }
|   dcl_name_list ntype LEQ expr_list { }
|   dcl_name_list       LEQ expr_list { }


constdcl:
|   dcl_name_list ntype LEQ expr_list { }
|   dcl_name_list       LEQ expr_list { }

constdcl1:
|   constdcl { }
|   dcl_name_list ntype { }
|   dcl_name_list   { }


typedcl: 
| typedclname ntype { }
/*(* alias decl, go 1.?? *)*/
| typedclname LEQ ntype { }

/*(*************************************************************************)*/
/*(*1 Statements *)*/
/*(*************************************************************************)*/

stmt:
| /*(*empty*)*/    { }
| compound_stmt { }
| common_dcl { }
| non_dcl_stmt { }

compound_stmt: LBRACE stmt_list RBRACE { }

non_dcl_stmt:
|   simple_stmt { }

|   if_stmt { }
|   for_stmt { }
|   switch_stmt { }
|   select_stmt { }

|   labelname LCOLON stmt { }
|   LGOTO new_name { }

|   LBREAK onew_name { }
|   LCONTINUE onew_name { }
|   LRETURN oexpr_list { }
|   LFALL { }

|   LGO pseudocall { }
|   LDEFER pseudocall { }


simple_stmt:
|   expr { }
|   expr LASOP expr { }
|   expr_list LEQ expr_list { }
|   expr_list LCOLAS expr_list { }
|   expr LINC { }
|   expr LDEC { }


/*(* IF cond body (ELSE IF cond body)* (ELSE block)? *) */
if_stmt: LIF  if_header loop_body elseif_list else_
    {
        (* if $3.Left == nil
            Yyerror("missing condition in if statement");
        *)
    }

if_header:
|   osimple_stmt { }
|   osimple_stmt LSEMICOLON osimple_stmt { }


elseif: LELSE LIF  if_header loop_body
    {
        (* if $4.Left == nil {
            Yyerror("missing condition in if statement");
        } *)
    }

else_:
| /*(*empty*)*/ { }
|   LELSE compound_stmt { }


for_stmt: LFOR for_body { }

for_body: for_header loop_body { }

for_header:
|   osimple_stmt LSEMICOLON osimple_stmt LSEMICOLON osimple_stmt { }
|   osimple_stmt { }
|   range_stmt { }

range_stmt:
|   expr_list LEQ LRANGE expr { }
|   expr_list LCOLAS LRANGE expr { }
|   LRANGE expr { }

loop_body: LBODY stmt_list RBRACE { }


switch_stmt: LSWITCH if_header LBODY caseblock_list RBRACE { }

select_stmt:  LSELECT LBODY caseblock_list RBRACE { }

case:
|   LCASE expr_or_type_list LCOLON { }
|   LCASE expr_or_type_list LEQ expr LCOLON { }
|   LCASE expr_or_type_list LCOLAS expr LCOLON { }
|   LDEFAULT LCOLON { }

caseblock: case stmt_list
    {
      (*
        // If the last token read by the lexer was consumed
        // as part of the case, clear it (parser has cleared yychar).
        // If the last token read by the lexer was the lookahead
        // leave it alone (parser has it cached in yychar).
        // This is so that the stmt_list action doesn't look at
        // the case tokens if the stmt_list is empty.
        yylast = yychar;
        $1.Xoffset = int64(block);
    
        // This is the only place in the language where a statement
        // list is not allowed to drop the final semicolon, because
        // it's the only place where a statement list is not followed 
        // by a closing brace.  Handle the error for pedantry.

        // Find the final token of the statement list.
        // yylast is lookahead; yyprev is last of stmt_list
        last := yyprev;

        if last > 0 && last != ';' && yychar != '}' {
            Yyerror("missing statement after label");
        }
        $$ = $1;
        $$.Nbody = $3;
        popdcl();
      *)
    }

/*(*************************************************************************)*/
/*(*1 Expressions *)*/
/*(*************************************************************************)*/

expr:
|   uexpr { }

|   expr LOROR expr { }
|   expr LANDAND expr { }
|   expr LEQEQ expr { }
|   expr LNE expr { }
|   expr LLT expr { }
|   expr LLE expr { }
|   expr LGE expr { }
|   expr LGT expr { }
|   expr LPLUS expr { }
|   expr LMINUS expr { }
|   expr LPIPE expr { }
|   expr LHAT expr { }
|   expr LMULT expr { }
|   expr LDIV expr { }
|   expr LPERCENT expr { }
|   expr LAND expr { }
|   expr LANDNOT expr { }
|   expr LLSH expr { }
|   expr LRSH expr { }

/*(* not an expression anymore, but left in so we can give a good error *)*/
|   expr LCOMM expr { }

uexpr:
|   pexpr { }

|   LMULT uexpr { }
|   LAND uexpr
    {
           (* // Special case for &T{...}: turn into ( *T){...}. *)
    }
|   LPLUS uexpr { }
|   LMINUS uexpr { }
|   LBANG uexpr { }
|   LTILDE uexpr
    {
        (* Yyerror("the bitwise complement operator is ^"); *)
    }
|   LHAT uexpr { }
|   LCOMM uexpr { }

pexpr:
|   pexpr_no_paren { }

|   LPAREN expr_or_type RPAREN { }


pexpr_no_paren:
|   basic_literal { }

|   name {  }

|   pexpr LDOT sym { }
|   pexpr LDOT LPAREN expr_or_type RPAREN { }
|   pexpr LDOT LPAREN LTYPE RPAREN { }
|   pexpr LBRACKET expr RBRACKET { }
|   pexpr LBRACKET oexpr LCOLON oexpr RBRACKET { }
|   pexpr LBRACKET oexpr LCOLON oexpr LCOLON oexpr RBRACKET
    {
        (*if $5 == nil {
            Yyerror("middle index required in 3-index slice");
        }
        if $7 == nil {
            Yyerror("final index required in 3-index slice");
        }
        *)
    }

|   pseudocall { }

|   convtype LPAREN expr ocomma RPAREN { }
|   comptype lbrace braced_keyval_list RBRACE { }

|   pexpr_no_paren LBRACE braced_keyval_list RBRACE { }
|   LPAREN expr_or_type RPAREN LBRACE braced_keyval_list RBRACE
    {
        (* Yyerror("cannot parenthesize type in composite literal"); *)
    }
|   fnliteral { }

basic_literal:
| LINT { }
| LFLOAT { }
| LIMAG { }
| LRUNE { }
| LSTR { }


/*
 * call-like statements that
 * can be preceded by 'defer' and 'go'
 */
pseudocall:
|   pexpr LPAREN RPAREN { }
|   pexpr LPAREN expr_or_type_list ocomma RPAREN { }
|   pexpr LPAREN expr_or_type_list LDDD ocomma RPAREN { }


keyval: complitexpr LCOLON complitexpr
    {
    }


bare_complitexpr:
|   expr { }
|   LBRACE braced_keyval_list RBRACE { }

complitexpr:
|   expr { }
|   LBRACE braced_keyval_list RBRACE { }


/*
 * list of combo of keyval and val
 */
keyval_list:
|   keyval { }
|   bare_complitexpr { }
|   keyval_list LCOMMA keyval { }
|   keyval_list LCOMMA bare_complitexpr { }

braced_keyval_list:
|/*(*empty*)*/ { }
|   keyval_list ocomma { }


/*(* todo: I don't think we need that with a good fix_tokens_lbody *)*/
lbrace:
|   LBODY { }
|   LBRACE { }

/*(*************************************************************************)*/
/*(*1 Names *)*/
/*(*************************************************************************)*/

sym:
|   LNAME
    {
        (*// during imports, unqualified non-exported identifiers are from builtinpkg 
        if importpkg != nil && !exportname($1.Name) {
            $$ = Pkglookup($1.Name, builtinpkg);
        }
        *)
      $1
    }

/*
 *  newname is used before declared
 *  oldname is used after declared
 */
new_name: sym { }

dcl_name: sym { }


name: sym %prec NotParen
    {
    }

dotname:
|   name { }
|   name LDOT sym { }

packname:
|   LNAME { }
|   LNAME LDOT sym { }

labelname: new_name { }

typedclname:  sym
    {
        (*
        // different from dclname because the name
        // becomes visible right here, not at the end
        // of the declaration.
        *)
    }

/*(*************************************************************************)*/
/*(*1 Types *)*/
/*(*************************************************************************)*/

/*(*
 // to avoid parsing conflicts, type is split into
 //  channel types
 //  function types
 //  parenthesized types
 //  any other type
 // the type system makes additional restrictions,
 // but those are not implemented in the grammar.
 *)*/

ntype:
|   dotname { }

|   ptrtype { }
|   recvchantype { }
|   fntype { }

|   othertype { }
|   LPAREN ntype RPAREN { }

non_recvchantype:
|   dotname { }

|   ptrtype { }
|   fntype { }

|   othertype { }
|   LPAREN ntype RPAREN { }


ptrtype: LMULT ntype { }

recvchantype: LCOMM LCHAN ntype { }

fntype: LFUNC LPAREN oarg_type_list_ocomma RPAREN fnres { }

fnres:
| /*(*empty *)*/    %prec NotParen
    {
    }
|   fnret_type { }
|   LPAREN oarg_type_list_ocomma RPAREN { }

fnret_type:
|   dotname { }

|   ptrtype { } 
|   recvchantype { }
|   fntype { }

|   othertype { }



othertype:
|   LBRACKET oexpr RBRACKET ntype { }
|   LBRACKET LDDD RBRACKET ntype  { }
|   LCHAN non_recvchantype { }
|   LCHAN LCOMM ntype { }
|   LMAP LBRACKET ntype RBRACKET ntype { }
|   structtype { }
|   interfacetype { }

dotdotdot:
|   LDDD
    {
        (* Yyerror("final argument in variadic function missing type"); *)
    }
|   LDDD ntype { }



convtype:
|   fntype { }
|   othertype { }

comptype: othertype { }


expr_or_type:
|   expr { }
|   non_expr_type   %prec PreferToRightParen { }

non_expr_type:
|   fntype { } 
|   recvchantype { }
|   othertype { }
|   LMULT non_expr_type { }

/*(*************************************************************************)*/
/*(*1 Struct/Interface *)*/
/*(*************************************************************************)*/

structtype:
|   LSTRUCT lbrace structdcl_list osemi RBRACE { }
|   LSTRUCT lbrace RBRACE { }

structdcl:
|   new_name_list ntype oliteral { }
|   embed oliteral { }
|   LPAREN embed RPAREN oliteral
    {
        (* Yyerror("cannot parenthesize embedded type"); *)
    }
|   LMULT embed oliteral { }
|   LPAREN LMULT embed RPAREN oliteral
    {
        (* Yyerror("cannot parenthesize embedded type"); *)
    }
|   LMULT LPAREN embed RPAREN oliteral
    {
        (* Yyerror("cannot parenthesize embedded type"); *)
    }

embed: packname { }

interfacetype:
    LINTERFACE lbrace interfacedcl_list osemi RBRACE { }
|   LINTERFACE lbrace RBRACE { }

interfacedcl:
|   new_name indcl { }
|   packname { }
|   LPAREN packname RPAREN
    {
        (* Yyerror("cannot parenthesize embedded type"); *)
    }

indcl: LPAREN oarg_type_list_ocomma RPAREN fnres
    {
        (* // without func keyword *)
    }

/*(*************************************************************************)*/
/*(*1 Function *)*/
/*(*************************************************************************)*/

/*(*
 // all in one place to show how crappy it all is
  *) */
xfndcl: LFUNC fndcl fnbody
    { []
    }

fndcl:
|   sym LPAREN oarg_type_list_ocomma RPAREN fnres { }
|   LPAREN oarg_type_list_ocomma RPAREN sym 
    LPAREN oarg_type_list_ocomma RPAREN fnres
    {
       (*
        if $2 == nil {
            Yyerror("method has no receiver");
            break;
        }
        if $2.Next != nil {
            Yyerror("method has multiple receivers");
            break;
        }
       *)
    }



fnbody:
| /*(*empty *)*/    {  }
|   LBRACE stmt_list RBRACE { }


fnliteral: fnlitdcl lbrace stmt_list RBRACE { }

fnlitdcl: fntype { }

arg_type:
|   name_or_type { }
|   sym name_or_type { }
|   sym dotdotdot { }
|   dotdotdot { }

name_or_type:  ntype { }

/*(*************************************************************************)*/
/*(*1 xxx_opt, xxx_list *)*/
/*(*************************************************************************)*/

/*
 * lists of things
 * note that they are left recursive
 * to conserve yacc stack. they need to
 * be reversed to interpret correctly
 */

/*(* basic lists, 0 element allowed *)*/
elseif_list:
| /*(*empty*)*/ { }
| elseif_list elseif { }

caseblock_list:
| /*(*empty*)*/  { }
| caseblock_list caseblock { }

/*(* lists with ending LSEMICOLON, 0 element allowed *)*/
xdcl_list:
| /*(*empty*)*/    { [] }
|   xdcl_list xdcl LSEMICOLON { $2 @ $1 }

imports:
| /*(* empty *)*/ { [] }
| imports import LSEMICOLON { $2 @ $1 }

/*(* lists with LSEMICOLON separator, at least 1 element *)*/
import_stmt_list:
|   import_stmt                             { }
|   import_stmt_list LSEMICOLON import_stmt { }

vardcl_list:
|   vardcl { }
|   vardcl_list LSEMICOLON vardcl { }

constdcl1_list:
|   constdcl1 { }
|   constdcl1_list LSEMICOLON constdcl1 { }

typedcl_list:
|   typedcl { }
|   typedcl_list LSEMICOLON typedcl { }

structdcl_list:
|   structdcl { }
|   structdcl_list LSEMICOLON structdcl { }

interfacedcl_list:
|   interfacedcl { }
|   interfacedcl_list LSEMICOLON interfacedcl { }

stmt_list:
|   stmt { }
|   stmt_list LSEMICOLON stmt { }


arg_type_list:
|   arg_type { }
|   arg_type_list LCOMMA arg_type { }

new_name_list:
|   new_name { }
|   new_name_list LCOMMA new_name { }

dcl_name_list:
|   dcl_name { }
|   dcl_name_list LCOMMA dcl_name { }

expr_list:
|   expr { }
|   expr_list LCOMMA expr { }

expr_or_type_list:
|   expr_or_type { }
|   expr_or_type_list LCOMMA expr_or_type { }

/*
 * optional things
 */
osemi:
|/*(*empty*)*/ { }
|   LSEMICOLON { }

ocomma:
|/*(*empty*)*/ { }
|   LCOMMA { }

oliteral:
|/*(*empty*)*/ { }
|   LSTR { }


oexpr:
|/*(*empty*)*/ { }
|   expr { }

oexpr_list:
|/*(*empty*)*/ { }
|   expr_list { }

osimple_stmt:
|/*(*empty*)*/ { }
|   simple_stmt { }

onew_name:
|/*(*empty*)*/   {  }
|   new_name { }

oarg_type_list_ocomma:
|/*(*empty*)*/  { }
|   arg_type_list ocomma { }
