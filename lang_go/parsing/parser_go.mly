%{
(* // Copyright 2009 The Go Authors. All rights reserved.
 * // Use of this source code is governed by a BSD-style
 * // license that can be found in the LICENSE file.
 *
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
 *)

(*
func fixlbrace(lbr int) {
    // If the opening brace was an LBODY,
    // set up for another one now that we're done.
    // See comment in lex.C about loophack.
    if lbr == LBODY {
        loophack = true
    }
*)
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

%token  <string * Ast_go.tok>
  LINT LFLOAT 
  LIMAG
  LRUNE LSTR

%token  <string * Ast_go.tok> LASOP 

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

%token  <string * Ast_go.tok> LNAME 

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
%left       NotPackage
%left       LPACKAGE

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

file: package imports xdcl_list EOF { }

sgrep_spatch_pattern: EOF { }

package: LPACKAGE sym LSEMICOLON { }

/*(*************************************************************************)*/
/*(*1 Import *)*/
/*(*************************************************************************)*/

imports:
| /*(* empty *)*/ { }
| imports import LSEMICOLON { }

import:
|   LIMPORT import_stmt { }
|   LIMPORT LPAREN import_stmt_list osemi RPAREN { }
|   LIMPORT LPAREN RPAREN { }

import_stmt: import_here  { }

import_stmt_list:
|   import_stmt                             { }
|   import_stmt_list LSEMICOLON import_stmt { }

import_here:
|   LSTR
    {
        (*// import with original name*)
    }
|   sym LSTR
    {
        (*// import with given name*)
    }
|   '.' LSTR
    {
        (*// import into my name space *)
    }

/*(*************************************************************************)*/
/*(*1 Declarations *)*/
/*(*************************************************************************)*/

xdcl:
|   common_dcl { }
|   xfndcl { }

common_dcl:
|   LVAR vardcl
    {
    }
|   LVAR LPAREN vardcl_list osemi RPAREN
    {
    }
|   LVAR LPAREN RPAREN
    {
    }

|   lconst constdcl
    {
    }
|   lconst LPAREN constdcl osemi RPAREN
    {
    }
|   lconst LPAREN constdcl LSEMICOLON constdcl_list osemi RPAREN
    {
    }
|   lconst LPAREN RPAREN
    {
    }

|   LTYPE typedcl
    {
    }
|   LTYPE LPAREN typedcl_list osemi RPAREN
    {
    }
|   LTYPE LPAREN RPAREN
    {
    }

lconst:
    LCONST
    {
    }

vardcl:
|   dcl_name_list ntype
    {
    }
|   dcl_name_list ntype LEQ expr_list
    {
    }
|   dcl_name_list LEQ expr_list
    {
    }

constdcl:
|   dcl_name_list ntype LEQ expr_list
    {
    }
|   dcl_name_list LEQ expr_list
    {
    }

constdcl1:
|   constdcl { }
|   dcl_name_list ntype
    {
    }
|   dcl_name_list
    {
    }

typedclname:  sym
    {
        (*
        // different from dclname because the name
        // becomes visible right here, not at the end
        // of the declaration.
        *)
    }

typedcl: typedclname ntype
    {
    }

/*(*************************************************************************)*/
/*(*1 Statements *)*/
/*(*************************************************************************)*/

simple_stmt:
|   expr
    {
    }
|   expr LASOP expr
    {
    }
|   expr_list LEQ expr_list
    {
    }
|   expr_list LCOLAS expr_list
    {
    }
|   expr LINC
    {
    }
|   expr LDEC
    {
    }

case:
|   LCASE expr_or_type_list LCOLON
    {
    }
|   LCASE expr_or_type_list LEQ expr LCOLON
    {
    }
|   LCASE expr_or_type_list LCOLAS expr LCOLON
    {
    }
|   LDEFAULT LCOLON
    {
    }

compound_stmt: LBRACE stmt_list RBRACE
    {
    }

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

caseblock_list:
|  /*(*empty*)*/  
    {
    }
|   caseblock_list caseblock
    {
    }

loop_body: LBODY stmt_list RBRACE
    {
    }

range_stmt:
|   expr_list LEQ LRANGE expr
    {
    }
|   expr_list LCOLAS LRANGE expr
    {
    }
|   LRANGE expr
    {
    }

for_header:
|   osimple_stmt LSEMICOLON osimple_stmt LSEMICOLON osimple_stmt
    {
    }
|   osimple_stmt
    {
    }
|   range_stmt { }

for_body: for_header loop_body
    {
    }

for_stmt: LFOR for_body
    {
    }

if_header:
|   osimple_stmt
    {
    }
|   osimple_stmt LSEMICOLON osimple_stmt
    {
    }

/*(* IF cond body (ELSE IF cond body)* (ELSE block)? *) */
if_stmt: LIF  if_header loop_body elseif_list else_
    {
        (* if $3.Left == nil
            Yyerror("missing condition in if statement");
        *)
    }

elseif: LELSE LIF  if_header loop_body
    {
        (* if $4.Left == nil {
            Yyerror("missing condition in if statement");
        } *)
    }

elseif_list:
| /*(*empty*)*/    
    {
    }
|   elseif_list elseif
    {
    }

else_:
| /*(*empty*)*/    
    {
    }
|   LELSE compound_stmt
    {
    }

switch_stmt: LSWITCH if_header LBODY caseblock_list RBRACE
    {
    }

select_stmt:  LSELECT LBODY caseblock_list RBRACE
    {
    }

/*(*************************************************************************)*/
/*(*1 Expressions *)*/
/*(*************************************************************************)*/

expr:
|   uexpr { }
|   expr LOROR expr
    {
    }
|   expr LANDAND expr
    {
    }
|   expr LEQEQ expr
    {
    }
|   expr LNE expr
    {
    }
|   expr LLT expr
    {
    }
|   expr LLE expr
    {
    }
|   expr LGE expr
    {
    }
|   expr LGT expr
    {
    }
|   expr LPLUS expr
    {
    }
|   expr LMINUS expr
    {
    }
|   expr LPIPE expr
    {
    }
|   expr LHAT expr
    {
    }
|   expr LMULT expr
    {
    }
|   expr LDIV expr
    {
    }
|   expr LPERCENT expr
    {
    }
|   expr LAND expr
    {
    }
|   expr LANDNOT expr
    {
    }
|   expr LLSH expr
    {
    }
|   expr LRSH expr
    {
    }
/*(* not an expression anymore, but left in so we can give a good error *)*/
|   expr LCOMM expr
    {
    }

uexpr:
|   pexpr { }
|   LMULT uexpr
    {
    }
|   LAND uexpr
    {
           (* // Special case for &T{...}: turn into ( *T){...}. *)
    }
|   LPLUS uexpr
    {
    }
|   LMINUS uexpr
    {
    }
|   LBANG uexpr
    {
    }
|   LTILDE uexpr
    {
        (* Yyerror("the bitwise complement operator is ^"); *)
    }
|   LHAT uexpr
    {
    }
|   LCOMM uexpr
    {
    }

/*
 * call-like statements that
 * can be preceded by 'defer' and 'go'
 */
pseudocall:
|   pexpr LPAREN RPAREN
    {
    }
|   pexpr LPAREN expr_or_type_list ocomma RPAREN
    {
    }
|   pexpr LPAREN expr_or_type_list LDDD ocomma RPAREN
    {
    }

basic_literal:
| LINT { }
| LFLOAT { }
| LIMAG { }
| LRUNE { }
| LSTR { }

pexpr_no_paren:
|   basic_literal
    {
    }
|   name {  }
|   pexpr LDOT sym
    {
    }
|   pexpr LDOT LPAREN expr_or_type RPAREN
    {
    }
|   pexpr LDOT LPAREN LTYPE RPAREN
    {
    }
|   pexpr LBRACKET expr RBRACKET
    {
    }
|   pexpr LBRACKET oexpr LCOLON oexpr RBRACKET
    {
    }
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
|   convtype LPAREN expr ocomma RPAREN
    {
    }
|   comptype lbrace braced_keyval_list RBRACE
    {
        (* fixlbrace($2); *)
    }
|   pexpr_no_paren LBRACE braced_keyval_list RBRACE
    {
    }
|   LPAREN expr_or_type RPAREN LBRACE braced_keyval_list RBRACE
    {
        (* Yyerror("cannot parenthesize type in composite literal"); *)
    }
|   fnliteral { }


keyval: complitexpr LCOLON complitexpr
    {
    }

bare_complitexpr:
|   expr
    {
    }
|   LBRACE braced_keyval_list RBRACE
    {
    }

complitexpr:
|   expr { }
|   LBRACE braced_keyval_list RBRACE
    {
    }

pexpr:
|   pexpr_no_paren { }
|   LPAREN expr_or_type RPAREN
    {
    }

expr_or_type:
|   expr { }
|   non_expr_type   %prec PreferToRightParen { }

name_or_type:  ntype { }

lbrace:
|   LBODY
    {
    }
|   LBRACE
    {
    }

/*(*************************************************************************)*/
/*(*1 Names *)*/
/*(*************************************************************************)*/

/*
 *  newname is used before declared
 *  oldname is used after declared
 */
new_name: sym
    {
    }

dcl_name: sym
    {
    }

onew_name:
|/*(*empty*)*/   {  }
|   new_name { }

sym:
|   LNAME
    {
        (*// during imports, unqualified non-exported identifiers are from builtinpkg 
        if importpkg != nil && !exportname($1.Name) {
            $$ = Pkglookup($1.Name, builtinpkg);
        }
        *)
    }

name: sym %prec NotParen
    {
    }

labelname: new_name { }

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

dotdotdot:
|   LDDD
    {
        (* Yyerror("final argument in variadic function missing type"); *)
    }
|   LDDD ntype
    {
    }

ntype:
|   recvchantype { }
|   fntype { }
|   othertype { }
|   ptrtype { }
|   dotname { }
|   LPAREN ntype RPAREN
    {
    }

non_expr_type:
|   recvchantype { }
|   fntype { } 
|   othertype { }
|   LMULT non_expr_type
    {
    }

non_recvchantype:
|   fntype { }
|   othertype { }
|   ptrtype { }
|   dotname { }
|   LPAREN ntype RPAREN
    {
    }

convtype:
|   fntype { }
|   othertype { }

comptype: othertype { }

fnret_type:
|   recvchantype { }
|   fntype { }
|   othertype { }
|   ptrtype { } 
|   dotname { }

dotname:
|   name { }
|   name LDOT sym
    {
    }

othertype:
|   LBRACKET oexpr RBRACKET ntype
    {
    }
|   LBRACKET LDDD RBRACKET ntype
    {
    }
|   LCHAN non_recvchantype
    {
    }
|   LCHAN LCOMM ntype
    {
    }
|   LMAP LBRACKET ntype RBRACKET ntype
    {
    }
|   structtype { }
|   interfacetype { }

ptrtype: LMULT ntype
    {
    }

recvchantype: LCOMM LCHAN ntype
    {
    }

structtype:
|   LSTRUCT lbrace structdcl_list osemi RBRACE
    {
        (* fixlbrace($2); *)
    }
|   LSTRUCT lbrace RBRACE
    {
        (* fixlbrace($2); *)
    }

interfacetype:
    LINTERFACE lbrace interfacedcl_list osemi RBRACE
    {
        (* fixlbrace($2); *)
    }
|   LINTERFACE lbrace RBRACE
    {
        (* fixlbrace($2); *)
    }

/*(*************************************************************************)*/
/*(*1 Function *)*/
/*(*************************************************************************)*/

/*(*
 // all in one place to show how crappy it all is
  *) */
xfndcl: LFUNC fndcl fnbody
    {
    }

fndcl:
|   sym LPAREN oarg_type_list_ocomma RPAREN fnres
    {
    }
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


fntype: LFUNC LPAREN oarg_type_list_ocomma RPAREN fnres
    {
    }

fnbody:
| /*(*empty *)*/    {
    }
|   LBRACE stmt_list RBRACE
    {
    }

fnres:
| /*(*empty *)*/    %prec NotParen
    {
    }
|   fnret_type
    {
    }
|   LPAREN oarg_type_list_ocomma RPAREN
    {
    }

fnlitdcl: fntype
    {
    }

fnliteral:
    fnlitdcl lbrace stmt_list RBRACE
    {
        (* fixlbrace($2); *)
    }

/*(*************************************************************************)*/
/*(*1 xxx_opt, xxx_list *)*/
/*(*************************************************************************)*/

/*
 * lists of things
 * note that they are left recursive
 * to conserve yacc stack. they need to
 * be reversed to interpret correctly
 */
xdcl_list:
| /*(*empty*)*/    {
    }
|   xdcl_list xdcl LSEMICOLON
    {
    }

vardcl_list:
|   vardcl { }
|   vardcl_list LSEMICOLON vardcl
    {
    }

constdcl_list:
|   constdcl1 { }
|   constdcl_list LSEMICOLON constdcl1
    {
    }

typedcl_list:
|   typedcl
    {
     }
|   typedcl_list LSEMICOLON typedcl
    {
    }

structdcl_list:
|   structdcl { }
|   structdcl_list LSEMICOLON structdcl
    {
    }

interfacedcl_list:
|   interfacedcl
    {
    }
|   interfacedcl_list LSEMICOLON interfacedcl
    {
    }

/*(*************************************************************************)*/
/*(*1 TODO *)*/
/*(*************************************************************************)*/

structdcl:
|   new_name_list ntype oliteral
    {
    }
|   embed oliteral
    {
    }
|   LPAREN embed RPAREN oliteral
    {
        (* Yyerror("cannot parenthesize embedded type"); *)
    }
|   LMULT embed oliteral
    {
    }
|   LPAREN LMULT embed RPAREN oliteral
    {
        (* Yyerror("cannot parenthesize embedded type"); *)
    }
|   LMULT LPAREN embed RPAREN oliteral
    {
        (* Yyerror("cannot parenthesize embedded type"); *)
    }

packname:
|   LNAME
    {
    }
|   LNAME LDOT sym
    {
    }

embed: packname
    {
    }

interfacedcl:
|   new_name indcl
    {
    }
|   packname
    {
    }
|   LPAREN packname RPAREN
    {
        (* Yyerror("cannot parenthesize embedded type"); *)
    }

indcl: LPAREN oarg_type_list_ocomma RPAREN fnres
    {
        (* // without func keyword *)
    }

/*(*************************************************************************)*/
/*(*1 Function arguments *)*/
/*(*************************************************************************)*/

arg_type:
|   name_or_type { }
|   sym name_or_type
    {
    }
|   sym dotdotdot
    {
    }
|   dotdotdot { }

arg_type_list:
|   arg_type
    {
    }
|   arg_type_list LCOMMA arg_type
    {
    }

oarg_type_list_ocomma:
|/*(*empty*)*/  
    {
    }
|   arg_type_list ocomma
    {
    }

/*(*************************************************************************)*/
/*(*1 TODO *)*/
/*(*************************************************************************)*/

stmt:
| /*(*empty*)*/    
    {
    }
|   compound_stmt { }
|   common_dcl
    {
    }
|   non_dcl_stmt { }

non_dcl_stmt:
|   simple_stmt { }
|   for_stmt { }
|   switch_stmt { }
|   select_stmt { }
|   if_stmt { }
|   labelname LCOLON stmt
    {
    }
|   LFALL
    {
    }
|   LBREAK onew_name
    {
    }
|   LCONTINUE onew_name
    {
    }
|   LGO pseudocall
    {
    }
|   LDEFER pseudocall
    {
    }
|   LGOTO new_name
    {
    }
|   LRETURN oexpr_list
    {
    }

stmt_list:
|   stmt
    {
    }
|   stmt_list LSEMICOLON stmt
    {
    }

new_name_list:
|   new_name
    {
    }
|   new_name_list LCOMMA new_name
    {
    }

dcl_name_list:
|   dcl_name
    {
    }
|   dcl_name_list LCOMMA dcl_name
    {
    }

expr_list:
|   expr
    {
    }
|   expr_list LCOMMA expr
    {
    }

expr_or_type_list:
|   expr_or_type
    {
    }
|   expr_or_type_list LCOMMA expr_or_type
    {
    }

/*
 * list of combo of keyval and val
 */
keyval_list:
|   keyval
    {
    }
|   bare_complitexpr
    {
    }
|   keyval_list LCOMMA keyval
    {
    }
|   keyval_list LCOMMA bare_complitexpr
    {
    }

braced_keyval_list:
|/*(*empty*)*/    
    {
    }
|   keyval_list ocomma
    {
    }

/*
 * optional things
 */
osemi:
|/*(*empty*)*/ { }
|   LSEMICOLON { }

ocomma:
|/*(*empty*)*/ { }
|   LCOMMA { }

oexpr:
|/*(*empty*)*/ { }
|   expr { }

oexpr_list:
|/*(*empty*)*/ { }
|   expr_list { }

osimple_stmt:
|/*(*empty*)*/ { }
|   simple_stmt { }


oliteral:
|/*(*empty*)*/ { }
|   LSTR { }
