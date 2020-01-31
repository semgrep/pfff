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
open Common
open Ast_generic (* for the arithmetic operator *)
open Ast_go

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let error tok s =
  pr2 s;
  raise (Parse_info.Parsing_error tok)

let mk_vars_or_consts xs type_opt exprs_opt mk_var_or_const = 
  let xs = List.rev xs in
  let ys = 
    match exprs_opt with
    | None -> []
    | Some ys -> List.rev ys
  in
  (* less: for consts we should copy the last value *)
  let rec aux xs ys =
    match xs, ys with
    | [], [] -> []
    | x::xs, [] -> mk_var_or_const x type_opt None :: aux xs ys 
    | x::xs, y::ys -> mk_var_or_const x type_opt (Some y) :: aux xs ys
    | [], _y::_ys -> 
        failwith "more values than entities"
  in
  aux xs ys

let mk_vars xs type_opt exprs_opt =
  mk_vars_or_consts xs type_opt exprs_opt 
    (fun a b c -> DVar (a,b,c))

let mk_consts xs type_opt exprs_opt =
  mk_vars_or_consts xs type_opt exprs_opt 
    (fun a b c -> DConst (a,b,c))
 

let mk_bin e1 op tok e2 =
  Binary (e1, (op, tok), e2)
let mk_unary op tok e = 
  Unary ((op, tok), e)
let mk_arg x =
  match x with
  | Left e -> Arg e
  | Right t -> ArgType t

let condition_of_stmt tok stmt =
  match stmt with
  | ExprStmt e -> e
  | _ -> error tok "condition is not an expression"

let mk_else elseifs else_ = 
  let elseifs = List.rev elseifs in
  List.fold_right (fun elseif accu ->
      let ((stopt, cond), body) = elseif in
      Some (If (stopt, cond, body, accu))
  ) elseifs else_

let rec expr_to_type tok e =
  match e with
  | Id (id, _) -> TName [id]
  | Deref (_, e) -> TPtr (expr_to_type tok e)
  | Selector (Id (id1, _), _, id2) -> TName [id1;id2]
  | ParenType t -> t
  | _ -> 
      pr2_gen e;
      error tok "TODO: expr_to_type"

let expr_or_type_to_type tok x = 
  match x with
  | Right t -> t
  | Left e -> expr_to_type tok e

let type_to_id x =
  match x with
  | TName [id] -> id
  | _ -> 
    pr2_gen x;
    failwith "type_to_id: was expecting an id"

(* see golang spec on signatures. If you have
 * func foo(a, b, c) then it means a, b, and c are types. If you have once
 * an identifier and a type, as in 
 * func foo(a, b, c, d e) this means a, b, c, and d are of type e.
 *)
let adjust_signatures params =
  let params = List.rev params in
  let all_types = 
   params |> List.for_all (function {pname = None; _} -> true | _ ->false) in
  if all_types
  then params
  else 
    let rec aux acc xs =
      match xs with
      | [] -> if acc = [] 
              then [] 
              else begin 
                pr2_gen acc;
                failwith "last parameter should have a type and id"
              end
      | x::xs ->
        (match x with
          | { pname = Some _; ptype = t; _ } ->
             ((acc |> List.rev |> List.map (fun id -> 
               { pname = Some id; ptype = t; pdots = None })) @ [x]) @
               aux [] xs
          | { pname = None; ptype = id_typ; _ } ->
            let id = type_to_id id_typ in
            aux (id::acc) xs
        )
    in
    aux [] params

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
%token  <Ast_generic.arithmetic_operator * Ast_go.tok> LASOP 
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

sgrep_spatch_pattern: 
 | expr EOF       { E $1 }
 | stmt LSEMICOLON EOF       { S $1 }
 | stmt LSEMICOLON stmt LSEMICOLON stmt_list EOF { Ss ($1::$3::List.rev $5) }

/*(*************************************************************************)*/
/*(*1 Import *)*/
/*(*************************************************************************)*/

import:
|   LIMPORT import_stmt { [$2] }
|   LIMPORT LPAREN import_stmt_list osemi RPAREN { List.rev $3 }
|   LIMPORT LPAREN RPAREN { [] }

import_stmt:
|        LSTR  
    { { i_path = $1; i_kind = ImportOrig } (*// import with original name*) }
|   sym  LSTR  
    { { i_path = $2; i_kind = ImportNamed $1 }(*// import with given name*)  }
|   LDOT LSTR  
    { { i_path = $2; i_kind = ImportDot $1 }(*// import into my name space *) }

/*(*************************************************************************)*/
/*(*1 Declarations *)*/
/*(*************************************************************************)*/

xdcl:
|   common_dcl { $1 |> List.map (fun decl -> D decl) }
|   xfndcl     { [$1] }

common_dcl:
|   LVAR vardcl  { $2 }
|   LVAR LPAREN vardcl_list osemi RPAREN { List.rev $3 }
|   LVAR LPAREN RPAREN { [] }

    /*(* at least the first const has a value *)*/
|   LCONST constdcl { $2 }
|   LCONST LPAREN constdcl osemi RPAREN { $3 }
|   LCONST LPAREN constdcl LSEMICOLON constdcl1_list osemi RPAREN 
      { $3 @ (List.rev $5) }
|   LCONST LPAREN RPAREN { [] }

|   LTYPE typedcl { [$2] }
|   LTYPE LPAREN typedcl_list osemi RPAREN { List.rev $3 }
|   LTYPE LPAREN RPAREN { [] }


vardcl:
|   dcl_name_list ntype               { mk_vars $1 (Some $2) None }
|   dcl_name_list ntype LEQ expr_list { mk_vars $1 (Some $2) (Some $4) }
|   dcl_name_list       LEQ expr_list { mk_vars $1 None      (Some $3) }

/*(* this enforces the const has a value *)*/
constdcl:
|   dcl_name_list ntype LEQ expr_list { mk_consts $1 (Some $2) (Some $4)  }
|   dcl_name_list       LEQ expr_list { mk_consts $1 None (Some $3) }

constdcl1:
|   constdcl            { $1 }
|   dcl_name_list ntype { mk_consts $1 (Some $2) None }
|   dcl_name_list       { mk_consts $1 None None }


typedcl: 
| typedclname ntype     { DTypeDef ($1, $2) }
/*(* alias decl, go 1.?? *)*/
| typedclname LEQ ntype { DTypeAlias ($1, $2, $3) }

/*(*************************************************************************)*/
/*(*1 Statements *)*/
/*(*************************************************************************)*/

stmt:
| /*(*empty*)*/   { Empty }
| compound_stmt   { $1 }
| common_dcl      { DeclStmts $1 }
| non_dcl_stmt    { $1 }

compound_stmt: LBRACE stmt_list RBRACE { Block (List.rev $2) }

non_dcl_stmt:
|   simple_stmt { SimpleStmt $1 }

|   if_stmt { $1 }
|   for_stmt { $1 }
|   switch_stmt { $1 }
|   select_stmt { $1 }

|   labelname LCOLON stmt { Label ($1, $3) }
|   LGOTO new_name        { Goto ($1, $2) }

|   LBREAK onew_name    { Break ($1, $2) }
|   LCONTINUE onew_name { Continue ($1, $2) }
|   LRETURN oexpr_list  { Return ($1, $2) }
|   LFALL { Fallthrough $1 }

|   LGO pseudocall    { Go ($1, $2) }
|   LDEFER pseudocall { Defer ($1, $2) }


simple_stmt:
|   expr                       { ExprStmt $1 }
|   expr LASOP expr            { AssignOp ($1, $2, $3) }
|   expr_list LEQ expr_list    { Assign (List.rev $1, $2, List.rev $3)  }
|   expr_list LCOLAS expr_list { DShortVars ($1, $2, $3) }
|   expr LINC                  { IncDec ($1, (Incr, $2), Postfix) }
|   expr LDEC                  { IncDec ($1, (Decr, $2), Postfix) }




/*(* IF cond body (ELSE IF cond body)* (ELSE block)? *) */
if_stmt: LIF  if_header loop_body elseif_list else_
    { match $2 with
      | stopt, Some st -> If (stopt, condition_of_stmt $1 st, $3,mk_else $4 $5)
      | _, None -> error $1 "missing condition in if statement"
    }

if_header:
|   osimple_stmt { (None, $1) }
|   osimple_stmt LSEMICOLON osimple_stmt { ($1, $3) }


elseif: LELSE LIF  if_header loop_body
    { match $3 with
      | stopt, Some st -> (stopt, condition_of_stmt $2 st), $4
      | _, None -> error $2 "missing condition in if statement"
    }

else_:
| /*(*empty*)*/         { None }
|   LELSE compound_stmt { Some $2 }


for_stmt: 
 | LFOR osimple_stmt LSEMICOLON osimple_stmt LSEMICOLON osimple_stmt loop_body
    { For (($2, Common.map_opt (condition_of_stmt $1) $4, $6), $7) }
 | LFOR osimple_stmt loop_body 
    { match $2 with
      | None ->    For ((None, None, None), $3)
      | Some st -> For ((None, Some (condition_of_stmt $1 st), None), $3)
    }
 | LFOR expr_list LEQ    LRANGE expr loop_body
    { Range (Some (List.rev $2, $3), $4, $5, $6)  }
 | LFOR expr_list LCOLAS LRANGE expr loop_body
    { Range (Some (List.rev $2, $3), $4, $5, $6) }
 | LFOR                  LRANGE expr loop_body
    { Range (None, $2, $3, $4) }


loop_body: LBODY stmt_list RBRACE { Block (List.rev $2) }


/*(* split in 2, switch expr and switch types *)*/
switch_stmt: LSWITCH if_header LBODY caseblock_list RBRACE 
    { let stopt1, stopt2 = $2 in
      Switch (stopt1, stopt2 ,List.rev $4)
    }

select_stmt:  LSELECT LBODY caseblock_list RBRACE 
    { Select ($1, List.rev $3) }

case:
|   LCASE expr_or_type_list LCOLON             { CaseExprs $2 }
|   LCASE expr_or_type_list LEQ expr LCOLON    { CaseAssign ($2, $3, $4) }
|   LCASE expr_or_type_list LCOLAS expr LCOLON { CaseAssign ($2, $3, $4) }
|   LDEFAULT LCOLON                            { CaseDefault $1 }

caseblock: case stmt_list
    {
      $1, Block (List.rev $2)
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
|   uexpr              { $1 }

|   expr LOROR expr    { mk_bin $1 Or $2 $3 }
|   expr LANDAND expr  { mk_bin $1 And $2 $3 }
|   expr LEQEQ expr    { mk_bin $1 Eq $2 $3 }
|   expr LNE expr      { mk_bin $1 NotEq $2 $3 }
|   expr LLT expr      { mk_bin $1 Lt $2 $3 }
|   expr LLE expr      { mk_bin $1 LtE $2 $3 }
|   expr LGE expr      { mk_bin $1 GtE $2 $3 }
|   expr LGT expr      { mk_bin $1 Gt $2 $3 }
|   expr LPLUS expr    { mk_bin $1 Plus $2 $3 }
|   expr LMINUS expr   { mk_bin $1 Minus $2 $3 }
|   expr LPIPE expr    { mk_bin $1 BitOr $2 $3 }
|   expr LHAT expr     { mk_bin $1 BitXor $2 $3 }
|   expr LMULT expr    { mk_bin $1 Mult $2 $3 }
|   expr LDIV expr     { mk_bin $1 Div $2 $3 }
|   expr LPERCENT expr { mk_bin $1 Mod $2 $3 }
|   expr LAND expr     { mk_bin $1 BitAnd $2 $3 }
|   expr LANDNOT expr  { mk_bin $1 BitNot (* BitAndNot aka BitClear *) $2 $3 }
|   expr LLSH expr     { mk_bin $1 LSL $2 $3 }
|   expr LRSH expr     { mk_bin $1 LSR $2 $3 }

/*(* old: was in expression, to give better error message, but better here *)*/
|   expr LCOMM expr    { Send ($1, $2, $3) }

uexpr:
|   pexpr { $1 }

|   LMULT uexpr { Deref ($1, $2)}
|   LAND uexpr
    {
           (* // Special case for &T{...}: turn into ( *T){...}. *)
      Ref ($1, $2)
    }
|   LPLUS  uexpr { mk_unary Plus $1 $2 }
|   LMINUS uexpr { mk_unary Minus $1 $2 }
|   LBANG  uexpr { mk_unary Not $1 $2 }
|   LHAT uexpr { mk_unary BitXor $1 $2  }
|   LCOMM uexpr { Receive ($1, $2) }

pexpr:
|   pexpr_no_paren { $1 }

|   LPAREN expr_or_type RPAREN 
    { match $2 with
      | Left e -> e
      | Right t -> ParenType t
    }


pexpr_no_paren:
|   basic_literal { BasicLit $1 }

|   name { Id ($1, ref None) }

    /*(* can be many things *)*/
|   pexpr LDOT sym { Selector ($1, $2, $3) }

|   pexpr LDOT LPAREN expr_or_type RPAREN 
    { TypeAssert ($1, expr_or_type_to_type $2 $4) }
    /*(* less: only inside a TypeSwitch, rewrite grammar? *)*/
|   pexpr LDOT LPAREN LTYPE RPAREN 
    { TypeSwitchExpr ($1, $3) }

|   pexpr LBRACKET expr RBRACKET { Index ($1, $3) }
|   pexpr LBRACKET oexpr LCOLON oexpr RBRACKET { Slice ($1, ($3, $5, None)) }
|   pexpr LBRACKET oexpr LCOLON oexpr LCOLON oexpr RBRACKET 
    { Slice ($1, ($3, $5, $7))
        (*if $5 == nil {
            Yyerror("middle index required in 3-index slice");
        }
        if $7 == nil {
            Yyerror("final index required in 3-index slice");
        }
        *)
    }

|   pseudocall { Call $1 }

|   convtype LPAREN expr ocomma RPAREN { Cast ($1, $3) }

|   comptype       lbrace braced_keyval_list RBRACE 
    { CompositeLit ($1, $3) }
|   pexpr_no_paren LBRACE braced_keyval_list RBRACE 
    { CompositeLit (expr_to_type $2 $1, $3) }

|   fnliteral { $1 }


basic_literal:
| LINT   { Int $1 }
| LFLOAT { Float $1 }
| LIMAG  { Imag $1 }
| LRUNE  { Rune $1 }
| LSTR   { String $1 }


/*
 * call-like statements that
 * can be preceded by 'defer' and 'go'
 */
pseudocall:
|   pexpr LPAREN RPAREN                               
      { ($1, []) }
|   pexpr LPAREN expr_or_type_list ocomma RPAREN      
      { ($1, $3 |> List.rev |> List.map mk_arg) }
|   pexpr LPAREN expr_or_type_list LDDD ocomma RPAREN 
      { let args = 
          match $3 |> List.map mk_arg with
          | [] -> raise Impossible
          | (Arg e)::xs -> List.rev ((ArgDots (e, $4))::xs)
          | (ArgDots _)::_ -> raise Impossible
          | (ArgType _t)::_ -> raise Impossible
         in
         $1, args 
      }


braced_keyval_list:
|/*(*empty*)*/         { [] }
|   keyval_list ocomma { List.rev $1 }

/*
 * list of combo of keyval and val
 */
keyval_list:
|   keyval                              { [$1] }
|   bare_complitexpr                    { [$1] }

|   keyval_list LCOMMA keyval           { $3 :: $1 }
|   keyval_list LCOMMA bare_complitexpr { $3 :: $1 }

keyval: complitexpr LCOLON complitexpr { InitKeyValue ($1, $2, $3) }

complitexpr:
|   expr { InitExpr $1 }
|   LBRACE braced_keyval_list RBRACE { InitBraces ($2) }

bare_complitexpr:
|   expr { InitExpr $1 }
|   LBRACE braced_keyval_list RBRACE { InitBraces $2 }





/*(* less: I don't think we need that with a good fix_tokens_lbody *)*/
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
new_name: sym { $1 }

dcl_name: sym { $1 }

name: sym %prec NotParen { $1 }

labelname: new_name { $1 }

typedclname:  sym
    { $1
        (*
        // different from dclname because the name
        // becomes visible right here, not at the end
        // of the declaration.
        *)
    }


dotname:
|   name { [$1] }
|   name LDOT sym { [$1; $3] }

packname:
|   LNAME { [$1] }
|   LNAME LDOT sym { [$1; $3] }

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
|   dotname      { TName $1 }

|   ptrtype      { $1 }
|   recvchantype { $1 }
|   fntype       { TFunc $1 }

|   othertype           { $1 }
|   LPAREN ntype RPAREN { $2 }

non_recvchantype:
|   dotname { TName $1 }

|   ptrtype { $1 }
|   fntype  { TFunc $1 }

|   othertype { $1 }
|   LPAREN ntype RPAREN { $2 }


ptrtype: LMULT ntype { TPtr $2 }

recvchantype: LCOMM LCHAN ntype { TChan (TRecv, $3) }

fntype: LFUNC LPAREN oarg_type_list_ocomma RPAREN fnres 
  { { fparams = $3; fresults = $5 } }

fnres:
| /*(*empty *)*/    %prec NotParen      { [] }
|   fnret_type                          
    { [{ pname = None; ptype = $1; pdots = None }] }
|   LPAREN oarg_type_list_ocomma RPAREN { $2 }

fnret_type:
|   dotname      { TName $1 }

|   ptrtype      { $1 } 
|   recvchantype { $1 }
|   fntype       { TFunc $1 }

|   othertype    { $1 }



othertype:
|   LBRACKET oexpr RBRACKET ntype 
      { match $2 with None -> TSlice $4 | Some e -> TArray (e, $4) }
|   LBRACKET LDDD RBRACKET ntype  
      { TArrayEllipsis ($2, $4) }

|   LCHAN non_recvchantype { TChan (TBidirectional, $2) }
|   LCHAN LCOMM ntype      { TChan (TSend, $3) }

|   LMAP LBRACKET ntype RBRACKET ntype { TMap ($3, $5) }

|   structtype    { $1 }
|   interfacetype { $1 }


dotdotdot:
|   LDDD ntype { $1, $2 }


convtype:
|   fntype    { TFunc $1 }
|   othertype { $1 }

comptype: 
| othertype { $1 }


expr_or_type:
|   expr                                     { Left $1 }
|   non_expr_type   %prec PreferToRightParen { Right $1 }

non_expr_type:
|   fntype              { TFunc $1 } 
|   recvchantype        { $1 }
|   othertype           { $1 }
|   LMULT non_expr_type { TPtr ($2) }

/*(*************************************************************************)*/
/*(*1 Struct/Interface *)*/
/*(*************************************************************************)*/

structtype:
|   LSTRUCT lbrace structdcl_list osemi RBRACE { TStruct (List.rev $3) }
|   LSTRUCT lbrace RBRACE                      { TStruct [] }

structdcl:
|   new_name_list ntype oliteral 
    { $1 |> List.map (fun id -> Field (id, $2), $3) }
|         packname      oliteral { [EmbeddedField (None, $1), $2] }
|   LMULT packname      oliteral { [EmbeddedField (Some $1, $2), $3] }


interfacetype:
    LINTERFACE lbrace interfacedcl_list osemi RBRACE { TInterface(List.rev $3)}
|   LINTERFACE lbrace RBRACE                         { TInterface [] }

interfacedcl:
|   new_name indcl { Method ($1, $2) }
|   packname       { EmbeddedInterface $1 }

/*(* fntype // without func keyword *)*/
indcl: LPAREN oarg_type_list_ocomma RPAREN fnres
   { { fparams = $2; fresults = $4 } }

/*(*************************************************************************)*/
/*(*1 Function *)*/
/*(*************************************************************************)*/

/*(*
 // all in one place to show how crappy it all is
  *) */
xfndcl: LFUNC fndcl fnbody
    { $2 $3 }

fndcl:
|   sym LPAREN oarg_type_list_ocomma RPAREN fnres 
    { fun body -> DFunc ($1, ({ fparams = $3; fresults = $5 }, body)) }
|   LPAREN oarg_type_list_ocomma RPAREN sym 
    LPAREN oarg_type_list_ocomma RPAREN fnres
    {
      fun body ->
        match $2 with
        | [x] -> DMethod ($4, x, ({ fparams = $6; fresults = $8 }, body))
        | [] -> error $1 "method has no receiver"
        | _::_::_ -> error $1 "method has multiple receivers"
    }

fnbody:
|  /*(*empty *)*/          {  Empty }
|  LBRACE stmt_list RBRACE { Block (List.rev $2) }


fnliteral: fnlitdcl lbrace stmt_list RBRACE 
    { FuncLit ($1, stmt1 (List.rev $3)) }

fnlitdcl: fntype { $1 }

arg_type:
|       name_or_type { { pname= None; ptype = $1; pdots = None } }
|   sym name_or_type { { pname= Some $1; ptype = $2; pdots = None } }
|   sym dotdotdot    { { pname= Some $1; ptype = snd $2; pdots = Some (fst $2)}}
|       dotdotdot    { { pname= None; ptype = snd $1; pdots = Some (fst $1)} }

name_or_type:  ntype { $1 }

arg_type_list:
|   arg_type                      { [$1] }
|   arg_type_list LCOMMA arg_type { $3::$1 }

oarg_type_list_ocomma:
|/*(*empty*)*/  { [] }
|   arg_type_list ocomma { adjust_signatures $1  }

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
| /*(*empty*)*/      { [] }
| elseif_list elseif { $2::$1 }

caseblock_list:
| /*(*empty*)*/  { [] }
| caseblock_list caseblock { $2::$1 }

/*(* lists with ending LSEMICOLON, 0 element allowed *)*/
xdcl_list:
| /*(*empty*)*/    { [] }
|   xdcl_list xdcl LSEMICOLON { $2 @ $1 }

imports:
| /*(* empty *)*/ { [] }
| imports import LSEMICOLON { $2 @ $1 }

/*(* lists with LSEMICOLON separator, at least 1 element *)*/
import_stmt_list:
|   import_stmt                             { [$1] }
|   import_stmt_list LSEMICOLON import_stmt { $3::$1 }

vardcl_list:
|   vardcl { $1 }
|   vardcl_list LSEMICOLON vardcl { $3 @ $1 }

constdcl1_list:
|   constdcl1 { $1 }
|   constdcl1_list LSEMICOLON constdcl1 { $3 @ $1 }

typedcl_list:
|   typedcl { [$1] }
|   typedcl_list LSEMICOLON typedcl { $3::$1 }

structdcl_list:
|   structdcl { $1 }
|   structdcl_list LSEMICOLON structdcl { $3 @ $1 }

interfacedcl_list:
|   interfacedcl { [$1] }
|   interfacedcl_list LSEMICOLON interfacedcl { $3::$1 }

stmt_list:
|   stmt { [$1] }
|   stmt_list LSEMICOLON stmt { $3::$1 }



new_name_list:
|   new_name { [$1] }
|   new_name_list LCOMMA new_name { $3::$1 }

dcl_name_list:
|   dcl_name { [$1] }
|   dcl_name_list LCOMMA dcl_name { $3::$1 }

expr_list:
|   expr { [$1] }
|   expr_list LCOMMA expr { $3::$1 }

expr_or_type_list:
|   expr_or_type { [$1] }
|   expr_or_type_list LCOMMA expr_or_type { $3::$1 }

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
|/*(*empty*)*/ { None }
|   LSTR       { Some $1 }


oexpr:
|/*(*empty*)*/ { None }
|   expr       { Some $1 }

oexpr_list:
|/*(*empty*)*/ { None }
|   expr_list  { Some (List.rev $1) }

osimple_stmt:
|/*(*empty*)*/  { None }
|   simple_stmt { Some $1 }

onew_name:
|/*(*empty*)*/   { None  }
|   new_name     { Some $1 }

