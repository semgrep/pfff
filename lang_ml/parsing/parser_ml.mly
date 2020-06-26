%{
(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)
open Common

open Cst_ml

(*************************************************************************)
(* Prelude *)
(*************************************************************************)
(* This file contains a grammar for OCaml (=~ 3.07 with some extensions for 
 * OCaml 4.xxx)
 * 
 * src: adapted from the official source of OCaml in its
 * parsing/ subdirectory. All semantic actions are new. Only the
 * grammar structure was copied.
 * was: $Id: parser.mly 10536 2010-06-07 15:32:32Z doligez $
 *
 * reference:
 * - http://caml.inria.fr/pub/docs/manual-ocaml/language.html
 *   (note that it unfortunately contains conflicts when translated into yacc).
 * 
 * other sources:
 * - http://www.cs.ru.nl/~tews/htmlman-3.10/full-grammar.html
 *   itself derived from the official ocaml reference manual
 *   (also contains conflicts when translated into yacc).
 * - http://www.mpi-sws.org/~rossberg/sml.html
 *   (also contains conflicts when translated into yacc).
 * - http://www.mpi-sws.org/~rossberg/hamlet/
 *   solves ambiguities
 * - linear-ML parser
 *)

(*************************************************************************)
(* Helpers *)
(*************************************************************************)
let (qufix: long_name -> tok -> (string wrap) -> long_name) = 
 fun longname dottok ident ->
  match longname with
  | xs, Name ident2 ->xs @ [Name ident2, dottok], Name ident

let to_item xs =
  xs |> Common.map_filter (function TopItem x -> Some x | _ -> None)

let (^@) sc xs =
  match sc with None -> xs | Some x -> [Right x] @ xs
%}
(*************************************************************************)
(* Tokens *)
(*************************************************************************)

(* unrecognized token, will generate parse error *)
%token <Parse_info.t> TUnknown
%token <Parse_info.t> EOF

(*-----------------------------------------*)
(* The space/comment tokens *)
(*-----------------------------------------*)

(* coupling: Token_helpers.is_real_comment *)
%token <Parse_info.t> TCommentSpace TCommentNewline   TComment
%token <Parse_info.t> TCommentMisc

(*-----------------------------------------*)
(* The normal tokens *)
(*-----------------------------------------*)

(* tokens with "values" *)
%token <string * Parse_info.t> TInt TFloat TChar TString
%token <string * Parse_info.t> TLowerIdent TUpperIdent
%token <string * Parse_info.t> TLabelUse TLabelDecl TOptLabelUse TOptLabelDecl

(* keywords tokens *)
%token <Parse_info.t>
 Tfun Tfunction Trec Ttype Tof Tif Tthen Telse
 Tmatch Twith Twhen
 Tlet Tin Tas
 Ttry Texception
 Tbegin Tend Tfor Tdo Tdone Tdownto Twhile Tto
 Tval Texternal
 Ttrue Tfalse
 Tmodule Topen Tfunctor Tinclude Tsig Tstruct
 Tclass Tnew Tinherit Tconstraint Tinitializer Tmethod Tobject Tprivate
 Tvirtual
 Tlazy Tmutable Tassert
 Tand 
 Tor Tmod Tlor Tlsl Tlsr Tlxor Tasr Tland

(* syntax *)
%token <Parse_info.t> 
 TOParen "(" TCParen ")" TOBrace "{" TCBrace "}" TOBracket "[" TCBracket "]"
 TOBracketPipe "[|" TPipeCBracket "|]"  TOBracketLess "[<" TGreaterCBracket ">]"
 TOBraceLess "{<" TGreaterCBrace ">}"
 TOBracketGreater "[>" TColonGreater ":>"
 TDot "." TDotDot ".."
 TComma "," TEq "=" TAssign ":=" TAssignMutable "<-" 
 TColon ":" TColonColon "::"
 TBang "!" TBangEq "!=" TTilde "~" TPipe "|"
 TSemiColon ";" TSemiColonSemiColon ";;"
 TQuestion "?" TQuestionQuestion "??"
 TUnderscore "_" TStar "*" TArrow "->" TQuote "'" TBackQuote "`" 
 TAnd TAndAnd 
 TSharp "#"
 TMinusDot TPlusDot

(* operators *)
%token <Parse_info.t> TPlus TMinus TLess TGreater
%token <string * Parse_info.t> TPrefixOperator TInfixOperator

(* attributes *)
%token <Parse_info.t> TBracketAt TBracketAtAt TBracketAtAtAt
%token <Parse_info.t> TBracketPercent TBracketPercentPercent

(*-----------------------------------------*)
(* extra tokens: *)
(*-----------------------------------------*)
%token <Parse_info.t> TSharpDirective

(*************************************************************************)
(* Priorities *)
(*************************************************************************)
(* Precedences and associativities.
 *
 * Tokens and rules have precedences.  A reduce/reduce conflict is resolved
 * in favor of the first rule (in source file order).  A shift/reduce conflict
 * is resolved by comparing the precedence and associativity of the token to
 * be shifted with those of the rule to be reduced.
 * 
 * By default, a rule has the precedence of its rightmost terminal (if any).
 * 
 * When there is a shift/reduce conflict between a rule and a token that
 * have the same precedence, it is resolved using the associativity:
 * if the token is left-associative, the parser will reduce; if
 * right-associative, the parser will shift; if non-associative,
 * the parser will declare a syntax error.
 * 
 * We will only use associativities with operators of the kind  x * x -> x
 * for example, in the rules of the form    expr: expr BINOP expr
 * in all other cases, we define two precedences if needed to resolve
 * conflicts.
 * 
 * The precedences must be listed from low to high.
 *)

%nonassoc below_SEMI
%nonassoc TSemiColon                     (* below TEq ({lbl=...; lbl=...}) *)
%nonassoc Tlet                           (* above TSemiColon ( ...; let ... in ...) *)
%nonassoc below_WITH
%nonassoc Tfunction Twith                 (* below TPipe  (match ... with ...) *)
%nonassoc Tthen                          (* below Telse (if ... then ...) *)
%nonassoc Telse                          (* (if ... then ... else ...) *)
%nonassoc TAssignMutable                 (* below TAssign (lbl <- x := e) *)
%right    TAssign                        (* expr (e := e := e) *)
%nonassoc Tas
%left     TPipe                          (* pattern (p|p|p) *)
%nonassoc below_COMMA
%left     TComma                         (* expr/expr_comma_list (e,e,e) *)
%right    TArrow                         (* core_type2 (t -> t -> t) *)

%right    Tor                            (* expr (e || e || e) *)
%right    TAnd TAndAnd                   (* expr (e && e && e) *)
%nonassoc below_EQUAL
%left     TEq TLess TGreater    (* expr (e OP e OP e) *)
%left     TBangEq
%right    TColonColon                    (* expr (e :: e :: e) *)
%left     TPlus TPlusDot TMinus TMinusDot  (* expr (e OP e OP e) *)
%left     TStar                 (* expr (e OP e OP e) *)
%left     TInfixOperator (* pad: *)
%left     Tmod Tlor Tlxor Tland
%right    Tlsr Tasr Tlsl

%nonassoc prec_unary_minus prec_unary_plus (* unary - *)
%nonassoc prec_constant_constructor      (* cf. simple_expr (C versus C x) *)
%nonassoc prec_constr_appl               (* above Tas TPipe TColonColon TComma *)
%nonassoc TSharp                         (* simple_expr/toplevel_directive *)
%nonassoc below_DOT
%nonassoc TDot
(* Finally, the first tokens of simple_expr are above everything else. *)
%nonassoc TBackQuote TBang Tbegin TChar Tfalse TFloat TInt
          TOBrace TOBraceLess TOBracket TOBracketPipe TLowerIdent TOParen
          Tnew TPrefixOperator TString Ttrue TUpperIdent

(*************************************************************************)
(* Rules type declaration *)
(*************************************************************************)
%start <Cst_ml.toplevel list> interface
%start <Cst_ml.toplevel list> implementation
%start <Cst_ml.any> sgrep_spatch_pattern

%%
(*************************************************************************)
(* Macros *)
(*************************************************************************)
list_sep(X,Sep):
 | X                      { [Left $1] }
 | list_sep(X,Sep) Sep X  { $1 @ [Right $2; Left $3] }

(* does not work
list_sep2(X,Sep):
 | X                      { [Left $1] }
 | X Sep                  { [Left $1; Right $2] }
 | list_sep2(X,Sep) Sep X  { $1 @ [Right $2; Left $3] }
*)

listr_sep(X,Sep):
 | X                       { [Left $1] }
 | X Sep listr_sep(X,Sep)  { [Left $1; Right $2] @ $3 }

(* list separated by Sep and possibly terminated by trailing Sep.
 * This has to be recursive on the right, otherwise s/r conflict.
 *)
list_sep_term(X,Sep):
 | X                       { [Left $1] }
 | X Sep                   { [Left $1; Right $2] }
 | X Sep list_sep_term(X,Sep)  { [Left $1; Right $2] @ $3 }

list_and(X): list_sep(X, Tand) { $1 }

qualified(X, Y): 
 | Y       { [], Name $1 }
 | X "." Y { qufix $1 $2 $3 }

(*************************************************************************)
(* TOC *)
(*************************************************************************)
(* - toplevel
 * - signature
 * - structure
 * - names
 * 
 * - expression
 * - type
 * - pattern
 * with for the last 3 sections subsections around values:
 *    - constants
 *    - constructors
 *    - lists
 *    - records
 *    - tuples 
 *    - arrays
 *    - name tags (`Foo)
 * 
 * - let/fun
 * - classes (not in AST)
 * - modules
 * - attributes
 *)

(*************************************************************************)
(* Toplevel, compilation units *)
(*************************************************************************)

interface:      signature EOF                        { $1 }
implementation: structure EOF                        { $1 }

sgrep_spatch_pattern:
 | expr EOF { Expr $1 }
 | signature_item EOF { Item $1 }
 | structure_item_minus_signature_item EOF { Item $1 }

structure_item_minus_signature_item:
 | Tlet Trec? list_and(let_binding)              { Let ($1, $2, $3) }
 (* modules *)
 | Tmodule TUpperIdent module_binding
      { match $3 with
        | None -> ItemTodo $1
        | Some (x, y) -> Module ($1, Name $2, x, y) 
      }
 | Tinclude module_expr                          { ItemTodo $1 }

 (* objects *)
  | Tclass Ttype list_and(class_type_declaration) { ItemTodo $1 }

 | Texception TUpperIdent "=" mod_longident { ItemTodo $1 }
 | floating_attribute { $1 }

(*************************************************************************)
(* Signature *)
(*************************************************************************)

signature:
 | (* empty *)                   { [] }
 | signature signature_item      { $1 @ [TopItem $2] }
 | signature signature_item ";;" { $1 @ [TopItem $2; ScSc $3] }

signature_item: signature_item_noattr post_item_attribute* { $1 }

signature_item_noattr:
 | Ttype list_and(type_declaration)            { Type ($1, $2) }
 | Tval val_ident ":" core_type                { Val ($1, Name $2, $3, $4) }
 | Texternal val_ident ":" core_type "=" primitive_declaration
     { External ($1, Name $2, $3, $4, $5, $6) }
 | Texception TUpperIdent constructor_arguments { Exception ($1, Name $2, $3) }

 (* modules *)
 | Topen mod_longident                          { Open ($1, $2) }

 | Tmodule Ttype ident "=" module_type          { ItemTodo $1 }
 | Tmodule TUpperIdent module_declaration       { ItemTodo $1 }

 (* objects *)
 | Tclass list_and(class_description)           { ItemTodo $1 }

(*----------------------------*)
(* Misc *)
(*----------------------------*)

primitive_declaration: TString+ { $1 }

(*************************************************************************)
(* Structure *)
(*************************************************************************)

(* pad: should not allow those toplevel seq_expr *)
structure:
 |          structure_tail                     { $1 }
 | seq_expr structure_tail                     { TopSeqExpr $1::$2 }

structure_tail:
 | (* empty *)                             { [] }
 | ";;"                                    { [ScSc $1] }
 | ";;" seq_expr structure_tail            { ScSc $1::TopSeqExpr $2::$3 }
 | ";;" structure_item structure_tail      { ScSc $1::TopItem $2::$3 }
 | ";;" TSharpDirective  structure_tail    { ScSc $1::TopDirective $2::$3 }

 | structure_item  structure_tail          { TopItem $1::$2 }
 | TSharpDirective structure_tail          { TopDirective $1::$2 }

structure_item: structure_item_noattr post_item_attribute* { $1 }

structure_item_noattr:
 (* as in signature_item *)
 | Ttype list_and(type_declaration)              { Type ($1, $2) }
 | Texception TUpperIdent constructor_arguments  { Exception ($1, Name $2, $3)}
 | Texternal val_ident ":" core_type "=" primitive_declaration
     { External ($1, Name $2, $3, $4, $5, $6)  }
 | Topen "!"? mod_longident                           { Open ($1, $3) }

 (* start of deviation *)
 | Tlet Trec? list_and(let_binding)              { Let ($1, $2, $3) }
 (* modules *)
 | Tmodule TUpperIdent module_binding
      { match $3 with
        | None -> ItemTodo $1
        | Some (x, y) -> Module ($1, Name $2, x, y) 
      }
 | Tmodule Ttype ident "=" module_type           { ItemTodo $1 }
 | Tinclude module_expr                          { ItemTodo $1 }

 (* objects *)
  | Tclass list_and(class_declaration)            { ItemTodo $1 }
  | Tclass Ttype list_and(class_type_declaration) { ItemTodo $1 }

 | Texception TUpperIdent "=" mod_longident { ItemTodo $1 }
 | floating_attribute { $1 }

(*************************************************************************)
(* Names *)
(*************************************************************************)

val_ident:
 | TLowerIdent                        { $1 }
 | "(" operator ")"                   { ("TODOOPERATOR", $1) }

operator:
 | TPrefixOperator      { } 
 | TInfixOperator       { }
 | "*"     { } | "="       { } | ":="   { } | "!"     { }
  (* but not Tand, because of conflict ? *)
 | Tor       { } | TAnd      { }
 | Tmod      { } | Tland     { } | Tlor      { } | Tlxor     { }
 | Tlsl      { } | Tlsr      { } | Tasr      { }
 | TPlus     { } | TPlusDot  { } | TMinus    { } | TMinusDot { }
 | TLess     { } | TGreater  { }
 | TAndAnd { } | TBangEq { }

(* for polymorphic types both 'a and 'A is valid. Same for module types. *)
ident:
 | TUpperIdent                                      { $1 }
 | TLowerIdent                                      { $1 }


constr_ident:
 | TUpperIdent     { $1 }
 | "(" ")"         { "()TODO", $1 }
 | "::"            { "::", $1 }
 | Tfalse          { "false", $1 }
 | Ttrue           { "true", $1 }
(*  | "[" "]"                           { } *)
(*  | "(" "::" ")"                    { "::" } *)

(* record field name (not olabl label) *)
label: TLowerIdent  { $1 }

(* name tag extension (polymorphic variant?) *)
name_tag: "`" ident   { $1 }

(*----------------------------*)
(* Labels (olabl labels) *)
(*----------------------------*)

label_var: TLowerIdent    { }

(* for label arguments like ~x or ?x *)
label_ident: TLowerIdent   { $1 }
 
(*----------------------------*)
(* Qualified names *)
(*----------------------------*)

mod_longident:
 | TUpperIdent                      { [], Name $1 }
 | mod_longident "." TUpperIdent    { qufix $1 $2 $3 }

mod_ext_longident:
 | TUpperIdent                                 { [], Name $1 }
 | mod_ext_longident "." TUpperIdent           { qufix $1 $2 $3 }
 | mod_ext_longident "(" mod_ext_longident ")" { [], Name ("TODOEXTMO", $2) }


constr_longident:
 | mod_longident   %prec below_DOT     { $1 }
 | "[" "]"                             { [], Name ("[]TODO", $1) }
 | "(" ")"                             { [], Name ("()TODO", $1) }
 | Tfalse                              { [], Name ("false", $1) }
 | Ttrue                               { [], Name ("true", $1) }

type_longident: qualified(mod_ext_longident, TLowerIdent) { $1 }
val_longident:  qualified(mod_longident, val_ident) { $1 }
(* record field name *)
label_longident: qualified(mod_longident, TLowerIdent) { $1 }
class_longident: qualified(mod_longident, TLowerIdent) { $1 }
mty_longident:   qualified(mod_ext_longident, ident) { $1 }
(* it's mod_ext_longident, not mod_longident *)
clty_longident: qualified(mod_ext_longident, TLowerIdent) { $1 }

(*************************************************************************)
(* Expressions *)
(*************************************************************************)

seq_expr:
 | expr      %prec below_SEMI   { [Left $1] }
 | expr ";" seq_expr            { Left $1::Right $2::$3 }
 (* bad ? should be removed ? but it's convenient in certain contexts like
  * begin end to allow ; as a terminator *)
 | expr ";"                     { [Left $1; Right $2] }


expr:
 | simple_expr                               { $1 }
 (* function application *)
 | simple_expr labeled_simple_expr+
     { match $1 with
       | L name -> FunCallSimple (name, $2)
       | _      -> FunCall ($1, $2) }

 | Tlet Trec? list_and(let_binding) Tin seq_expr  { LetIn ($1, $2, $3, $4, $5)}

 | Tfun labeled_simple_pattern fun_def
     { let (params, (tok, e)) = $3 in
       Fun ($1, $2::params, tok, e) }

 | Tfunction "|"? match_cases                { Function ($1, $2 ^@ $3) }

 | expr_comma_list %prec below_COMMA         { Tuple $1 }
 | constr_longident simple_expr              { Constr ($1, Some $2) }

 | expr "::" expr            { Infix ($1, ("::", $2), $3) (* TODO? ConsList?*)}

 | expr TInfixOperator expr  { Infix ($1, $2, $3) }

 | expr Tmod expr            { Infix ($1, ("mod", $2), $3) }
 | expr Tland expr           { Infix ($1, ("land", $2), $3) }
 | expr Tlor expr            { Infix ($1, ("lor", $2), $3) }
 | expr Tlxor expr           { Infix ($1, ("lxor", $2), $3) }
 
 | expr Tlsl expr            { Infix ($1, ("lsl", $2), $3) }
 | expr Tlsr expr            { Infix ($1, ("lsr", $2), $3) }
 | expr Tasr expr            { Infix ($1, ("asr", $2), $3) }

 | expr TBangEq expr         { Infix ($1, ("!=", $2), $3) }

 | Tif seq_expr Tthen expr Telse expr   { If ($1, $2, $3, $4, Some ($5, $6)) }
 | Tif seq_expr Tthen expr              { If ($1, $2, $3, $4, None) }

 | Tmatch seq_expr Twith "|"? match_cases   { Match ($1, $2, $3, $4 ^@ $5) }

 | Ttry seq_expr Twith "|"? match_cases     { Try ($1, $2, $3, $4 ^@ $5) }

 | Twhile seq_expr Tdo seq_expr Tdone       { While ($1, $2, $3, $4, $5) }
 | Tfor val_ident "=" seq_expr direction_flag seq_expr Tdo seq_expr Tdone
     { For ($1, Name $2, $3, $4, $5, $6, $7, $8, $9)  }

 | expr ":=" expr { RefAssign ($1, $2, $3) }

 | expr "=" expr   { Infix ($1, ("=", $2), $3) }

 | expr TPlus expr     { Infix ($1, ("+", $2), $3)  }
 | expr TMinus expr    { Infix ($1, ("-", $2), $3) }
 | expr TPlusDot expr  { Infix ($1, ("+.", $2), $3) }
 | expr TMinusDot expr { Infix ($1, ("-.", $2), $3) }
 | expr "*" expr        { Infix ($1, ("*", $2), $3) }
 | expr TLess expr     { Infix ($1, ("<", $2), $3) }
 | expr TGreater expr  { Infix ($1, (">", $2), $3) }
 | expr Tor expr       { Infix ($1, ("or", $2), $3) }
 | expr TAnd expr      { Infix ($1, ("&", $2), $3) }
 | expr TAndAnd expr   { Infix ($1, ("&&", $2), $3) }

 | subtractive expr %prec prec_unary_minus    { Prefix ($1, $2) }
 | additive expr %prec prec_unary_plus        { Prefix ($1, $2) }

 | simple_expr "." label_longident "<-" expr  { FieldAssign ($1,$2,$3,$4,$5) }

 (* array extension *)
 | simple_expr "." "(" seq_expr ")" "<-" expr { ExprTodo $2 }
 | simple_expr "." "[" seq_expr "]" "<-" expr { ExprTodo $2 }
 (* bigarray extension, a.{i} <- v *)
 | simple_expr "." "{" expr "}" "<-" expr     { ExprTodo $2 }
     
 | Tlet Topen mod_longident Tin seq_expr      { ExprTodo $1 }

 | Tassert simple_expr                        { ExprTodo $1 }

 | name_tag simple_expr                       { ExprTodo $1 }

 | Tlazy simple_expr                          { ExprTodo $1 }

  (* objects *)
 | label "<-" expr                            { ExprTodo $2 }



simple_expr:
 | constant          { C $1 }
 | val_longident     { L $1 }
 (* this includes 'false' *)
 | constr_longident %prec prec_constant_constructor  { Constr ($1, None) }

 | simple_expr "." label_longident  { FieldAccess ($1, $2, $3) }

 (* if only one expr then prefer to generate a ParenExpr *)
 | "(" seq_expr ")"
     { match $2 with
     | [] -> Sequence ($1, $2, $3) 
     | [Left x] -> ParenExpr ($1, x, $3)
     | [Right _] -> raise Impossible
     | _ -> Sequence ($1, $2, $3) 
     }

 | Tbegin seq_expr Tend     { Sequence ($1, $2, $3)  }
 | Tbegin Tend              { Sequence ($1, [], $2) }

 (* bugfix: must be in simple_expr. Originally made the mistake to put it
  * in expr: and the parser would then not recognize things like 'foo !x' *)
 | TPrefixOperator simple_expr   { Prefix ($1, $2) }
 | "!" simple_expr               { RefAccess ($1, $2) }

 | "{" record_expr "}"           { Record ($1, $2, $3) }
 | "["  list_sep_term(expr, ";") "]"   { List ($1, $2, $3) }
 | "[|" list_sep_term(expr, ";")? "|]" { ExprTodo $1 }

 (* array extension *)
 | simple_expr "." "(" seq_expr ")"  { ExprTodo $2 }
 | simple_expr "." "[" seq_expr "]"  { ExprTodo $2 }
 (* bigarray extension *)
 | simple_expr "." "{" expr "}"      { ExprTodo $2 }

 (* object extension *)
 | simple_expr "#" label             { ObjAccess ($1, $2, Name $3) }
 | Tnew class_longident              { New ($1, $2) }
 | "{<" list_sep_term(field_expr, ";") ">}"         { ExprTodo $1 }

 (* name tag extension *)
 | name_tag %prec prec_constant_constructor  { ExprTodo $1 }

 | "(" seq_expr type_constraint ")"          { ExprTodo $1 }

 (* scoped open, 3.12 *)
 | mod_longident "." "(" seq_expr ")"       { ExprTodo $2 }


labeled_simple_expr:
 | simple_expr     { ArgExpr $1 }
 | label_expr      { $1 }

(* a bit different than list_sep() *)
expr_comma_list:
 | expr_comma_list "," expr                  { $1 @ [Right $2; Left $3] }
 | expr "," expr                             { [Left $1; Right $2; Left $3] }


record_expr:
 |                   list_sep_term(lbl_expr, ";")  { RecordNormal ($1) }
 | simple_expr Twith list_sep_term(lbl_expr, ";")  { RecordWith ($1, $2, $3) }

lbl_expr: 
 | label_longident "=" expr { FieldExpr ($1, $2, $3) }
 (* new 3.12 feature! *)
 | label_longident          { FieldImplicitExpr ($1) }

additive:
  | TPlus                                        { "+", $1 }
  | TPlusDot                                     { "+.", $1 }

subtractive:
  | TMinus                                       { "-", $1 }
  | TMinusDot                                    { "-.", $1 }

direction_flag:
 | Tto                                          { To $1 }
 | Tdownto                                      { Downto $1 }

(*----------------------------*)
(* Constants *)
(*----------------------------*)

constant:
 | TInt     { Int $1 }
 | TChar    { Char $1 }
 | TString  { String $1 }
 | TFloat   { Float $1 }

(*----------------------------*)
(* Labels *)
(*----------------------------*)

label_expr:
 | "~" label_ident        { ArgImplicitTildeExpr ($1, Name $2) }
 | "?" label_ident        { ArgImplicitQuestionExpr ($1, Name $2) }
 | TLabelDecl simple_expr { ArgLabelTilde (Name $1 (* TODO del ~/:? *), $2) }
 | TOptLabelDecl simple_expr { ArgLabelQuestion (Name $1 (* del too *), $2) }

(*----------------------------*)
(* objects *)
(*----------------------------*)

field_expr: label "=" expr { }

(*************************************************************************)
(* Patterns *)
(*************************************************************************)

match_case: pattern match_action { ($1, $2) }

(* cant factorize with list_sep, or listr_sep *)
match_cases:
 | match_case                   { [Left ($1)] }
 | match_cases "|" match_case   { $1 @ [Right $2; Left ($3)] }

match_action:
 |                "->" seq_expr   { Action ($1, $2) }
 | Twhen seq_expr "->" seq_expr   { WhenAction ($1, $2, $3, $4) }


pattern:
 | simple_pattern   { $1 }

 | constr_longident pattern %prec prec_constr_appl  { PatConstr ($1, Some $2) }
 | pattern_comma_list       %prec below_COMMA     { PatTuple ($1) }
 | pattern "::" pattern                           { PatConsInfix ($1, $2, $3) }

 | pattern Tas val_ident                          { PatAs ($1, $2, Name $3) }

 (* nested patterns *)
 | pattern "|" pattern                            { PatDisj ($1, $2, $3) }

 (* name tag extension *)
 | name_tag pattern %prec prec_constr_appl        { PatTodo $1 }




simple_pattern:
 | val_ident %prec below_EQUAL      { PatVar (Name $1) }
 | constr_longident                 { PatConstr ($1, None) }
 | "_"                              { PatUnderscore $1 }
 | signed_constant                  { PatConstant $1 }

 | "{" lbl_pattern_list record_pattern_end "}" { PatRecord ($1,$2,(*$3*) $4) }
 | "["  list_sep_term(pattern, ";")  "]"       { PatList (($1, $2, $3)) }
 | "[|" list_sep_term(pattern, ";")? "|]"      { PatTodo $1 }

 (* note that let (x:...) a =  will trigger this rule *)
 | "(" pattern ":" core_type ")"               { PatTyped ($1, $2, $3, $4, $5)}

 (* name tag extension *)
 | name_tag                    { PatTodo $1 }
 (* range extension *)
 | TChar ".." TChar            { PatTodo $2 }

 | "(" pattern ")"             { ParenPat ($1, $2, $3) }

lbl_pattern: 
 | label_longident "=" pattern               { PatField ($1, $2, $3) }
 | label_longident                           { PatImplicitField ($1) }

(* cant factorize with list_sep or list_sep_term *)
lbl_pattern_list:
 | lbl_pattern { [Left $1] }
 | lbl_pattern_list ";" lbl_pattern { $1 @ [Right $2; Left $3] }

record_pattern_end:
 | ";"?                      { }
 (* new 3.12 feature! *)
 | ";" "_" ";"?              { }

(* not exactly like list_sep() *)
pattern_comma_list:
 | pattern_comma_list "," pattern            { $1 @ [Right $2; Left $3] }
 | pattern "," pattern                       { [Left $1; Right $2; Left $3] }


signed_constant:
 | constant       { C2 $1 }
 | TMinus TInt    { CMinus ($1, Int $2) }
 | TMinus TFloat  { CMinus ($1, Float $2) }
 | TPlus TInt     { CPlus ($1, Int $2) }
 | TPlus TFloat   { CPlus ($1, Float $2) }

(*************************************************************************)
(* Types *)
(*************************************************************************)

type_constraint:
 | ":" poly_type           { }
 (* object cast extension *)
 | ":>" core_type    { }

(*----------------------------*)
(* Types definitions *)
(*----------------------------*)

type_declaration: type_parameters TLowerIdent type_kind (*TODO constraints*)
   { match $3 with
     | None -> TyAbstract ($1, Name $2)
     | Some (tok_eq, type_kind) -> TyDef ($1, Name $2, tok_eq, type_kind)
   }


type_kind:
 | (*empty*)
      { None }
 | "=" core_type
      { Some ($1, TyCore $2) }
 | "=" list_sep(constructor_declaration, "|")
      { Some ($1, TyAlgebric $2) }
 | "=" (*TODO private_flag*) "|" list_sep(constructor_declaration, "|")
      { Some ($1, TyAlgebric (Right $2::$3)) }
 | "=" (*TODO private_flag*) "{" list_sep_term(label_declaration, ";") "}"
      { Some ($1, TyRecord ($2, ($3), $4)) }


constructor_declaration: constr_ident constructor_arguments  { Name $1, $2 }

constructor_arguments:
 | (*empty*)                                { NoConstrArg }
 | Tof list_sep(simple_core_type, "*")      { Of ($1, $2) }

type_parameters:
 |  (*empty*)                          { TyNoParam  }
 | type_parameter                      { TyParam1 $1 }
 | "(" list_sep(type_parameter, ",") ")" { TyParamMulti (($1, $2, $3)) }

type_parameter: (*TODO type_variance*) "'" ident   { ($1, Name $2) }

label_declaration: Tmutable? label ":" poly_type          
   { { fld_mutable = $1; fld_name = Name $2; fld_tok = $3; fld_type = $4; } }

(*----------------------------*)
(* Types expressions *)
(*----------------------------*)

core_type: core_type2 { $1 }

core_type2:
 | simple_core_type_or_tuple
     { $1 }
 | core_type2 "->" core_type2
     { TyFunction ($1, $2, $3) }

 (* ext: olabl *)
 | TLowerIdent     ":" core_type2 "->" core_type2
     { TyFunction ($3, $4, $5) (* TODO $1 $2 *)  }
 | "?" TLowerIdent ":" core_type2 "->" core_type2
     { TyFunction ($4, $5, $6) (* TODO $1 $2 *)  }
 (* pad: only because of lexer hack around labels *)
 | TOptLabelDecl    core_type2 "->" core_type2
     { TyFunction ($2, $3, $4) (* TODO $1 $2 *)  }


simple_core_type_or_tuple:
 | simple_core_type                        { $1 }
 | simple_core_type "*" list_sep(simple_core_type, "*")
     { TyTuple (Left $1::Right $2::$3) }


simple_core_type:
 | simple_core_type2   { $1 }
 (* weird diff between 'Foo of a * b' and 'Foo of (a * b)' *)
 | "(" list_sep(core_type, ",") ")" { TyTuple2 (($1, $2, $3)) }

simple_core_type2:
 | "'" ident                                     { TyVar ($1, Name $2) }
 | type_longident                                { TyName ($1) }
 | simple_core_type2 type_longident              { TyApp (TyArg1 $1, $2) }
 | "(" list_sep(core_type, ",") ")" type_longident 
      { TyApp (TyArgMulti (($1, $2, $3)), $4) }

 (* name tag extension *)
 | "[" row_field "|" list_sep(row_field, "|") "]"          { TyTodo $1 }
 | "["           "|" list_sep(row_field, "|") "]"          { TyTodo $1 }
 | "[" tag_field "]"                             { TyTodo $1 }

 (* objects types *)
  | TLess meth_list TGreater                    { TyTodo $1 }
  | TLess TGreater                              { TyTodo $1 }


meth_list:
  | field ";" meth_list                     { }
  | field ";"?                              {  }
  | ".."                                    {  }

field: label ":" poly_type             { }

(*----------------------------*)
(* Misc *)
(*----------------------------*)

poly_type: 
 | type_parameter "." core_type { $3 (* TODO AST $1 *) } 
 | core_type { $1 }

row_field:
 | tag_field                                   { }
 | simple_core_type2                           { }

tag_field:
 | name_tag Tof TAnd? list_and(core_type)   { }
 | name_tag       { }

(*************************************************************************)
(* Let/Fun definitions *)
(*************************************************************************)

let_binding:
 | val_ident fun_binding
      { let (params, (teq, body)) = $2 in
        LetClassic { l_name = Name $1; l_params = params; l_tok = teq;
                     l_body = body;
        } }
 | pattern "=" seq_expr
      { LetPattern ($1, $2, $3) }


fun_binding:
 | strict_binding               { $1 }
 (* let x arg1 arg2 : t = e *)
 | type_constraint "=" seq_expr { [], ($2, $3) (* TODO return triple with $1*)}

strict_binding:
 (* simple values, e.g. 'let x = 1' *)
 | "=" seq_expr  { [], ($1, $2) }
 (* function values, e.g. 'let x a b c = 1' *)
 | labeled_simple_pattern fun_binding { let (args, body) = $2 in $1::args,body}

fun_def:
 | "->" expr                       { [], ($1, $2) }
 | labeled_simple_pattern fun_def  { let (args, body) = $2 in $1::args, body }


labeled_simple_pattern:
  | simple_pattern { ParamPat $1 }
  | label_pattern  { $1 }

opt_default:
 | (*empty*)               { None  }
 | "=" seq_expr            { Some ($1, $2) }

(*----------------------------*)
(* Labels *)
(*----------------------------*)

label_pattern:
  | "~" label_var                                 { ParamTodo $1 }
  (* ex: let x ~foo:a *)
  | TLabelDecl simple_pattern                     { ParamTodo (snd $1) }
  | "~" "(" label_let_pattern ")"                 { ParamTodo $1 }
  | "?" "(" label_let_pattern opt_default ")"     { ParamTodo $1 }
  | "?" label_var                                 { ParamTodo $1 }
 
label_let_pattern:
 | label_var                { }
 | label_var ":" core_type  { }

(*************************************************************************)
(* Classes *)
(*************************************************************************)

(*----------------------------*)
(* Class types *)
(*----------------------------*)
class_description: Tvirtual? class_type_parameters TLowerIdent ":" class_type
  { }

class_type_declaration: 
  Tvirtual? class_type_parameters TLowerIdent "=" class_signature
  { }

class_type:
  | class_signature { }
  | simple_core_type_or_tuple "->" class_type { }

class_signature:
  | actual_class_parameters  clty_longident               {  }
  | Tobject class_sig_body Tend  {  }

class_sig_body: class_self_type class_sig_fields { }

class_self_type:
  | (*empty*) {  }
  | "(" core_type ")"  { }

class_sig_fields: class_sig_field* { }

class_sig_field:
  | Tinherit class_signature    {  }
  | virtual_method_type        {  }
  | method_type                {  }
  | Tval value_type            {  }

method_type: Tmethod Tprivate? label ":" poly_type { }

virtual_method_type:
  | Tmethod Tprivate Tvirtual label ":" poly_type    {  }
  | Tmethod Tvirtual Tprivate? label ":" poly_type   {  }

value_type:
  | Tvirtual Tmutable? label ":" core_type     { }
  | Tmutable Tvirtual? label ":" core_type  {  }
  | label ":" core_type     {  }

(*----------------------------*)
(* Class expressions *)
(*----------------------------*)

(*----------------------------*)
(* Class definitions *)
(*----------------------------*)

class_declaration: 
 Tvirtual? class_type_parameters TLowerIdent class_fun_binding
      { }

class_type_parameters:
  | (*empty*)                              { }
  | "[" list_sep(type_parameter, ",") "]"  { }

class_fun_binding:
  | "=" class_expr  { }
  | labeled_simple_pattern class_fun_binding  { }

class_expr:
  | class_simple_expr                         { }
  | Tfun class_fun_def                        { }
  | class_simple_expr labeled_simple_expr+    { }
  | Tlet Trec? list_and(let_binding) Tin class_expr    { }

%inline
actual_class_parameters: 
 | "[" list_sep(core_type, ",") "]"  { }
 | (* empty *) { }

class_simple_expr:
  | actual_class_parameters class_longident   { }
  | Tobject class_structure Tend                   { }
  | "(" class_expr ")"                             { }

class_fun_def:
  | labeled_simple_pattern "->" class_expr   { }
  | labeled_simple_pattern class_fun_def     { }

class_structure: class_self_pattern class_fields { }

class_self_pattern:
  | "(" pattern ")"                { }
  | "(" pattern ":" core_type ")"  { }
  | (*empty*)                      { }


class_fields: class_field* { }

class_field:
  | Tinherit "!"? class_expr parent_binder   { }
  | Tval virtual_value  { }
  | Tval value          { }
  | virtual_method      { }
  | concrete_method     { }
  | Tinitializer seq_expr  { }

parent_binder:
  | Tas TLowerIdent { }
  | (* empty *) { }

virtual_value:
  | "!"? Tmutable Tvirtual label ":" core_type  { }
  |      Tvirtual Tmutable label ":" core_type                { }

value:
  | "!"? ioption(Tmutable) label "=" seq_expr      { }
  | "!"? ioption(Tmutable) label type_constraint "=" seq_expr  { }

virtual_method:
  | Tmethod "!"? Tprivate Tvirtual label ":" poly_type  { }
  | Tmethod "!"? Tvirtual Tprivate? label ":" poly_type { }

concrete_method:
  | Tmethod "!"? Tprivate? label strict_binding  { }
  | Tmethod "!"? Tprivate? label ":" poly_type "=" seq_expr { }

(*************************************************************************)
(* Modules *)
(*************************************************************************)

module_binding:
 | "=" module_expr                                     { Some ($1, $2) }
 | "(" TUpperIdent ":" module_type ")" module_binding  { None }
 | ":" module_type "=" module_expr                     { (*$1 *) Some($3, $4) }

module_declaration:
 | ":" module_type  { }
 | "(" TUpperIdent ":" module_type ")" module_declaration { }

(*----------------------------*)
(* Module types *)
(*----------------------------*)

module_type:
 | mty_longident         { }
 | Tsig signature Tend   { }
 | Tfunctor "(" TUpperIdent ":" module_type ")" "->" module_type
    %prec below_WITH
      { }
 | module_type Twith list_and(with_constraint) { }
 | "(" module_type ")"     { }


with_constraint:
 | Ttype type_parameters label_longident with_type_binder core_type 
    (*constraints*)
   { }

with_type_binder:
 | "="           {  }
 | "=" Tprivate  {  }

(*----------------------------*)
(* Module expressions *)
(*----------------------------*)

module_expr:
  (* when just do a module aliasing *)
  | mod_longident       { ModuleName $1 }
  (* nested modules *)
  | Tstruct structure Tend { ModuleStruct ($1, to_item $2, $3) }
  (* functor definition *)
  | Tfunctor "(" TUpperIdent ":" module_type ")" "->" module_expr 
     { ModuleTodo $1 }
  (* module/functor application *)
  | module_expr "(" module_expr ")" { ModuleTodo $2 }

(*************************************************************************)
(* Attributes *)
(*************************************************************************)

(*pad: this is a limited implementation for now; just enough for efuns/pfff *)
floating_attribute: TBracketAtAtAt attr_id payload "]" { ItemTodo $1 }

single_attr_id:
  | TLowerIdent { $1 }
  | TUpperIdent { $1 }
(* should also put all keywords here, but bad practice no? *)

attr_id: listr_sep(single_attr_id, ".") { $1 }

post_item_attribute: TBracketAtAt attr_id payload "]" { }

payload: structure { }
