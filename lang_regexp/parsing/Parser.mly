(*
   Menhir parser for regexps.
*)

%{
open AST
%}

%token <AST.t> NODE DIRECTIVE
%token <AST.loc * AST.group_kind * AST.t option> OPEN_GROUP
%token <AST.loc> CLOSE_GROUP BAR END
%token <AST.loc * AST.repeat_range * AST.matching_pref> QUANTIFIER

/* "Directives" like '(?i)' have weaker precedence than even alternatives.
   e.g. '(?i)a|b' matches the same input as '(?i)(a|b)'. */
%nonassoc DIRECTIVE
/* Choosing right associativity for aesthetic purposes when dumping the AST
   and consistency with the sequence operation.
   Any associativity is correct. */
%right BAR

%start main
%type <AST.t> main
%%

main: regexp0 END       { $1 }

/* May be empty */
regexp0:
  | alt                  { $1 }
  | seq                  { $1 }
  |                      { Empty AST.dummy_loc }

alt:
  | DIRECTIVE regexp0    { seq $1 $2 }
  | regexp0 BAR regexp0  { let a = $1 and b = $3 in
                           Alt (location2 a b, a, b)
                         }

seq:
  | repeat seq           { seq $1 $2 }
  | repeat               { $1 }

repeat:
  | regexp1 QUANTIFIER   { let a = $1 in
                           let (_, end_), range, match_pref = $2 in
                           let loc = fst (location a), end_ in
                           Repeat (loc, a, range, match_pref)
                         }
  | regexp1              { $1 }

/* May not be empty */
regexp1:
  | NODE                         { $1 }

  | OPEN_GROUP regexp0 CLOSE_GROUP
                                 { let (start, _), kind, opts = $1 in
                                   let contents = $2 in
                                   let _, end_ = $3 in
                                   let loc = start, end_ in
                                   let contents =
                                     match opts with
                                     | None -> contents
                                     | Some node -> seq node contents
                                   in
                                   Group (loc, kind, contents)
                                 }
