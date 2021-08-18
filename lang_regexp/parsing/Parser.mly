(*
   Menhir parser for regexps.
*)

%{
open AST
%}

%token <AST.loc * string> COMMENT
%token <AST.loc * AST.group_kind> OPEN_GROUP
%token <AST.loc> CLOSE_GROUP BAR END
%token <AST.loc * AST.char_class> CHAR
%token <AST.loc * AST.repeat_range * AST.matching_pref> QUANTIFIER

%left BAR

%start main
%type <AST.t> main
%%

main: regexp0 END { $1 }

/* May be empty */
regexp0:
  | alt                  { $1 }
  | seq                  { $1 }
  |                      { Empty AST.dummy_loc }

alt:
  | regexp0 BAR regexp0  { let a = $1 and b = $3 in
                           Alt (location2 a b, a, b)
                         }

seq:
  | repeat seq           { let a = $1 and b = $2 in
                                   Seq (location2 a b, a, b)
                         }
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
  | CHAR                         { let loc, char_class = $1 in
                                   Char (loc, char_class)
                                 }

  | OPEN_GROUP regexp0 CLOSE_GROUP
                                 { let (start, _), kind = $1 in
                                   let _, end_ = $3 in
                                   let loc = start, end_ in
                                   Group (loc, kind, $2)
                                 }

  | COMMENT                      { let loc, _s = $1 in
                                   Empty loc
                                 }
