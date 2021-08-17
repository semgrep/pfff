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

%start main
%type <AST.t> main
%%

main:
  | regexp END { $1 }
  | END { Empty $1 }

regexp:
  | regexp regexp  { let a = $1 and b = $2 in
                     Seq (location2 a b, a, b)
                   }
  | regexp BAR regexp { let a = $1 and b = $3 in
                        Alt (location2 a b, a, b)
                      }
  | regexp QUANTIFIER { let a = $1 in
                        let (_, end_), range, match_pref = $2 in
                        let loc = fst (location a), end_ in
                        Repeat (loc, a, range, match_pref)
                      }

  | CHAR          { let loc, char_class = $1 in
                    Char (loc, char_class) }
  | OPEN_GROUP regexp CLOSE_GROUP
                  { let (start, _), kind = $1 in
                    let _, end_ = $3 in
                    let loc = start, end_ in
                    Group (loc, kind, $2)
                  }
  | OPEN_GROUP CLOSE_GROUP
                  { let (start, _), kind = $1 in
                    let _, end_ = $2 in
                    let loc = start, end_ in
                    Group (loc, kind, Empty loc)
                  }
  | COMMENT regexp { $2 }
