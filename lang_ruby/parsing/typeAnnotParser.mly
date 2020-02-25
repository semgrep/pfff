%{
  open Printf

  let merge_quantifiers tvars cons pos = 
    let rec work acc tvars cons = match tvars, cons with
      | tvars, [] -> 
          let rest = List.map (fun x -> x,None) tvars in
            List.rev_append rest acc
      | [], (tv,b)::_ -> 
          Log.fatal (Log.of_loc pos) 
            "The type variable %a can not be used as a lower bound at this point."
            Annotation.format_quant_var tv
      | tv::rest_tvars, _ -> 
          let bounds,rest_cons = 
            List.partition (fun (tv',bound) -> tv = tv') cons
          in
            match bounds with
              | [] -> work ((tv,None)::acc) rest_tvars rest_cons
              | [(_,t)] -> work ((tv,Some t)::acc) rest_tvars rest_cons
              | lst ->
                  let t = Annotation.Type_Union (List.map snd lst) in
                    work ((tv,Some t)::acc) rest_tvars rest_cons
    in
    let final = work [] tvars cons in
      List.rev final

  let pragma key = match key with
    | "FIXME" -> Annotation.Type_Fixme
    | _ -> failwith ("Uknown pragma keyword: "^key)

%}

%token T_EOF
%token <Lexing.position> K_DEF K_CLASS K_MODULE K_CAST
%token <Lexing.position> K_OR K_SELF

%token <Lexing.position> T_BEGIN_LINE T_NEWLINE T_SEMICOLON
%token <Lexing.position> T_COLON T_DOUBLE_COLON T_DOT 
%token <Lexing.position> T_STAR T_QUESTION 
%token <Lexing.position> T_CARROT T_BANG

%token <Lexing.position> T_RARROW 
%token <Lexing.position> T_LPAREN T_RPAREN 
%token <Lexing.position> T_LESS T_GREATER T_COMMA
%token <Lexing.position> T_LBRACKET T_RBRACKET
%token <Lexing.position> T_LBRACE T_RBRACE

%token <Lexing.position> T_SUBTYPE

%token <Lexing.position * string> T_INST_VAR
%token <Lexing.position * string> T_CONST_ID T_TYPE_ID
%token <Lexing.position * string> T_METHOD_NAME

%left T_COMMA
%right T_RARROW 
%left K_OR

%start input
%type <Annotation.t option> input

%%
input: 
  | K_DEF T_EOF    { None }
  | K_CLASS T_EOF  { None }
  | K_MODULE T_EOF { None }

  | K_DEF method_annotation_list T_EOF { Some (Annotation.MethodType $2) }
  | K_CLASS class_annotation T_EOF     { Some (Annotation.ClassType $2) }
  | K_MODULE class_annotation T_EOF    { Some (Annotation.ClassType $2) }
  | K_CAST type_expr T_EOF {Some (Annotation.ExprType $2)}

method_annotation_list: 
  | T_BEGIN_LINE method_type T_NEWLINE 
      { [$2] }
  | T_BEGIN_LINE method_type T_NEWLINE method_annotation_list
      { $2::$4 }

method_type:
  | method_name T_LESS type_id_list T_GREATER constraint_list T_COLON method_sig 
      { $1, (merge_quantifiers $3 $5 $2), $7 }
  | method_name T_COLON method_sig
      { $1, [], $3 }

method_name:
  | T_METHOD_NAME { Annotation.TIdent_Relative (snd $1) }
  | T_TYPE_ID { Annotation.TIdent_Relative (snd $1) }
  | type_ident { $1 }
  | type_ident T_DOT T_TYPE_ID { Annotation.TIdent_Scoped ($1,snd $3) }
  | type_ident T_DOT T_CONST_ID { Annotation.TIdent_Scoped ($1,snd $3) }
  | type_ident T_DOT T_METHOD_NAME { Annotation.TIdent_Scoped ($1,snd $3) }

method_sig:
  | T_LPAREN T_RPAREN block T_RARROW type_expr { Annotation.MethodSig([],$3,$5) }
  | type_expr block T_RARROW type_expr
      { match $1 with
          | Annotation.Type_Tuple lst -> Annotation.MethodSig(lst,$2,$4) 
          | t -> Annotation.MethodSig([t],$2,$4) 
      }

block:
  | { None }
  | T_LBRACE method_sig T_RBRACE {Some $2}

type_expr_comma_list:
  | type_expr { [$1] }
  | type_expr T_COMMA type_expr_comma_list {$1::$3}

declared_subtypes:
  | { [] }
  | T_SUBTYPE type_expr_comma_list { $2 }

class_decl:
  | T_CONST_ID T_LESS type_id_list T_GREATER { snd $1, $3 }
  | T_CONST_ID { snd $1, [] }

class_annotation:
  | T_BEGIN_LINE class_decl declared_subtypes constraint_list T_NEWLINE
      { let pos = $1 in
        let name,vars = $2 in
        let subs = $3 in
        let cons = $4 in
        let qlist = merge_quantifiers vars cons pos in
          (name,qlist,subs)}
  | T_BEGIN_LINE error T_NEWLINE
      { Log.fatal (Log.of_loc $1) "parse error in annotation" }

constraint_list:
  | { [] }
  | T_SEMICOLON bounded_quantifier_list { $2 }

bounded_quantifier_list:
  | bounded_quantifier {[$1]}
  | bounded_quantifier T_COMMA bounded_quantifier_list {$1::$3}

bounded_quantifier:
  | type_var T_SUBTYPE type_expr { $1, $3 }

type_id_list:
  | type_var { [$1] }
  | type_var T_COMMA type_id_list { $1::$3 }

type_var:
  | K_SELF { Annotation.QSelf }
  | T_TYPE_ID { Annotation.QVar (snd $1) }
  | T_CARROT T_TYPE_ID { Annotation.QParam (snd $2) }

type_ident:
  | T_CONST_ID { Annotation.TIdent_Relative (snd $1) }
  | T_DOUBLE_COLON T_CONST_ID { Annotation.TIdent_Absolute (snd $2) }
  | type_ident T_DOUBLE_COLON T_CONST_ID { Annotation.TIdent_Scoped($1, snd $3) }

type_expr:
  | single_type_expr { $1 }
  | single_type_expr K_OR or_type_list { Annotation.Type_Union ($1::$3)}

or_type_list:
  | single_type_expr { [$1] }
  | single_type_expr K_OR or_type_list {$1::$3}

tuple:
  | T_LPAREN type_expr_comma_list T_RPAREN 
      { match $2 with
          | [] -> assert false
          | [x] -> x
          | lst -> Annotation.Type_Tuple lst }

single_type_expr:
  | type_var { Annotation.Type_Var $1 }
  | type_ident { Annotation.Type_Ident $1 }
  | tuple { $1 }
  | T_QUESTION { Annotation.Type_Dynamic }
  | T_QUESTION single_type_expr { Annotation.Type_Optional $2 }
  | T_STAR single_type_expr { Annotation.Type_Varargs $2 }
  | T_LBRACKET field_or_method_list T_RBRACKET { Annotation.Type_Object $2 }
  | type_ident T_LESS type_expr_comma_list T_GREATER
      { Annotation.Type_App($1, $3) }
  | T_BANG T_CONST_ID { pragma (snd $2) }
  | T_CARROT T_LPAREN type_expr_comma_list T_RPAREN
      { Annotation.Type_ParamList $3 }

field_type: 
  | T_INST_VAR T_COLON type_expr { (snd $1), $3 }

field_or_method_nonempty_list:
  | field_type { [$1],[] }
  | method_type { [],[$1] }
  | field_type T_COMMA field_or_method_nonempty_list 
      { let f,m = $3 in ($1::f),m }
  | method_type T_COMMA field_or_method_nonempty_list 
      { let f,m = $3 in f, ($1::m) }

field_or_method_list:
  | { [],[] }
  | field_or_method_nonempty_list {$1}

