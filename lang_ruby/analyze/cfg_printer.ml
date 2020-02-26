
open Format
open Cfg
open Utils

module type CfgPrinter = sig
  val format_unary_op : Format.formatter -> unary_op -> unit
  val format_binary_op : Format.formatter -> binary_op -> unit
  val format_identifier : Format.formatter -> identifier -> unit
  val format_msg_id : Format.formatter -> msg_id -> unit
  val format_star_expr : Format.formatter -> star_expr -> unit
  val format_expr : Format.formatter -> expr -> unit
  val format_literal : Format.formatter -> literal -> unit
  val format_tuple_expr : Format.formatter -> tuple_expr -> unit
  val format_lhs : Format.formatter -> lhs -> unit
  val format_def_name : Format.formatter -> def_name -> unit
  val format_any_formal : Format.formatter -> any_formal -> unit
  val format_class_kind : Format.formatter -> class_kind -> unit
  val format_method_call : Format.formatter -> method_call -> unit
  val format_case : Format.formatter -> case_block -> unit
  val format_codeblock : Format.formatter -> block_formal_param list * stmt -> unit
  val format_rescue_guard : Format.formatter -> rescue_guard -> unit 
  val format_stmt : Format.formatter -> stmt -> unit
  val format_cfg : Format.formatter -> stmt -> unit

  val format_def_id : Format.formatter -> identifier -> unit
  val format_formal_tuple : Format.formatter -> any_formal list -> unit
  val format_formals : Format.formatter -> any_formal list -> unit
  val format_target_and_msg : Format.formatter -> expr option * msg_id -> unit

  val string_of_expr : expr -> string
  val string_of_cfg : stmt -> string
  val print_stmt : out_channel -> stmt -> unit
end

let str_uop = function
  | Op_UMinus   -> "-"
  | Op_UPlus    -> "+"
  | Op_UTilde   -> "~"

let rec str_binop = function
  | Op_Plus     -> "+"
  | Op_Minus    -> "-"
  | Op_Times    -> "*"
  | Op_Div      -> "/"
  | Op_Rem      -> "%"
  | Op_CMP  	-> "<=>"
  | Op_EQ  	-> "=="
  | Op_EQQ  	-> "==="
  | Op_GEQ  	-> ">="
  | Op_LEQ  	-> "<="
  | Op_LT  	-> "<"
  | Op_GT  	-> ">"
  | Op_BAnd     -> "&"
  | Op_BOr      -> "|"
  | Op_Match    -> "=~"
  | Op_XOR      -> "^"
  | Op_Pow      -> "**"
  | Op_ARef     -> "[]"
  | Op_ASet     -> "[]="
  | Op_LShift   -> "<<"
  | Op_RShift   -> ">>"

(** Since we want to share much of the code between the CodePrinter
    and the ErrorPrinter, we use a recursive functor to implement late
    (but still static) binding of all of these functions.  Thus, every
    function is invoked indirectly through the functor argument:
    PP.format_foo instead of just format_foo.  The final module is
    then constructed below, which ties the "recursive knot" using a
    recursive module.
*)
module Code_F(PP : CfgPrinter) = struct

  let format_unary_op ppf op = pp_print_string ppf (str_uop op ^ "@")

  let format_binary_op ppf op = pp_print_string ppf (str_binop op)

  let format_identifier ppf : identifier -> unit = function
    | `ID_Var(_,s) -> pp_print_string ppf s
    | `ID_Scope(i1,i2) -> fprintf ppf "%a::%s" PP.format_identifier i1 i2
    | `ID_UScope(i1) -> fprintf ppf "::%s" i1
    | `ID_Self -> pp_print_string ppf "self"
    | `ID_Nil -> pp_print_string ppf "nil"
    | `ID_True -> pp_print_string ppf "true"
    | `ID_False -> pp_print_string ppf "false"

  let format_def_id ppf : identifier -> unit = function
    | `ID_Scope(i1,i2) -> fprintf ppf "(%a::%s)" PP.format_def_id i1 i2
    | `ID_UScope(i1) -> fprintf ppf "(::%s)" i1
    | i -> PP.format_identifier ppf i

  let format_msg_id ppf : msg_id -> unit = function
    | `ID_UOperator uop -> PP.format_unary_op ppf uop
    | `ID_Operator binop -> PP.format_binary_op ppf binop
    | `ID_MethodName(s) -> pp_print_string ppf s
    | `ID_Assign(s) -> pp_print_string ppf (s ^ "=")
    | `ID_Super -> pp_print_string ppf "super"

  let format_def_name ppf = function
    | Instance_Method e -> PP.format_msg_id ppf e
    | Singleton_Method(e1, e2) -> 
        fprintf ppf "%a.%a" PP.format_def_id e1 PP.format_msg_id e2

  let format_literal ppf : literal -> unit = function
    | `Lit_FixNum i -> fprintf ppf "%d" i
    | `Lit_BigNum big -> 
        fprintf ppf "%s" (Big_int.string_of_big_int big)
    | `Lit_Float(s,f) -> fprintf ppf "%s" s
    | `Lit_String str -> fprintf ppf "%%{%s}" (escape_chars str ['{'; '}'])
    | `Lit_Atom str -> fprintf ppf ":\"%s\"" (escape_chars str ['"'])
    | `Lit_Regexp (str,m) -> 
        fprintf ppf "%%r{%s}%s" str (*(escape_chars str ['{'; '}'])*) m
    | `Lit_Array lst -> fprintf ppf "[@[%a@]]" 
        (format_comma_list PP.format_star_expr) lst
    | `Lit_Hash lst -> 
        let format_assoc ppf (l,r) = 
	  fprintf ppf "%a => %a" PP.format_expr l PP.format_expr r
        in
	  fprintf ppf "{@[%a@]}" (format_comma_list format_assoc) lst
    | `Lit_Range(true,l,u) ->
        fprintf ppf "(%a...%a)" PP.format_expr l PP.format_expr u

    | `Lit_Range(false,l,u) ->
        fprintf ppf "(%a..%a)" PP.format_expr l PP.format_expr u

  let format_expr ppf : expr -> unit = function
    | #literal as l -> PP.format_literal ppf l
    | #identifier as id -> PP.format_identifier ppf id

  let format_star_expr ppf : star_expr -> unit = function
    | #expr as e -> PP.format_expr ppf e
    | `Star e -> fprintf ppf "*%a" PP.format_expr e (* XXX *)

  let format_tuple_expr ppf : tuple_expr -> unit = function
    | `Tuple(el) -> 
        fprintf ppf "@[%a@]" (format_comma_list PP.format_tuple_expr) el
    | #expr as e -> PP.format_expr ppf e
    | `Star (`Tuple el as tup) -> fprintf ppf "*%a" PP.format_tuple_expr tup
    | `Star (#expr as e) -> fprintf ppf "*%a" PP.format_expr e

  let format_lhs ppf : lhs -> unit = function
    | #identifier as id -> PP.format_identifier ppf id
    | `Star (#identifier as i) -> fprintf ppf "*%a" PP.format_identifier i
    | `Tuple(el) -> 
        fprintf ppf "(@[%a@])" (format_comma_list PP.format_lhs) el

  let format_any_formal ppf (f:any_formal) =  match f with
    | `Formal_meth_id(s)
    | `Formal_block_id(_,s) -> pp_print_string ppf s
    | `Formal_amp s -> fprintf ppf "&%s" s
    | `Formal_star(s) -> fprintf ppf "*%s" s
    | `Formal_tuple t -> PP.format_formal_tuple ppf (t :> any_formal list)
    | `Formal_default(s,e) -> 
        fprintf ppf "@[%s = @[%a@]@]"
	  s PP.format_tuple_expr e

  let format_formal_tuple ppf (l : any_formal list) = 
    fprintf ppf "(@[";
    format_comma_list PP.format_any_formal ppf l;
    fprintf ppf "@])"

  let format_formals ppf (l: any_formal list) = 
    fprintf ppf "@[<hov>";
    format_comma_list PP.format_any_formal ppf l;
    fprintf ppf "@]"

  let format_method_arg_expr ppf = function
    | #star_expr as s -> PP.format_star_expr ppf s
    | `Proc e -> fprintf ppf "&%a" PP.format_expr e

  let format_lhs_opt ppf o = match o with
    | None -> ()
    | Some lhs -> fprintf ppf "%a = " PP.format_lhs lhs

  let format_rescues ppf = function
    | [] -> ()
    | lst -> 
        List.iter
	  (fun resc -> 
	     fprintf ppf "@[<v 2>rescue %a@,%a@]@," 
	       (format_comma_list PP.format_rescue_guard) resc.rescue_guards 
	       PP.format_stmt resc.rescue_body
	  ) lst

  let format_exn_ensure ppf s = fprintf ppf "@[<v 2>ensure@,%a@]@," PP.format_stmt s

  let format_else ppf s = fprintf ppf "@[<v 2>else@,%a@]@," PP.format_stmt s

  let format_case_whens ppf whens = 
    List.iter
      (fun (guards,body) -> 
         fprintf ppf "@[<v 2>@[<hv 2>when @[%a@] then@]@,%a@]@,"
	   PP.format_tuple_expr guards
	   PP.format_stmt body
      ) whens

  let format_case ppf c = 
    fprintf ppf "@[<v 0>case %a@,%a"
      PP.format_expr c.case_guard
      format_case_whens c.case_whens;
    format_option format_else ppf c.case_else;
    fprintf ppf "end@]"

  let format_stmt ppf stmt : unit = match stmt.snode with
    | Seq(stmt_list) -> 
        fprintf ppf  "@[<v 0>%a@]" (format_break_list PP.format_stmt) stmt_list

    | Alias(Alias_Method(m1,m2)) -> 
        fprintf ppf "alias %a %a" 
	  PP.format_msg_id m1
	  PP.format_msg_id m2

    | Alias(Alias_Global(v1,v2)) -> 
        fprintf ppf "alias %a %a" 
          PP.format_identifier (v1 :> identifier) 
          PP.format_identifier (v2 :> identifier) 

    | If(guard,then_e,else_e) ->
        fprintf ppf "@[<v 0>@[<v 2>if %a then@,%a@]@,"
	  PP.format_expr guard
	  PP.format_stmt then_e;
        format_else ppf else_e;
        fprintf ppf "end@]"

    | MethodCall(lhs_o,mc) -> 
        fprintf ppf "@[<v 0>%a%a@]" format_lhs_opt lhs_o PP.format_method_call mc
          
    | Return(e) -> fprintf ppf "return @[%a@]" (format_option PP.format_tuple_expr) e
    | Yield(lhs,e) -> ( 
        match lhs with 
          | None -> fprintf ppf "yield(@[%a@])" 
              (format_comma_list PP.format_star_expr) e
          | Some(_) ->
              fprintf ppf "%a = yield(@[%a@])"
                (format_option PP.format_lhs) lhs 
                (format_comma_list PP.format_star_expr) e
      )
    | Assign(id,e) ->
        fprintf ppf "%a = %a" PP.format_lhs id PP.format_tuple_expr e

    | Expression(e) -> 
        begin match stmt.annotation with
          | None -> PP.format_expr ppf e
          | Some annot -> 
              fprintf ppf "%a%a" Annotation.format_annotation annot 
                PP.format_expr e
        end  
  
    | Module(lhs_o,m,body) ->
        fprintf ppf "@[<v 0>%a@[<v 2>%amodule %a@,%a@]@,"
          (format_option Annotation.format_annotation) stmt.annotation
	  format_lhs_opt lhs_o
	  PP.format_identifier m
	  PP.format_stmt body;
        fprintf ppf "end@]"
          
    | Method(meth,formals,body) ->
        fprintf ppf "@[<v 0>%a@[<v 2>def %a(@[%a@])@,%a@]@,"
	  (format_option Annotation.format_annotation) stmt.annotation
	  format_def_name meth
	  format_formals (formals :> any_formal list)
	  PP.format_stmt body;
        fprintf ppf "end@]"      

    | Class(lhs_o,ck,body) ->
        fprintf ppf "@[<v 0>%a@[<v 2>%aclass %a@,%a@]@,"
	  (format_option Annotation.format_annotation) stmt.annotation
	  format_lhs_opt lhs_o
	  PP.format_class_kind ck
	  PP.format_stmt body;
        fprintf ppf "end@]"      

    | For(formals,guard,body) ->
        fprintf ppf "@[<v 0>@[<v 2>for %a in %a@,%a@]@;end@]"
	  PP.format_formals (formals :> any_formal list)
	  PP.format_expr guard
	  PP.format_stmt body

    | Begin(body) ->
        fprintf ppf "@[<v 0>@[<v 2>BEGIN {@,%a@]@,}@]"
	  PP.format_stmt body

    | End(body) ->
        fprintf ppf "@[<v 0>@[<v 2>END {@,%a@],}@]"
	  PP.format_stmt body

    | While(guard,body)  -> 
        fprintf ppf "@[<v 0>@[<v 2>while %a@,%a@]@,end@]"
	  PP.format_expr guard
	  PP.format_stmt body

    | Case(c) -> PP.format_case ppf c

    | ExnBlock(b) -> 
        fprintf ppf "@[<v 0>@[<v 2>begin@,%a@]@,"
	  PP.format_stmt b.exn_body;
        format_rescues ppf b.exn_rescue;
        format_option format_else ppf b.exn_else;
        format_option format_exn_ensure ppf b.exn_ensure;
        fprintf ppf "end@]"      

    | Defined(id, s) -> fprintf ppf "%a = defined?((@[<v>%a@]))" PP.format_identifier id PP.format_stmt s
    | Redo  -> pp_print_string ppf "redo"
    | Retry -> pp_print_string ppf "retry"

    | Break(None) -> pp_print_string ppf "break"
    | Break(Some tup) -> fprintf ppf "break @[%a@] " PP.format_tuple_expr tup

    | Next(None)  -> pp_print_string ppf "next"
    | Next(Some tup)  -> fprintf ppf "next @[%a@] " PP.format_tuple_expr tup

    | Undef es -> fprintf ppf "undef @[%a@]"
        (format_comma_list PP.format_msg_id) es


  let format_class_kind ppf = function
    | MetaClass id -> fprintf ppf "@[ << %a@]" PP.format_identifier id
    | NominalClass(id,None) -> PP.format_identifier ppf id
    | NominalClass(id,Some inh) -> 
        fprintf ppf "@[%a < %a@]" PP.format_identifier id PP.format_identifier inh

  let format_comma_delim_stmt ppf stmt = match stmt.snode with
    | Seq l -> format_comma_list PP.format_stmt ppf l
    | _ -> PP.format_stmt ppf stmt
        
  let format_rescue_guard ppf = function
    | Rescue_Expr e -> PP.format_tuple_expr ppf e
    | Rescue_Bind (e,b) -> fprintf ppf "%a => %a" PP.format_tuple_expr e PP.format_identifier b

  let format_target_and_msg ppf (targ,msg) = match targ with
    | None -> PP.format_msg_id ppf msg
    | Some e -> 
        fprintf ppf "%a.%a" PP.format_expr e PP.format_msg_id msg
          
  let format_method_call ppf mc = 
    let base ppf () = 
      fprintf ppf "%a(@[<hov>%a@]"
        PP.format_target_and_msg (mc.mc_target,mc.mc_msg)
        (format_comma_list PP.format_star_expr) mc.mc_args
    in
      match mc.mc_cb with
        | None -> fprintf ppf "%a)" base ()
        | Some (CB_Arg e) -> 
            if mc.mc_args == []
            then fprintf ppf "%a%a)" base () format_expr e
            else fprintf ppf "%a,%a)" base () format_expr e
        | Some (CB_Block(formals,body)) -> 
	    fprintf ppf "%a) {%a@,}"
	      base () PP.format_codeblock (formals,body)

  let format_codeblock ppf (formals,body) =  
    match formals with
      | [] -> fprintf ppf "||@,  @[<v 0>%a@]" PP.format_stmt body
      | lst-> fprintf ppf "|%a|@,  @[<v 0>%a@]" 
	  PP.format_formals (formals :> any_formal list)
	    PP.format_stmt body

  let format_cfg ppf cfg = fprintf ppf "@[<v 0>%a@]\n" PP.format_stmt cfg

  let string_of_expr e = format_to_string PP.format_expr e

  let string_of_cfg cfg = format_to_string PP.format_cfg cfg

  let print_stmt oc s = 
    let ppf = Format.formatter_of_out_channel oc in
      format_cfg ppf s;
      pp_print_flush ppf ()

end

(** Close the module *)
module rec CodePrinter : CfgPrinter = Code_F(CodePrinter)

(** This module "inherits" from the CodePrinter and makes use of late
    binding through the functor argument to simply redefine the few
    cases that are changed.  Thus, most of the implementation is
    reused from CodePrinter. *)
module Err_F(PP : CfgPrinter) = struct

  module Super = Code_F(PP)
  include Super

  let format_identifier ppf : identifier -> unit = function
    | `ID_Var(_,s) -> 
        (* print _ instead of RIL temporaries *)
        if is_tmp_var s then pp_print_string ppf "_"
        else pp_print_string ppf s
    | i -> Super.format_identifier ppf i

  let format_stmt ppf stmt : unit = match stmt.snode with
    | MethodCall(lhs_o, {mc_msg=`ID_MethodName "safe_require";mc_args=[s1;_]}) ->
        fprintf ppf "%arequire(%a)"
          format_lhs_opt lhs_o PP.format_star_expr s1

    | Return(Some tup) -> 
        let s = format_to_string Super.format_tuple_expr tup in
          (* Format [return __tmp_X] as [implicit return].  Note this
             doesn't necessarily mean the return statement was not in
             the original syntax, since the argument could have been
             hoisted.  We should probably add an explicit field to
             this type to denote when returns are implicit. *)
          if is_tmp_var s 
          then fprintf ppf "%s" "implicit return"  
          else Super.format_stmt ppf stmt

   | Yield(Some(`ID_Var(_,s)),e) ->
       (* remove any implicit assign inserted by RIL.  Such as [__tmp = yield()] *)
       if is_tmp_var s 
       then fprintf ppf "yield(@[%a@])" (format_comma_list PP.format_star_expr) e
       else  Super.format_stmt ppf stmt

    | _ -> Super.format_stmt ppf stmt
            
 let format_target_and_msg ppf (targ,msg) = match targ with
   | Some e -> 
       (* for nested method calls, we omit the hoisted receiver.  That
       is, [__tmp_X.foo()] is formatted simply as [foo()] *)
      let s = format_to_string Super.format_expr e in
        if is_tmp_var s then fprintf ppf "%a" PP.format_msg_id msg
        else Super.format_target_and_msg ppf (targ,msg)

   | _ -> Super.format_target_and_msg ppf (targ,msg)

 let format_codeblock ppf ((formals,body) as cb) = 
   match formals with
       (* omit the empty block parameter list || *)
     | [] -> fprintf ppf "%a" format_stmt body
     | lst-> Super.format_codeblock ppf cb

end

module rec ErrorPrinter : CfgPrinter = Err_F(ErrorPrinter)

module type UnParser = sig
  type ('a,'b) fmt

  val s : string -> ('a, 'a) fmt

  val unary_op : ('a, unary_op -> 'a) fmt
  val binary_op : ('a, binary_op -> 'a) fmt
  val identifier : ('a, identifier -> 'a) fmt
  val msg_id : ('a, msg_id -> 'a) fmt
  val star_expr : ('a, star_expr -> 'a) fmt
  val expr : ('a, expr -> 'a) fmt
  val literal : ('a, literal -> 'a) fmt
  val tuple_expr : ('a, tuple_expr -> 'a) fmt
  val lhs : ('a, lhs -> 'a) fmt
  val def_name : ('a, def_name -> 'a) fmt
  val any_formal : ('a, any_formal -> 'a) fmt
  val class_kind : ('a, class_kind -> 'a) fmt
  val method_call : ('a, method_call -> 'a) fmt
  val case : ('a, case_block -> 'a) fmt
  val codeblock : ('a, block_formal_param list * stmt -> 'a) fmt
  val rescue_guard : ('a, rescue_guard -> 'a) fmt 
  val stmt : ('a, stmt -> 'a) fmt

  val comma_list : (unit,'elt -> unit) fmt -> ('a, 'elt list -> 'a) fmt
  val option : (unit,'elt -> unit) fmt -> ('a, 'elt option -> 'a) fmt
  val string : ('a, string -> 'a) fmt

  val of_fmt : (Format.formatter -> 'b -> unit) -> ('a, 'b -> 'a) fmt
  val to_fmt : (unit, 'b -> unit) fmt -> (Format.formatter -> 'b -> unit)

  val format : Format.formatter -> (unit,'a) fmt -> 'a
  val sprintf : (string,'a) fmt -> 'a

  val kformat : (Format.formatter -> 'a) -> Format.formatter -> ('a,'b) fmt -> 'b
  val ksformat : (string -> 'a) -> ('a,'b) fmt -> 'b

  val (++) : ('a, 'b) fmt -> ('c, 'a) fmt -> ('c, 'b) fmt
end

module UnParse_F(P : CfgPrinter) : UnParser = struct
  type ('a,'b) fmt = (Format.formatter -> 'a) -> Format.formatter -> 'b

  let s str k ppf = k (Format.pp_print_string ppf str;ppf)


  let unary_op k ppf op = k (P.format_unary_op ppf op;ppf) 
  let binary_op k ppf op = k (P.format_binary_op ppf op;ppf)
  let identifier k ppf x = k (P.format_identifier ppf x;ppf)
  let msg_id k ppf x = k (P.format_msg_id ppf x;ppf)
  let star_expr k ppf x = k (P.format_star_expr ppf x;ppf)
  let expr k ppf x = k (P.format_expr ppf x;ppf)
  let literal k ppf x = k (P.format_literal ppf x;ppf)
  let tuple_expr k ppf x = k (P.format_tuple_expr ppf x;ppf)
  let lhs k ppf x = k (P.format_lhs ppf x;ppf)
  let def_name k ppf x = k (P.format_def_name ppf x;ppf)
  let any_formal k ppf x = k (P.format_any_formal ppf x;ppf)
  let class_kind k ppf x = k (P.format_class_kind ppf x;ppf)
  let method_call k ppf x = k (P.format_method_call ppf x;ppf)
  let case k ppf x = k (P.format_case ppf x;ppf)
  let codeblock k ppf x = k (P.format_codeblock ppf x;ppf)
  let rescue_guard k ppf x = k (P.format_rescue_guard ppf x;ppf)
  let stmt k ppf x =k (P.format_stmt ppf x;ppf)

  let to_fmt f = f ignore
  let of_fmt f k ppf x = k (f ppf x;ppf)

  let comma_list (f:(unit,'b->unit) fmt) k ppf lst =
    let fmt = to_fmt f in
      k (Format.fprintf ppf "@[<hov>%a@]" (format_comma_list fmt) lst;ppf)

  let option f k ppf opt = 
    k (format_option (to_fmt f) ppf opt;ppf)

  let string k ppf x = k (Format.pp_print_string ppf x;ppf)

  let kformat cont ppf k = k cont ppf
  let format ppf k = k ignore ppf

  let ksformat cont k = 
    let buf = Buffer.create 128 in
    let ppf = Format.formatter_of_buffer buf in
      k (fun ppf -> 
           Format.pp_print_flush ppf ();
           cont (Buffer.contents buf)
        ) ppf

  let sprintf k = ksformat Utils.id k

  let (++) k i1 i2 = k (i1 i2)    
end

module CodeUnparser = UnParse_F(CodePrinter)
module ErrorUnparser = UnParse_F(ErrorPrinter)
