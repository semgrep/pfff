
open Format
open Utils

type t = 
  | ClassType of class_annotation
  | MethodType of method_annotation list
  | ExprType of type_expr

and class_annotation = string * bounded_quantifier list * type_expr list

and method_annotation = type_ident * bounded_quantifier list * method_type

(* we need an explicit constructor here since the type is otherwise cyclic *)
and method_type = MethodSig of type_expr list * method_type option * type_expr

and type_expr = 
  | Type_Var of quant_var
  | Type_Ident of type_ident
  | Type_Object of object_type
  | Type_Union of type_expr list
  | Type_App of type_ident * type_expr list
  | Type_Tuple of type_expr list
  | Type_Dynamic
  | Type_Fixme
  | Type_Optional of type_expr
  | Type_Varargs of type_expr
  | Type_ParamList of type_expr list

and type_ident = 
  | TIdent_Relative of string
  | TIdent_Absolute of string
  | TIdent_Scoped of type_ident * string

and object_type = field_type list * method_annotation list

and field_type = string * type_expr 

and bounded_quantifier = quant_var * type_expr option

and quant_var = 
  | QSelf
  | QVar of string
  | QParam of string

let format_quant_var ppf = function
  | QSelf -> pp_print_string ppf "self"
  | QVar s -> pp_print_string ppf s
  | QParam s -> fprintf ppf "^%s" s

let rec format_type_ident ppf = function
  | TIdent_Relative s -> pp_print_string ppf s
  | TIdent_Absolute s -> fprintf ppf "::%s" s
  | TIdent_Scoped(ti,s) -> fprintf ppf "%a::%s" format_type_ident ti s

let rec format_type_expr ppf = function
  | Type_Var qv -> format_quant_var ppf qv
  | Type_Ident ti -> format_type_ident ppf ti
  | Type_Object (fields,meths) -> 
      fprintf ppf "[%a,@ %a]"
        (format_comma_list format_field_type) fields
        (format_comma_list format_method_annotation) meths
  | Type_Union lst -> format_delim_list " or " format_type_expr ppf lst
  | Type_App(id,args) ->
      fprintf ppf "%a<%a>" format_type_ident id 
        (format_comma_list format_type_expr) args
  | Type_Tuple te_list ->  
      fprintf ppf "(%a)" (format_comma_list format_type_expr) te_list

  | Type_Dynamic -> pp_print_string ppf "?"
  | Type_Fixme -> pp_print_string ppf "!FIXME"
  | Type_Optional te -> fprintf ppf "?%a" format_type_expr te
  | Type_Varargs te -> fprintf ppf "*%a" format_type_expr te
  | Type_ParamList tlst ->
      fprintf ppf "^(%a)" (format_comma_list format_type_expr) tlst

and format_field_type ppf (f,t) = fprintf ppf "%s: %a" f format_type_expr t

and format_method_type ppf (MethodSig(params,blk,ret)) = match params with
  | [x] -> 
      fprintf ppf "%a %a -> %a" format_type_expr x 
        format_type_block blk format_type_expr ret
  | lst -> 
      fprintf ppf "(%a) %a -> %a" (format_comma_list format_type_expr) lst
        format_type_block blk format_type_expr ret

and format_type_block ppf = function
  | None -> ()
  | Some mt -> fprintf ppf "{%a}" format_method_type mt

and format_method_annotation ppf = function
  | (name,[],mt) -> fprintf ppf "%a: %a" format_type_ident name format_method_type mt
  | (name,lst,mt) -> 
      fprintf ppf "%a<%a> : %a" format_type_ident name
        (format_comma_list format_bounded_quantifier) lst format_method_type mt

and format_bounded_quantifier ppf = function
  | qv, None -> format_quant_var ppf qv
  | qv, Some te -> fprintf ppf "%a < %a" format_quant_var qv format_type_expr te

let format_declared_subtypes ppf lst = 
  (format_comma_list (fun ppf x -> fprintf ppf "<= %a" format_type_expr x)) ppf lst

let format_class_annotation ppf : class_annotation -> unit = function
  | s,[],subs -> fprintf ppf "%s %a" s format_declared_subtypes subs
  | s,lst,subs -> 
      fprintf ppf "%s<%a> %a" s (format_comma_list format_bounded_quantifier) lst
        format_declared_subtypes subs

let format_annotation ppf = function
  | ClassType c -> fprintf ppf "@[<v 0>##%% @[<h>%a@]@,@]" format_class_annotation c
  | MethodType mlst -> 
      fprintf ppf "@[<v 0>";
      List.iter (fprintf ppf "##%% @[<h>%a@]@," format_method_annotation) mlst;
      fprintf ppf "@]"
      
  | ExprType e -> fprintf ppf "@[<v 0>###%% @[<h>(%a)@]@]" format_type_expr e


let string_of_quant_var = function
  | QSelf -> "self"
  | QParam s | QVar s -> s

let string_of_annotation annot = 
  format_to_string format_annotation annot

let compare_class_annot c1 c2 = compare c1 c2
let compare_method_annot m1 m2 = compare m1 m2
let compare_expr_annot e1 e2 = compare e1 e2

let compare_annotation a1 a2 = match a1,a2 with
  | ClassType c1, ClassType c2 -> compare_class_annot c1 c2
  | MethodType m1, MethodType m2  -> compare_method_annot m1 m2
  | ExprType e1, ExprType e2 -> compare_expr_annot e1 e2

  | (ClassType _|MethodType _|ExprType _), (ClassType _|MethodType _|ExprType _) ->
      cmp_ctors a1 a2

let equal_annotation a1 a2 = (compare_annotation a1 a2) == 0

open Visitor
(*
let rec visit_annot_expr vtor e = 
  visit vtor#visit_annot_expr e begin function
    | Annot_Id id -> 
        let id' = visit_id vtor id in
          if id == id' then e else Annot_Id id'
    | Annot_Union lst -> 
        let lst' = map_preserve List.map (visit_annot_expr vtor) lst in
          if lst == lst' then e else Annot_Union lst'
    | Annot_Inst(id,lst) ->
        let id' = visit_id vtor id in
        let lst' = map_preserve List.map (visit_annot_expr vtor) lst in
          if lst == lst' && id == id' then e else Annot_Inst(id',lst')
    | Annot_Object o -> 
        let meths' = map_preserve List.map (visit_annot_method vtor) o.annot_obj_methods in
        let fields' = map_preserve List.map (visit_annot_field vtor) o.annot_obj_fields in
          if o.annot_obj_methods == meths' &&
            o.annot_obj_fields == fields' 
          then e else Annot_Object {annot_obj_methods=meths';annot_obj_fields=fields'}
    | Annot_ParamList params ->
        let params' =  map_preserve List.map (visit_annot_method_param vtor) params in
          if params == params' then e else Annot_ParamList params'
    | Annot_Dynamic | Annot_Fixme -> e
  end

and visit_annot_field vtor ((s,te) as field) = 
  let te' = visit_annot_expr vtor te in
    if te == te' then field else (s,te')

and visit_annot_method_param vtor e = 
  visit vtor#visit_annot_method_param e begin function
    | Annot_Param(ae) ->
        let ae' = visit_annot_expr vtor ae in
          if ae == ae' then e else Annot_Param ae'

    | Annot_Vararg(ae) ->
        let ae' = visit_annot_expr vtor ae in
          if ae == ae' then e else Annot_Vararg ae'

    | Annot_Optarg(ae) ->
        let ae' = visit_annot_expr vtor ae in
          if ae == ae' then e else Annot_Optarg ae'

    | Annot_Polyadic s -> e
  end

and visit_annot_func vtor e = 
  visit vtor#visit_annot_func e begin fun f ->
    let args' = map_preserve List.map (visit_annot_method_param vtor) f.annot_func_args in
    let ret' = visit_annot_expr vtor f.annot_func_ret in
      if f.annot_func_args == args' && f.annot_func_ret == ret' 
      then f
      else {annot_func_args = args'; annot_func_ret = ret'}
  end

and visit_annot_method vtor e = 
  visit vtor#visit_annot_method e begin fun f -> 
    let dn' = visit_def_name vtor f.annot_meth_name in
    let typ' = visit_annot_func vtor f.annot_meth_typ in
    let blk' = map_opt (visit_annot_func vtor) f.annot_meth_block in
      if f.annot_meth_name == dn' && f.annot_meth_typ == typ' &&
        f.annot_meth_block == blk'
      then f
      else {f with annot_meth_name = dn'; annot_meth_typ = typ';
            annot_meth_block = blk';}
  end

let visit_annot_class vtor e = 
  visit vtor#visit_annot_class e begin fun c ->
    let incs' = map_preserve List.map
      (fun ((id,aexpr_l) as pair) ->
         let id' = visit_id vtor id in
         let aexpr_l' = map_preserve List.map (visit_annot_expr vtor) aexpr_l in
           if id == id' && aexpr_l == aexpr_l' then pair else (id',aexpr_l')
      ) c.annot_cls_incs
    in
      if c.annot_cls_incs == incs' then c
      else {c with annot_cls_incs = incs'}
  end
*)
