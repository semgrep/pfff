open Xml

let debug = ref true

let ml_header = Buffer.create 99
let add_ml_header s = Buffer.add_string ml_header s
let () = add_ml_header "type -'a obj\n"
let c_header= Buffer.create 99
let add_c_header s = Buffer.add_string c_header s
let () = add_c_header
  "#include <gtk/gtk.h>
#define GTK_TEXT_USE_INTERNAL_UNSUPPORTED_API
#include <gtk/gtktextdisplay.h>
#include <gdk/gdkprivate.h>
#include <glib/gstdio.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include \"../lablgtk/src/wrappers.h\"
#include \"../lablgtk/src/ml_gobject.h\"
#define Val_double(val) caml_copy_double(val)
#define Val_string_new(val) caml_copy_string(val)
#define Val_int32(val) caml_copy_int32(val)
#define Val_int32_new(val) caml_copy_int32(val)
"

module SSet=
  Set.Make(struct type t = string let compare = Pervasives.compare end)

let caml_avoid = List.fold_right SSet.add ["true";"false";"exit"] SSet.empty
let is_caml_avoid s = SSet.mem s caml_avoid

type c_simple_stub = {cs_nb_arg:int;
		      cs_c_name:string;
		      cs_ret:string;
		      cs_params:string list}
type c_stub = Simple of c_simple_stub | Complex of string 
type ml_stub =  { ms_c_name:string;
		  ms_ml_name:string;
		  ms_ret:string;
		  ms_params:string list;
		}
type stub = {
  stub_c:c_stub;
  stub_external:ml_stub;
}

type transfer_ownership = ODefault | OFull | ONone | OContainer
type direction = DDefault | DIn | DOut | DInOut
type c_typ = {
  mutable t_name:string;
  mutable t_c_typ:string;
  mutable t_content: c_typ option
}
type c_array = {
  mutable a_c_typ:string;
  mutable a_fixed_size:string;
  mutable a_zero_terminated: string;
  mutable a_length: string; (* rank of arg containing length?*)
  mutable a_typ: c_typ
}
type typ = NoType | Array of c_array | Typ of c_typ
type parameter = {
  mutable p_name:string;
  mutable p_ownership: transfer_ownership;
  mutable p_typ: typ;
  mutable p_scope:string;
  mutable p_closure:string;
  mutable p_destroy:string;
  mutable p_direction:direction;
  mutable p_allow_none:bool;
  mutable p_doc:string;
  mutable p_varargs:bool;
}
type return_value = {
  mutable r_ownership: transfer_ownership;
  mutable r_typ : typ;
  mutable r_doc : string; 
}
type function_ = {
  mutable f_name:string;
  mutable c_identifier:string;
  mutable version:string;
  mutable doc:string;
  mutable return_value: return_value;
  mutable parameters: parameter list;
  mutable deprecated: string;
  mutable deprecated_version:string;
  mutable throws: bool;
}

type constructor = function_
type c_method = function_
type property = {
  mutable pr_name:string;
  mutable pr_version:string;
  mutable pr_writable:bool;
  mutable pr_readable:bool;
  mutable pr_construct:bool;
  mutable pr_construct_only:bool;
  mutable pr_doc:string;
  mutable pr_typ:typ;}

type signal

type klass = {mutable c_name:string;
	      mutable c_c_type:string;
	      mutable c_abstract:bool;
	      mutable c_doc:string;
	      mutable c_parent:string;
	      mutable c_glib_type_name:string;
	      mutable c_glib_get_type:string;
	      mutable c_glib_type_struct:string;
	      mutable c_constructors: constructor list;
	      mutable c_methods: c_method list;
	      mutable c_functions: function_ list;
	      mutable c_properties : property list;
	      mutable c_glib_signals : signal list;
	     }

type namespace = {
  mutable ns_name: string;
  mutable ns_c_prefix: string;
  mutable ns_version: string;
  mutable ns_klass:
    (string * stub option list * stub option list) list;
  mutable ns_functions: stub option list;
  mutable ns_shared_library: string;
}

module Pretty = struct

  let rec pp_list sep fmt l = match l with
    | [] -> ()
    | e::r -> Format.fprintf fmt "%s%s" e sep;
	pp_list sep fmt r
	  
  let c_stub fmt s = match s with 
    | Simple s -> 
	Format.fprintf fmt "ML_%d(%s,%a%s)@\n" s.cs_nb_arg s.cs_c_name
	  (pp_list ", ") s.cs_params
	  s.cs_ret;
	if s.cs_nb_arg>=6 then 
	  Format.fprintf fmt "ML_bc%d(ml_%s)@ " s.cs_nb_arg s.cs_c_name
    | Complex s -> Format.fprintf fmt "%s@." s
	
  let ml_stub fmt s = 
    Format.fprintf fmt "external %s: %a%s = \"%s\"@ "
      s.ms_ml_name
      (pp_list " -> ") s.ms_params 
      s.ms_ret
      s.ms_c_name

  let stub fmt s = 
    Format.fprintf fmt "=====@.%a%a@." c_stub s.stub_c ml_stub s.stub_external

  let may_ml_stub fmt s = match s with 
    | Some s -> ml_stub fmt s.stub_external 
    | None -> ()

  let may_c_stub fmt s = match s with 
    | Some s -> c_stub fmt s.stub_c 
    | None -> ()
	  
  let klass ~ml ~c (k_name,methods,functions) = 
    Format.fprintf ml "@[<hv 2>module %s = struct@\n"
      k_name;
    List.iter (may_ml_stub ml) methods;
    List.iter (may_ml_stub ml) functions;
    Format.fprintf ml "@]end@\n";

    Format.fprintf c "@[/* Module %s */@\n" k_name;
    List.iter (may_c_stub c) methods;
    List.iter (may_c_stub c) functions;
    Format.fprintf c "@]/* end of %s */@\n" k_name
      
  let functions ~ml ~c f = 
    Format.fprintf ml "@[(* Global functions *)@\n";
    List.iter (may_ml_stub ml) f;
    Format.fprintf ml "(* End of global functions *)@]@\n";

    Format.fprintf c "@[/* Global functions */@\n";
    List.iter (may_c_stub c) f;
    Format.fprintf c "/* End of global functions */@]@\n"

  let namespace n = 
    let stub_ml_channel = open_out ("stubs_"^n.ns_name^".ml") in
    let ml = Format.formatter_of_out_channel stub_ml_channel in
    let stub_c_channel = open_out ("ml_stubs_"^n.ns_name^".c") in
    let c = Format.formatter_of_out_channel stub_c_channel in
    Format.fprintf ml "%s@\n" (Buffer.contents ml_header);
    Format.fprintf c "%s@\n" (Buffer.contents c_header);
    List.iter (klass ~ml ~c) n.ns_klass;
    functions ~ml ~c n.ns_functions;
    Format.fprintf ml "@.";
    Format.fprintf c "@.";
    close_out stub_ml_channel;
    close_out stub_c_channel


end
		
let dummy_ownership () = ODefault
let dummy_typ () = NoType
let dummy_c_typ () = {t_name="";t_c_typ="";t_content=None}
let dummy_array () = 
  {a_c_typ="";
   a_fixed_size="";
   a_zero_terminated="";
   a_length="";
   a_typ= dummy_c_typ();
}

let dummy_parameter () = 
  {p_name="";p_ownership=dummy_ownership ();
   p_typ=dummy_typ ();
   p_scope="";p_closure="";p_destroy="";p_direction=DDefault;
   p_allow_none=false;p_doc="";p_varargs=false;}
let dummy_property () = 
  {pr_name="";pr_version="";
   pr_typ=dummy_typ ();
   pr_writable=false;
   pr_readable=true;
   pr_construct=false;
   pr_construct_only=false;
   pr_doc="";}

let dummy_return_value () = {r_ownership=dummy_ownership ();
			     r_typ=dummy_typ();
			     r_doc=""}
let dummy_function () = { f_name="";c_identifier="";version="";doc="";
		     return_value=dummy_return_value ();
		     parameters= [];deprecated="";
		     deprecated_version="";throws=false;}


let dummy_klass () = { 
  c_name="";
  c_c_type = "";
  c_abstract = false;
  c_doc = "";
  c_parent="";
  c_glib_type_name="";
  c_glib_get_type="";
  c_glib_type_struct="";
  c_constructors=[];
  c_methods=[];
  c_functions=[];
  c_properties=[];
  c_glib_signals=[];
}

let protos_xml = ref []
let debug_ownership fmt o = 
  if !debug then 
    Format.fprintf fmt "%s" (match o with 
			       | ODefault -> "default"
			       | OFull -> "full"
			       | ONone -> "none"
			       | OContainer -> "container")
let debug_array fmt a = 
  if !debug then 
    Format.fprintf fmt "ARRAY"

let debug_ctyp fmt t = 
  if !debug then 
  Format.fprintf fmt "@[tname:%s@]@ @[t_c_typ:%s@]" t.t_name t.t_c_typ

let debug_typ fmt t = 
  if !debug then 
  match t with 
    | NoType -> Format.fprintf fmt "NOTYPE"
    | Array a -> debug_array fmt a
    | Typ t -> debug_ctyp fmt t 

let debug_parameter fmt p = 
  if !debug then 
    Format.fprintf fmt "(%a%s%s) @[%a@]@ " 
    debug_ownership p.p_ownership
    (match p.p_direction with 
       | DDefault -> ""
       | DIn -> ",in"
       | DOut -> ",out"
       | DInOut -> ",inout")
    (if p.p_allow_none then ",?" else "")
    debug_typ p.p_typ

let debug_parameters fmt l = 
  if !debug then 
    let c = ref 0 in
    List.iter (fun p -> 
		 Format.fprintf fmt "param %d:@[%a@]@\n" !c debug_parameter p;
		 incr c;)
      l

let debug_return_value fmt r = 
  if !debug then 
    Format.fprintf fmt "(%a) %a" 
      debug_ownership r.r_ownership
      debug_typ r.r_typ
    
let debug_function fmt l = 
  if !debug then 
    Format.fprintf fmt 
      "@[Name='%s'@ C:'%s'@ Version:'%s'@ Deprecated since '%s'(%s)@\n\
      Returns: @[%a@]@\n\
      Args:@[%a@]@]" 
      l.f_name l.c_identifier l.version l.deprecated_version l.deprecated
      debug_return_value l.return_value
      debug_parameters l.parameters
      
let debug_property fmt p = 
  if !debug then 
    Format.fprintf fmt 
      "@[Name='%s'@ Version:'%s'@ Writable:%b Readable:%b@\n\
      Type: @[%a@]@]" 
      p.pr_name  p.pr_version 
      p.pr_writable p.pr_readable
      debug_typ p.pr_typ


let debug_klass fmt k = 
  if !debug then begin
    Format.fprintf fmt "@[<hov 1>Class: '%s'@ Parent:'%s'@ Abstract:%b@ \
                      C type:'%s'@ get_type:'%s'@\n" 
      k.c_name
      k.c_parent
      k.c_abstract
      k.c_glib_type_name
      k.c_glib_get_type
    ;
    List.iter (fun f -> Format.fprintf fmt "Constructor %a@\n" debug_function f)
      k.c_constructors;
    List.iter (fun f -> Format.fprintf fmt "Function %a@\n" debug_function f)
      k.c_functions;
    List.iter (fun f -> Format.fprintf fmt "Method %a@\n" debug_function f)
      k.c_methods;
    List.iter (fun f -> Format.fprintf fmt "Property %a@\n" debug_property f)
      k.c_properties;
    Format.fprintf fmt "@]" 
  end

exception Cannot_emit of string
let fail_emit s = raise (Cannot_emit s)

module Translations = struct
let tbl = Hashtbl.create 17
let base_translation  =
  [
    ["gboolean"],("Val_bool","Bool_val",fun _ ->"bool");
   
    [ "int"; "gint";"guint";
      "guint16";"gint16"; "gushort";
      "char";"gchar";"guchar";
      "gssize";"gsize"],
    ("Val_int","Int_val",fun _ -> "int");
   
   [ "gint32";"guint32";"gunichar"],
    ("Val_int32","Int32_val",fun _ -> "int32");
   
   [ "gint64";"guint64";],
    ("Val_int64","Int64_val",fun _ -> "int64");
   
   [ "void" ] , ("Unit","void_param????",fun _ -> "unit");
   
   ["gdouble"; "long double"; "glong"; "gulong" ; "double"] , 
    ("Val_double","Double_val",fun _ -> "float");
   
   [ "gchar*";"char*"; "guchar*" ], 
    ("Val_string","String_val",fun _ -> "string");
  ]
let () = 
  List.iter 
    (fun (kl,v) -> List.iter (fun k -> Hashtbl.add tbl k v) kl) 
    base_translation

let find k = try Hashtbl.find tbl k with Not_found -> 
  fail_emit ("Cannot translate type " ^k)

let to_type_macro s = 
  match s with 
    | "GtkCList" -> "GTK_CLIST"
    | "GtkCTree" -> "GTK_CTREE"
    | "GtkHSV" -> "GTK_HSV"
    | "GtkIMContext" -> "GTK_IM_CONTEXT"
    | "GtkIMMulticontext" -> "GTK_IM_MULTICONTEXT"
    | "GtkUIManager" -> "GTK_UI_MANAGER"
    | "GdkDisplay" -> "GDK_DISPLAY_OBJECT"
    | "GdkGC" -> "GDK_GC"
    | _ -> 
	let length = String.length s in
	if length <= 1 then String.uppercase s
	else
	  let buff = Buffer.create (length+3) in
	  Buffer.add_char buff s.[0];
	  for i=1 to length-1 do
	    if 'A'<=s.[i] && s.[i]<='Z' then begin
	      Buffer.add_char buff '_';
	      Buffer.add_char buff s.[i]
	    end else Buffer.add_char buff (Char.uppercase s.[i])
	  done;
	  Buffer.contents buff

let add_klass c_typ = 
  let val_of = "Val_"^c_typ in
  let of_val = c_typ^"_val" in
  add_c_header 
    (Format.sprintf "#define %s(val) check_cast(%s,val)@." 
       of_val 
       (to_type_macro c_typ));
  add_c_header 
    (Format.sprintf "#define %s(val) Val_GObject((GObject*)val)@." 
     val_of);
  add_c_header 
    (Format.sprintf "#define %s_new(val) Val_GObject_new((GObject*)val)@." 
     val_of);
  Hashtbl.add tbl (c_typ^"*") 
    (val_of,of_val,
     fun variance ->
       Format.sprintf "[%c`%s] obj" variance (String.lowercase c_typ))
end    

let emit_parameter rank p = match p.p_typ with
  | NoType -> fail_emit "NoType(p)"
  | Array _ -> fail_emit "Array(p)"
  | Typ ct -> 
      match p.p_direction with 
	| DIn | DDefault ->
	    let ctyp = match p.p_ownership with 
	      | OFull -> ct.t_c_typ ^"*"
	      | _ -> ct.t_c_typ
	    in
	    let _,c_base,ml_base = Translations.find ctyp in
	    let ml_base = ml_base '>' in
	    if p.p_allow_none then 
	      Format.sprintf "Option_val(arg%d,%s,NULL) Ignore" rank c_base,
	    ml_base^" option"
	    else c_base,ml_base
	| DOut | DInOut -> fail_emit "(out)" 

let emit_parameters throws l = 
  let l = if throws then 
    let gerror = dummy_parameter()in
    let t = dummy_c_typ () in
    t.t_c_typ<-"GError**";
    gerror.p_name<-"error";
    gerror.p_typ<-Typ t;
    l@[gerror]
  else l
  in
  let counter = ref 0 in
  List.split (List.map (fun p -> incr counter;emit_parameter !counter p) l)

let emit_return_value r = 
  match r.r_typ with
    | NoType -> fail_emit "NoType"
    | Array _ -> fail_emit "Array"
    | Typ ct -> 
	let c_typ,_,ml_typ = Translations.find ct.t_c_typ in
	match r.r_ownership with
	  | OFull -> c_typ ^ "_new",ml_typ
	  | ONone | ODefault | OContainer -> c_typ,ml_typ

let emit_prototype f = 
  let nb_args = List.length f.parameters in
  let c_params,ml_params = emit_parameters f.throws f.parameters in
  let c_ret,ml_ret = emit_return_value f.return_value in
  let ml_ret = ml_ret '<' in
  {stub_c=Simple {cs_nb_arg=nb_args;
		  cs_c_name=f.c_identifier;
		  cs_ret=c_ret;
		  cs_params=c_params};
   stub_external={ms_c_name="ml_"^f.c_identifier;
		  ms_ml_name=f.f_name;
			   ms_ret=ml_ret;
			   ms_params= match ml_params with 
			     | [] -> ["unit"]
			     | _ -> ml_params;}}
    
let emit_method k m = 
  try 
    let self = dummy_parameter()in
    let t = dummy_c_typ () in
    t.t_c_typ<-k.c_c_type^"*";
    self.p_name<-"self";
    self.p_typ<-Typ t;
    let nb_args = List.length m.parameters+1 in
    let c_params,ml_params = emit_parameters m.throws (self::m.parameters) in
    let c_ret,ml_ret = emit_return_value m.return_value in
    let ml_ret = ml_ret '<' in
    Some {stub_c=Simple {cs_nb_arg=nb_args;
		    cs_c_name=m.c_identifier;
		    cs_ret=c_ret;
		    cs_params=c_params};
     stub_external={ms_c_name="ml_"^m.c_identifier;
		    ms_ml_name=m.f_name;
		    ms_ret=ml_ret;
		    ms_params=ml_params;}}
  with Cannot_emit s ->  
    Format.printf "Cannot emit method %s::%S '%s'@." 
      k.c_name
      m.f_name
      s;
    None

let emit_function p = 
  try Some (emit_prototype p)
  with Cannot_emit s -> Format.printf "Cannot emit function %s:'%s'@." 
    p.c_identifier s; None


let register_function p = 
  protos_xml := p::!protos_xml

let klass = ref []
let register_klass k = 
  if !debug then Format.printf "@[<hov 1>%a@]@." debug_klass k;
  Translations.add_klass k.c_c_type;
  klass:= k::!klass
let all_klass () = List.rev (!klass)

let emit_klass k = 
  let methods = List.map (emit_method k) k.c_methods in
  let functions = List.map  emit_function k.c_functions in
  k.c_name,methods,functions

let all_functions () = List.rev (!protos_xml)

let emit_all () = 
  let global_functions = List.map emit_function (all_functions()) in
  let klass =  List.map emit_klass (all_klass()) in
  global_functions,klass

let rec parse_c_typ set attrs children = 
  let typ = dummy_c_typ () in
  List.iter (fun (key,v) -> match key with 
	       | "name" -> typ.t_name <- v
	       | "c:type" -> typ.t_c_typ <- v
	       | other -> 
		   Format.printf "Ignoring attribute in typ:%s@." other)
    attrs;
  List.iter (function 
	       | PCData s -> 
		   Format.printf "Ignoring PCData in type:%s@." s
	       | Element (key,attrs,children) -> 
		 match key with 
		   | "type" -> 
		       parse_c_typ 
			 (fun t ->
			    typ.t_content <- Some t)
			 attrs children
		   | other -> 
		       Format.printf "Ignoring child in typ:%s@." other)
    children;
  set typ
  
let parse_ownership set s = 
  match s with "full" -> set OFull
    | "none" -> set ONone
    | "container" -> set OContainer
    | s -> Format.printf "Ignoring ownership transfer '%s'@." s


let parse_array set attrs children = 
  let r = dummy_array () in
  List.iter (fun (key,v) -> match key with 
	       | "c:type" -> r.a_c_typ <- v
	       | "length" -> r.a_length <- v
	       | other -> 
		   Format.printf "Ignoring attribute in array: %s@." other) 
    attrs;
  List.iter (function 
	       | PCData s -> 
		   Format.printf "Ignoring PCData in array:%s@." s
	       | Element (key,attrs,children) -> 
		   match key with 
		     | "type" -> parse_c_typ
			 (fun t -> r.a_typ <-t) attrs children
		     | other -> Format.printf 
			 "Ignoring child in array:%s@." other)
    children;
  set r

let parse_return_value set attrs children =
  let r = dummy_return_value () in
  List.iter (fun (key,v) -> match key with
	       | "transfer-ownership" -> 
		   parse_ownership (fun o -> r.r_ownership <-o) v
	       | "doc" -> r.r_doc <- v
	       | other -> Format.printf 
		   "Ignoring attribute in return-value:%s@." other)
    attrs;
  List.iter (function 
	     | PCData s -> 
		 Format.printf "Ignoring PCData in return-value:%s@." s
	     | Element (key,attrs,children) -> 
		 match key with 
		   | "type" -> parse_c_typ (fun t -> 
					      assert (r.r_typ= NoType); 
					      r.r_typ <-Typ t)
		       attrs children
		   | "array" -> parse_array 
		       (fun t -> assert (r.r_typ= NoType); 
			  r.r_typ <- Array t) attrs children
		   | other -> Format.printf 
		       "Ignoring child in return-value:%s@." other)
    children;
  set r
  
let parse_property set attrs children =
  let p = dummy_property () in
  List.iter (fun (key,v) -> match key with
	       | "name" -> p.pr_name <- v
	       | "doc" -> p.pr_doc <- v
	       | "version" -> p.pr_version <- v
	       | "writable" -> p.pr_writable <- v="1"
	       | "readable" -> p.pr_readable <- v="1"
	       | "construct" -> p.pr_construct <- v="1"
	       | "construct-only" -> p.pr_construct_only <- v="1"
	       | other -> Format.printf 
		   "Ignoring attribute in property:%s@." other)
    attrs;
  List.iter (function 
	     | PCData s -> 
		 Format.printf "Ignoring PCData in property:%s@." s
	     | Element (key,attrs,children) -> 
		 match key with 
		   | "type" -> parse_c_typ (fun t -> 
					      assert (p.pr_typ=NoType); 
					      p.pr_typ <-Typ t)
		       attrs children
		   | "array" -> parse_array 
		       (fun t -> assert (p.pr_typ= NoType); 
			  p.pr_typ <- Array t) attrs children
		   | other -> Format.printf 
		       "Ignoring child in property:%s@." other)
    children;
  set p

let parse_parameter set attrs children = 
  let r = dummy_parameter () in
  List.iter (fun (key,v) -> match key with 
	       | "name" -> r.p_name <- v
	       | "transfer-ownership" -> 
		   parse_ownership 
		     (fun o -> r.p_ownership <- o)
		     v
	       | "scope" -> r.p_scope <- v
	       | "closure" -> r.p_closure <- v
	       | "destroy" -> r.p_destroy <- v
	       | "direction" -> 
		   begin match v with
		     | "in" -> r.p_direction <- DIn
		     | "out" -> r.p_direction <- DOut
		     | "inout" -> r.p_direction <- DInOut
		     | s -> 
			 Format.printf 
			   "Ignoring direction in parameter: %s@." s
		   end
	       | "allow-none" -> r.p_allow_none <- v="1"
	       | "doc" -> r.p_doc <- v
	       | other -> 
		   Format.printf "Ignoring attribute in parameter: %s@." other) 
    attrs;
  List.iter (function 
	       | PCData s -> 
		   Format.printf "Ignoring PCData in parameter:%s@." s
	       | Element (key,attrs,children) -> 
		   match key with 
		     | "type" -> parse_c_typ
			 (fun t -> r.p_typ <- Typ t) attrs children
		     | "varargs" -> r.p_varargs <- true
		     | "array" -> parse_array 
			 (fun t -> r.p_typ <- Array t) attrs children
		     | other -> Format.printf 
			 "Ignoring child in parameter:%s@." other)
    children;
  set r
  
  
let parse_parameters set attrs children =
  let r = ref [] in
  List.iter (fun (key,v) -> match key with
	       | other -> Format.printf 
		   "Ignoring attribute in parameters:%s@." other)
    attrs;
  List.iter (function 
	     | PCData s -> 
		 Format.printf "Ignoring PCData in parameters:%s@." s
	     | Element (key,attrs,children) -> 
		 match key with 
		   | "parameter" -> parse_parameter
		       (fun t -> r := t::!r) attrs children
		   | other -> Format.printf 
		       "Ignoring child in parameters:%s@." other)
    children;
  set (List.rev !r)
		       
let parse_function set attrs children = 
  let fct = dummy_function () in
  List.iter (fun (key,v) -> match key with 
	       | "name" -> fct.f_name <- v
	       | "c:identifier" -> fct.c_identifier <- v
	       | "version" -> fct.version <- v
	       | "doc" -> fct.doc <- v
	       | "deprecated" -> fct.deprecated <- v
	       | "deprecated-version" -> fct.deprecated_version <- v
	       | "throws" -> fct.throws <- v="1"
	       | other -> 
		   Format.printf "Ignoring attribute in fct: %s@." other) 
    attrs;
  if is_caml_avoid fct.f_name then
    fct.f_name <- fct.c_identifier;
  List.iter (function 
	       | PCData s -> Format.printf "Ignoring PCData in fct:%s@." s
	       | Element (key,attrs,children) -> 
		   match key with 
		     | "return-value" -> 
			 parse_return_value 
			   (fun r -> fct.return_value <- r)
			   attrs 
			   children
		     | "parameters" -> 
			 parse_parameters (fun l -> fct.parameters <- l)
			   attrs
			   children
		     | other -> Format.printf "Ignoring fct child: %s@." other) 
    children;
  set fct

let parse_constructor set attrs children = 
  parse_function set attrs children

let parse_method set attrs children = 
  parse_function set attrs children

let parse_klass set attrs children = 
  let k = dummy_klass () in
  List.iter (fun (key,v) -> match key with 
	       | "name" -> k.c_name <- v
	       | "c:type" -> k.c_c_type <- v
	       | "doc" -> k.c_doc <- v
	       | "parent" -> k.c_parent <- v
	       | "glib:type-name" -> k.c_glib_type_name <- v
	       | "glib:get-type" -> k.c_glib_get_type <- v
	       | "glib:type-struct" -> k.c_glib_type_struct <- v
	       | "abstract" -> k.c_abstract<- v="1"
	       | other -> 
		   Format.printf "Ignoring attribute in klass %s: %s@." 
		     k.c_name 
		     other)
    attrs;
  List.iter (function 
	       | PCData s -> Format.printf "Ignoring PCData in klass:%s@." s
	       | Element (key,attrs,children) -> 
		   match key with 
		     | "function" -> 
			 parse_function 
			   (fun f -> k.c_functions <- f::k.c_functions)
			   attrs
			   children
		     | "constructor" -> 
			 parse_constructor 
			   (fun f -> k.c_constructors <- f::k.c_constructors)
			   attrs
			   children
		     | "method" -> 
			 parse_method 
			   (fun f -> k.c_methods <- f::k.c_methods)
			   attrs
			   children
		     | "property" -> 
			 parse_property 
			   (fun f -> k.c_properties <- f::k.c_properties)
			   attrs
			   children
		     | "field"|"implements"|"virtual-method" -> 
			 if !debug then 
			   Format.printf
			     "Ignoring unused child in klass %s: %s@."
			     k.c_name
			     key
		     | other -> 
			 Format.printf "Ignoring child in klass %s: %s@."
			   k.c_name
			 other) 
    children;
  set k
    
let rec parse_xml x = 
  match x with 
    | PCData c -> Format.printf "CDATA: %S@ " c
    | Element (key,attrs,children) -> 
	let children =
	  begin match key with 
	    | "function" ->
		parse_function register_function attrs children;
		[]
	    | "class" -> 
		parse_klass (fun k -> register_klass k) attrs children;
		[]
	    | _ -> children
	  end
	in List.iter parse_xml children

let parse f = 
  try 
    Format.printf "Parsing '%s'@." f;
    let xml = Xml.parse_file f in
    parse_xml xml
  with Xml.Error m -> 
    Format.printf "XML error:%s@." (Xml.error m);
    exit 1

 let () = 
   for i=1 to Array.length Sys.argv - 1 do
     parse Sys.argv.(i)
   done;
  let funcs,klass = emit_all () in
  let stub_ml_channel = open_out "stubs.ml" in
  let ml = Format.formatter_of_out_channel stub_ml_channel in
  let stub_c_channel = open_out "ml_stubs.c" in
  let c = Format.formatter_of_out_channel stub_c_channel in
  Format.fprintf ml "%s@\n" (Buffer.contents ml_header);
  Format.fprintf c "%s@\n" (Buffer.contents c_header);
  List.iter (Pretty.klass ~ml ~c) klass;
  Pretty.functions ~ml ~c funcs;
  Format.fprintf ml "@.";
  Format.fprintf c "@.";
  close_out stub_ml_channel;
  close_out stub_c_channel;
