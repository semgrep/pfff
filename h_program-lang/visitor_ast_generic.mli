open Ast_generic

(* the hooks *)
type visitor_in = {
  kexpr: (expr  -> unit) * visitor_out -> expr  -> unit;
  kstmt: (stmt  -> unit) * visitor_out -> stmt  -> unit;
  ktype_: (type_  -> unit) * visitor_out -> type_  -> unit;
  kpattern: (pattern  -> unit) * visitor_out -> pattern  -> unit;

  kdef: (definition  -> unit) * visitor_out -> definition  -> unit;
  kdir: (directive  -> unit) * visitor_out -> directive  -> unit;
  kitem: (item  -> unit) * visitor_out -> item  -> unit;

  kattr: (attribute  -> unit) * visitor_out -> attribute  -> unit;
  kparam: (parameter  -> unit) * visitor_out -> parameter  -> unit;
  kname: (name -> unit)  * visitor_out -> name  -> unit;
  kentity: (entity -> unit)  * visitor_out -> entity  -> unit;

  kinfo: (tok -> unit)  * visitor_out -> tok  -> unit;
}
and visitor_out = any -> unit

val default_visitor : visitor_in

val mk_visitor: visitor_in -> visitor_out

(* poor's man fold *)
(* 
val do_visit_with_ref:
  ('a list ref -> visitor_in) -> any -> 'a list
*)
