(* Julien Verlaguet, Yoann Padioleau
 *
 * Copyright (C) 2019 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 * 
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)

module Ast = Ast_js

module ISet = Set.Make (Int)
module SSet = Set.Make (String)

(* Use the modules below when you want to use ocamldebug and better 
 * see the contents of env and heap *)
module SMapDebug = struct
  (* when want to use ocamldebug and better see the contents of env.vars *)
  type 'a t = (string * 'a) list
  let empty = []
  let fold f xs acc = 
    List.fold_left (fun acc (x, y) -> f x y acc) acc xs
  let iter f xs = List.iter (fun (s,v) -> f s v) xs
  let add s v xs = (s,v)::xs
  let remove s xs = List.remove_assoc s xs
  let find s xs = List.assoc s xs
end

module IMapDebug = struct

  type 'a t = (int * 'a) list
  let empty = []
  let fold f xs acc = 
    List.fold_left (fun acc (x, y) -> f x y acc) acc xs
  let iter f xs = List.iter (fun (s,v) -> f s v) xs
  let add s v xs = (s,v)::xs
  let remove s xs = List.remove_assoc s xs
  let find s xs = List.assoc s xs
  let mem n xs = List.mem_assoc n xs
end

(* 
module IMap = Map.Make (Int)
module SMap =  Map.Make (String) 
*)
module IMap = IMapDebug
module SMap = SMapDebug


(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Main types and data structures used by the abstract interpreter
 * (mostly the "environment" and the "heap").
 *
 * In the abstract interpreter, all variables are pointers to value.
 * So with 'x = 42;' we have x = &1{42}.
 * In 'env.vars' we have "x" = Vptr 1 and in the 'heap' we
 * then have [0 -> ...; 1 -> Vint 42]
 * meaning that x is a variable with address 1, where the content
 * of this cell is the value 42. 
 *
 * Why this model? Why not having variables contain directly values,
 * that is x = {42} without any Vptr? Because dealing with lvalues in the
 * interpreter would then be tedious. For instance with 'x = array(1,2,3);'
 * How would you handle 'x[0] = 1;' without any Vptr and a heap?
 * You would need to use OCaml references to mimic those Vptr.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type code_database = 
  (Ast.qualified_name, Ast.var) Hashtbl.t

(* The abstract interpreter manipulates "values" *)
type value =
  (* Precise values. 
   * Vstring can be useful for Require?
   * Vint can be useful for?
   *)
  | Vbool   of bool
  | Vint    of int
  | Vfloat  of float
  | Vstring of string

  (* could be useful for nullpointer analysis *)
  | Vnull
  | Vundefined

  (* We converge quickly to a very abstract value. Values are either
   * a single precise value (e.g. 42), or a type (e.g. int). No
   * intermediate range for instance.
   *)
  | Vabstr  of type_

  (* Union of possible types/values, ex: null | object, bool | string, etc.
   * This could grow a lot, so the abstract interpreter needs to turn
   * that into a Vany at some point ???
   * todo: used also to record a list of possible function candidates
   *)
  | Vsum    of value list

  (* A pointer is an int address in the heap. A variable is a pointer
   * to a value. A field is also a pointer to a value. A class is a
   * pointer to Vobject. An object too. By using this intermediate we
   * can easily change the value of a variable by modifying the heap.
   *)
  | Vptr of int

  (* pad: because of some imprecision, we actually have a set of addresses? *)
  | Vref    of ISet.t

  (* TODO still valid comment?
   * Objects are represented as a set of members where a member can be
   * a constant, a field, a static variable, or a method. for instance with:
   * 
   *    class A {
   *      public fld = 1;
   *      static public fld2 = 2;
   *      public function foo() { }
   *      static public function bar() { }
   *      const CST = 3;
   *    }
   *    o = new A();
   * 
   * we will get:
   * 
   * o = &2{&REF 16{object(
   *   'fld' => &19{&18{1}}, 
   *   '$fld2' => &10{&9{2}})
   *   'foo' => method(&17{&REF 16{rec}), 
   *   'bar' => method(&17{&REF 16{rec}), 
   *   'CST' => 2, 
   *   }}
   * 
   * Note that static variables have $ in their name but not
   * regular fields. They are also shared by all objects of the class.
   * 
   * A class is actually also represented as a Vobject, with a special
   * *BUILD* method which is called when we do 'new A()', so on previous
   * example we will get in env.globals:
   * A = &6{&5{object(
   *     '$fld2' => &10{&9{2}})
   *     'foo' => method(Null), 
   *     'bar' => method(Null), 
   *     'CST' => 2, 
   *     '*BUILD*' => method(Null), 
   *  }}
   * 
   * TODO, not sure why we get foo() here there too.
   *)
  | Vobject of value SMap.t (* todo: and special value for prototype? *)

  (* try to differentiate the different usage of JS objects *)
  | Varray of value (* value is a ptr to a union representing all the elts *)
  (* Vrecord of value SMap.t *)

  (* TODO still valid comment?
   * The first 'value' below is for 'this' which will be a pointer to
   * the object. The only place where it's used is in Unify.value.
   * Note that this, as well as self/parent, are also handled
   * via closures in make_method().
   * 
   * The integer key of the IMap below is a unique identifier for a 
   * method (we could have chosen the full name as in "A::foo"). The
   * Imap is a set of methods because when we unify/merge objects,
   * we merge methods and remember all possible values for this method.
   * It's essentially a (closures Set.t) but because we can't compare
   * for equality closures in OCaml we need this method id intermediate
   * and IMap. See sum_call() in the interpreter and call_methods().
   * 
   * make_method() builds the method closure with self/parent/this
   * correctly bind to the right class and object pointers.
   * 
   * What is the value of x given: 
   *   class A { public function foo() { } }
   *   x = new A();
   * It should be:
   * x = &2{REF 1{Vobject (["foo"->Vmethod (&2{rec}, [0x42-> (<foo closure>)])])}}
   *)
  | Vmethod of value * (env -> heap -> Ast.expr list -> heap * value) IMap.t

  (* TODO still valid comment?
   * We would need a Vfun too if we were handling Lambda. But for
   * regular function calls, we just handle 'Call (Id "...")' specially
   * in the interpreter (but we don't for 'Call (Obj_get ...)', hence
   * this intermediate Vmethod value above).
   *)

  (* tainting analysis for security *)
  | Vtaint of string

  (* usually used when we don't handle certain constructs or when
   * the code is too dynamic
   *)
  | Vany

  and type_ =
    | Tbool
    | Tnum (* merge of Tint and Tfloat *)
    | Tstring

(* this could be one field of env too, close to .vars and .globals *)
and heap = {
  (* a heap maps addresses to values (which can also be addresses (Vptr)) *)
  ptrs: value IMap.t;
}

(* mutually recursive types because Vmethod above needs an env *)
and env = {
  db: code_database;

  (* local variables and parameters (will be pointer to values) *)
  vars    : value SMap.t ref;

  (* globals.
   * This is also used for classes which are considered as globals
   * pointing to a Vobject with a method *BUILD* that can build objects
   * of this class.
   * TODO 'globals' is also used for self/parent, and this (set/unset
   * respectively when entering/leaving the method)
   *)
  globals : value SMap.t ref;

  (* for debugging, to print for instance in which file we have XSS  *)
  file    : string ref;
  (* current function processed, used for? *)
  cfun    : string;

  (* number of recursive calls to a function f. if >2 then stop, for fixpoint. 
   * todo: should not be called stack. Path above is actually the call stack.
   *)
  stack   : int SMap.t;

  (* TODO opti: cache of already processed functions safe for tainting *)
  safe    : value SMap.t ref;
  
  (* TODO call stack used for debugging when found an XSS hole and used also
   * for callgraph generation.
   * todo: could be put in the env too, next to 'stack' and 'safe'? take
   * care of save_excursion though.
   *)
  path: (*Callgraph_php2.node*) Ast.qualified_name list ref;

}

(* The code of the abstract interpreter and tainting analysis used to be
 * in the same file, but it's better to separate concerns, hence this module
 * which contains hooks that a tainting analysis can fill in.
 *)
(* TODO module type TAINT =
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let empty_heap = {
  ptrs  = IMap.empty;
}

let empty_env db file =
  let globals = ref SMap.empty in
  let locals = ref SMap.empty in
  { db = db;
    file = ref file;
    globals = globals;
    vars = locals; 
    cfun = "*TOPLEVEL*";
    stack   = SMap.empty ;
    safe = ref SMap.empty;
    path = ref [];
  }

(*****************************************************************************)
(* String-of like *)
(*****************************************************************************)

let rec list o f l =
  match l with
  | [] -> ()
  | [x] -> f o x
  | x :: rl -> f o x; o ", "; list o f rl

let rec value ptrs o x =
  match x with
  | Vany -> o "Any"

  | Vnull -> o "Null"
  | Vundefined -> o "Undefined"

  | Vtaint s -> o "PARAM:"; o s

  | Vabstr ty -> type_ o ty

  | Vbool b   -> o (string_of_bool b)
  | Vint n    -> o (string_of_int n)
  | Vfloat f  -> o (string_of_float f)
  | Vstring s -> o "'"; o s; o "'"

  | Vptr n ->
      if IMap.mem n ptrs then begin
        o "&";
        o (string_of_int n);
        o "{";
        value (IMap.remove n ptrs) o (IMap.find n ptrs);
        o "}"
      end else
        (* this happens for instance for objects which are pointers
         * to pointers to Vobject with for members a set of Vmethod
         * where the first element, this, backward points to the
         * object.
         *)
        o "rec"

  | Vref set ->
      o "&REF ";
      let l = ISet.elements set in
      list o (fun o x -> o (string_of_int x)) l;
      let n = ISet.choose set in
      (try
          o "{";
          value (IMap.remove n ptrs) o (IMap.find n ptrs);
          o "}"
      with Not_found -> o "rec"
      )

  | Vobject m ->
      let vl = SMap.fold (fun x y acc -> (x, y) :: acc) m [] in
      o "object(";
      list o (fun o (x, v) -> o "'"; o x ; o "' => "; value ptrs o v) vl;
      o ")"
  | Vmethod (v, _imap) -> o "method("; value ptrs o v; o ")"
  | Varray v ->
      o "array("; value ptrs o v;  o ")";
  | Vsum vl ->
      o "choice(";
      list o (value ptrs) vl;
      o ")"

and type_ o x =
  match x with
  | Tbool -> o "bool"
  | Tnum -> o "num"
  | Tstring -> o "string"

(*****************************************************************************)
(* Debugging *)
(*****************************************************************************)

let print_locals_and_globals o env heap =
  SMap.iter (fun s v ->
    o "|"; o s ; o " = "; value heap.ptrs o v; o "\n"
  ) !(env.vars);
  SMap.iter (fun s v ->
    o "|"; o "GLOBAL "; o s ; o " = "; value heap.ptrs o v; o "\n"
  ) !(env.globals)


let string_of_value heap v =
  Common2.with_open_stringbuf (fun (_pr, buf) ->
    let pr s = Buffer.add_string buf s in
    value heap.ptrs pr v
  )
