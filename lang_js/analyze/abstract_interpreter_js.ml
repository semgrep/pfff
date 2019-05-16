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
open Common

open Ast_js
open Abstract_interpreter_js_env
open Abstract_interpreter_js_helpers

module A = Ast_js
module Env = Abstract_interpreter_js_env
module H = Abstract_interpreter_js_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Abstract interpreter for Javascript
 * TODO with hooks for tainting analysis, callgraph generation, typing.
 *
 * An abstract interpreter kinda mimics a normal interpreter by
 * also executing a program, in a top-down manner, modifying a heap,
 * managing local and global variables, but maintains abstract
 * values for variables instead of concrete values as in a regular
 * interpreter. See abstract_interpreter_js_env.ml
 *
 * For instance, on 'if(cond()) { x = 42; } else { x = 3;}'
 * the abstract interpreter will actually "execute" both branches and
 * merge/unify the different values for the variable in a more
 * abstract value. So, while processing the first branch the interpreter
 * will add a new local variable x, allocate space in the abstract
 * heap, and sets its value to the precise (Vint 42). But after
 * both branches, the abstract interpreter will unify/merge/abstract
 * the different values for x to a (Vabstr Tint) and from now on,
 * the value for x will be that abstract.
 *
 * Two key concepts are the different level of abstractions
 * (resulting from unification/generalization values), and
 * reaching a fixpoint.
 *
 * References:
 *  - http://en.wikipedia.org/wiki/Abstract_interpretation
 *
 * Actually the unify/merge/abstract in the example above will happen
 * as soon as processing the else branch in the current algorithm.
 * So there will be no (Vint 3) in the heap. 
 * TODO See tests/js/ia/if.js.
 *
 * The algorithm is kinda:
 *  - flow insensitive, because??
 *  - path insensitive, because we don't look for instance
 *    at the resulting value of a condition in a 'if' to determine
 *    unreachable path
 *  - BUT context sensitive, because we treat different calls
 *    to the same function differently (we unroll each call).
 *    There is a limit on the depth of the call stack though,
 *    see max_depth below.
 *
 * To help you debug the interpreter you can put some
 * 'var_dump(x)' in the Javascript file to see the abstract
 * value of a variable at a certain point in the program.
 *
 * pad's notes:
 *  - strings are sometimes (ab)used to not only represent
 *    variables and entities but also special variables:
 *    * "*return*", to communicate the return value to the caller
 *    * "*myobj*", to build an object
 *    * "*BUILD*", to call the 'new' method of a class
 *    * special "self"/"parent", in env.globals
 *    * "$this", also in env.globals
 *  - How the method lookup mechanism works? there is no lookup,
 *    instead at the moment where we build the object, we put
 *    all the methods of the parents in the new object. But then
 *    what about the use of self:: or parent:: when executing the
 *    code of a parent method? The references to self and parent
 *    are in the closure of the method and are pointers to
 *    the fake object that represents the class.
 *
 * TODO:
 *  - ask juju about the many '??' in this file
 *  - still? the places where expect a VPtr, and so need to call Ptr.get,
 *    or even a VptrVptr and so where need to call Ptr.get two times,
 *    and the places where expect a final value is not clear.
 *  - before processing the file, maybe should update the code database
 *    with all the entities in the file, cos when one process a script,
 *    many scripts have a main() or usage() but the code database
 *    stores only one.
 *  - $x++ is ignored (we don't really care about int for now)
 *  - C-s for Vany, it's usually a Todo
 *
 * TODO long term:
 *  - we could use the abstract interpreter to find bugs that my current
 *    checkers can't find (e.g. undefined methods in o->m() because
 *    of the better interprocedural class analysis, wrong type,
 *    passing null, use of undeclared field in o->fld, etc).
 *    But the interpreter first needs to be correct
 *    and to work on programs without so many exceptions.
 *    TODO just go through all constructs and find opportunities
 *    to detect bugs?
 *  - It could also be used for program understanding purpose
 *    by providing a kind of tracer.
 *  - maybe it could be combined with the type inference to give
 *    more precise results and find even more bugs.
 *
 * history:
 *  - basic values (Vint 2, Vstring "foo"), abstract value with types
 *    (Vabstr Tint), also range for ints. Special care for PHP references
 *    by using pointer to pointer to val via the Vptr, as in Zend.
 *    Basically handling simple sequences of assignements.
 *  - loop, recursive functions, by fixpoint on the heap. Could configure
 *    the number of times we run the loop to allow to converge
 *    to a fixpoint after more than two iterations. When the fixpoint
 *    is not reached for certain variables, then set a more abstract
 *    value (for instance if the range for ints grows, then turn it into
 *    a Vabstr Tint). Manage branches such as ifthenelse by unifying/merging
 *    heaps
 *  - unfiying heaps was too costly, so just unify what
 *    is needed (unifying pointers), process ifthenelse "sequentially"
 *    not independently.
 *  - fixpoint was too costly, and when '$i = 1;for() { $i=$i+1 }' it does
 *    not converge if use range so make it in such a way to reach the fixpoint
 *    in one step when unify. In the end we don't unify heaps,
 *    we don't do fixpoint. So for the loop example before,
 *    first have $i = Vint 1, but as soon as have $i=$i+1, we say it's a
 *    Vabstr Tint (and this is the fixpoint, in just one step).
 *  - handle objects and other constructs.
 *  - all comments added by pad, split files, add mli, add unit tests,
 *    add tests/ia/*.php, fixed bugs, added strict mode, etc
 *)

(*****************************************************************************)
(* Configuration *)
(*****************************************************************************)

(* Generating a callgraph or not. Could be put in the environment, but
 * it's more about a configuration option than a local information in
 * an environment.
 *)
let extract_paths = ref true

(* Number of function calls to handle.
 * Julien thinks 6 is the value above which there is diminushing return
 * regarding the callgraph. The size of the callgraph does not grow that
 * much when goes from 6 to 7.
 *)
let max_depth = ref 4

(* throw exn instead of passing-over silently unhandled constructs *)
let strict = ref true

let show_vardump = ref true

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)

(* used by unit testing when encountering the 'checkpoint()' function call *)
let _checkpoint_heap = ref
  (None: (Env.heap * value SMap.t (* local vars *)) option)

(* for callgraph generation *)
(*
let (graph: Callgraph_php2.callgraph ref) = ref Map_.empty
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* less: could maybe factorize in Unknown of Database_code.entity_kind,
 *  but for members for instance we also want to show also some
 *  extra information like the available methods and fields.
 *)
exception UnknownFunction of string
exception UnknownClass    of string
exception UnknownConstant of string

exception UnknownMember of string * string * string list
exception UnknownObject

(* Exception thrown when a call to a function or method is not known.
 * This happens when the code is intrisically far too dynamic or
 * when we have done too aggressive approximations in the interpreter
 * on certain values.
 *)
exception LostControl

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* add an edge in callgraph *)
let save_path _env _target =
  if !extract_paths
  then ()
 (*graph := CG.add_graph (List.hd !(env.path)) target !graph *)

let fake_info str = { Parse_info.
  token = Parse_info.FakeTokStr (str, None);
  transfo = Parse_info.NoTransfo;
  }

let mk_id s scope =
  Id ((s, fake_info s), ref scope)

let opt_to_list = function
  | None -> []
  | Some x -> [x]

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(*module Interp = functor (Taint: Env_interpreter_php.TAINT) -> struct *)

let rec program env heap program =

  (* env.path := [CG.File !(env.file)]; *)
  let finalheap = toplevels env heap program in
  finalheap

(* ---------------------------------------------------------------------- *)
(* Toplevel *)
(* ---------------------------------------------------------------------- *)
and toplevels env heap xs = 
  List.fold_left (toplevel env) heap xs

and toplevel env heap = function
  | S (_tok, st) -> stmt env heap st
  | V var ->
     (match var.v_init with
     | Fun _ -> 
        if !(var.v_resolved) = NotResolved
        then Hashtbl.add env.db (Ast.str_of_name var.v_name) var;
        heap
     | _ -> 
        (match !(var.v_resolved) with
        (* probably forgot graph_code_js naming phase, probably because
         * we are in -test_ai_js; just consider it as a local
         *)
        | NotResolved ->
          stmt env heap (VarDecl var)
        | Local | Param -> raise Impossible
        (* n: treat globals lazily? *)
        | Global _ -> heap
        )
     )
  (* n: safe to skip, we already did the naming phase in graph_code_js so
   * now every names should be resolved
  *)
  | Import _ | Export _ -> heap

(* ---------------------------------------------------------------------- *)
(* Stmt *)
(* ---------------------------------------------------------------------- *)
and stmt env heap x =
  match x with
  | VarDecl var ->
     (match !(var.v_resolved) with 
     | Local | NotResolved -> ()
     | Global _ | Param -> raise Impossible
     );
     let heap, v = expr env heap var.v_init in
     let heap, v = Ptr.new_val heap v in
     let str = Ast.str_of_name var.v_name in
     Var.set env str v;
     heap

  (* special keywords in the code to debug the abstract interpreter state.
   * less: find a function so that one can easily run a JS test file
   * with node or 'pfff -test_ai_js' and get both run working
   *)
  | ExprStmt (Apply (Id ((("show" | "var_dump"),_),_), [e])) ->
      let heap, v = expr env heap e in
      if !show_vardump then begin
        Env.print_locals_and_globals print_string env heap;
        pr (Env.string_of_value heap v);
      end;
      heap

  (* used by unit testing *)
  | ExprStmt (Apply (Id (("checkpoint",_),_), [])) ->
      _checkpoint_heap := Some (heap, !(env.vars));
      heap

  | ExprStmt e ->
      let heap, _v = expr env heap e in
      heap

  | Block xs ->
      stmtl env heap xs

  (* With 'if(true) { x = 1; } else { x = 2; }'
   * we will endup with a heap with x = &1{int}.
   * Going in first branch will populate the heap
   * and env.vars with an entry for x, and when visiting
   * the second branch the second assignment will
   * cause a generalization for x to an int.
   *)
  | If (c, st1, st2) ->
      (* todo: warn type error if value/type of c is not ok? *)
      let heap, _v = expr env heap c in
      (* TODO: what about var defined in only one branch? *)

      (* not that we are not doing any path sensitivity here ...
       * even if we can statically determine that c is always true,
       * we just process both branches, and we actually process them
       * sequentially (we used to process them independently
       * and then merge/unify the resulting heaps).
       *)
      let heap = stmt env heap st1 in
      let heap = stmt env heap st2 in
      heap

  (* this may seem incorrect to treat 'do' and 'while' in the same way,
   * because the evaluation of e does not happen at the same time.
   * But here we care about the pointfix of the values, and so
   * the order does not matter.
   * todo: but need to process the stmts 2 times at least to get a fixpoint?
   *)
  | Do (st, e) | While (e, st) ->
      let heap, _ = expr env heap e in
      let heap = stmt env heap st in
      heap

  | For (_, _) -> raise Todo

  | Switch (e, cl) ->
      let heap, _ = expr env heap e in
      let heap = List.fold_left (case env) heap cl in
      heap

  (* n: we do not care about the control flow; we are flow-insensitive *)
  | Continue _lblopt | Break _lblopt -> 
     heap
  | Label (_lbl, st) -> stmt env heap st

  | Return e ->
      (* the special "*return*" variable is used in call_fun() below *)
      let id = mk_id "*return*" Local in
      let heap, _ = expr env heap (Assign (id, e)) in
      heap

  | Throw e ->
      let heap, _ = expr env heap e in
      heap

  | Try (st, cl, fl) ->
      let heap = stmt env heap st in
      let heap = List.fold_left (catch env) heap (opt_to_list cl) in
      let heap = List.fold_left (finally env) heap (opt_to_list fl) in
      heap

(* What if break/continue/return/throw in the middle of the list of stmts?
 * Do we still abstract interpret the rest of the code? Yes because
 * we care about the pointfix of the values, and so we don't really
 * care about the control flow. The analysis is kinda of flow insensitive.
 *)
and stmtl env heap xs = List.fold_left (stmt env) heap xs

and case env heap x =
  match x with
  | Case (e, st) ->
      let heap, _ = expr env heap e in
      let heap = stmt env heap st in
      heap
  | Default st ->
      let heap = stmt env heap st in
      heap

and catch env heap (_name, st) =
  (* TODO: add name as local *)
  stmt env heap st
and finally env heap st =
  stmt env heap st

(* ---------------------------------------------------------------------- *)
(* Expr *)
(* ---------------------------------------------------------------------- *)
and expr env heap x =
(*
  if !Taint.taint_mode
  then Taint.taint_expr env heap
    (expr_, lvalue, get_dynamic_function, call_fun, call, assign) !(env.path) x
   else 
*)
  expr_ env heap x

(* will return a "concrete" value, or a pointer to a concrete value *)
and expr_ env heap x =
  match x with
  | Bool (b,_) -> heap, Vbool b
  | Num (s, _) ->
       (try 
        let i = int_of_string s in
        heap, Vint i
       with _ ->
        let f = float_of_string s in
        heap, Vfloat f
       )
  | String (s,_) -> heap, Vstring s
  | Regexp (s,_) -> heap, Vstring s
  | Nop -> heap, Vundefined

  | IdSpecial (special, _) ->
     (match special with
     | Null -> heap, Vnull
     | Undefined -> heap, Vundefined
     | _ -> raise Todo
    )


  (* with 'x = (true)? 42 : "foo"' we will return a
   * Tsum(Vint 42, Vstring "foo")
   *)
  | Conditional (e1, e2, e3) ->
      let heap, _ = expr env heap e1 in
      let heap, v1 = expr env heap e2 in
      let heap, v2 = expr env heap e3 in
      let heap, v = Unify.value heap v1 v2 in
      heap, v

  (* code for x = ..., o->fld = ..., etc *)
  | Assign (e1, e2) ->
      let heap, lval = lvalue env heap e1 in
      let heap, rval = expr env heap e2 in
      assign env heap lval rval

  | Apply (Id (name, resolved), xs) ->
      (match !resolved with
      | Global qualified_name ->
          call_qualified env heap qualified_name xs
      | NotResolved ->
          let qualified_name = Ast.str_of_name name in
          call_qualified env heap qualified_name xs
      | Local | Param ->
        let heap, v = expr env heap (Id (name, resolved)) in
        call env heap v xs
      )

  | Apply (IdSpecial (spec, _), xs) ->
      let heap, vs = Utils.lfold (expr env) heap xs in
      special heap env spec vs

  (* expression call or method call (Call (Obj_get...)) or
   * static method call (Call (Class_get ...))
   *)
  | Apply (e, el) ->
      let heap, v = expr env heap e in
      call env heap v el

  | Id _ as lv ->
      (* The lvalue will contain the pointer to val, e.g. &1{...}
       * so someone can modify it. See also assign() below.
       * But in an expr context, we actually want the value, hence
       * the Ptr.get dereference, so we will return {...}
       *)
      let heap, x = lvalue env heap lv in
      let heap, x = Ptr.get heap x in
      heap, x

  | Obj _
  | Class _
  | ObjAccess (_, _)

  | Fun (_, _)
    -> raise Todo

(* ---------------------------------------------------------------------- *)
(* Special *)
(* ---------------------------------------------------------------------- *)
and special heap _env spec vs =
  match spec, vs with
  | (Plus | Minus), 
      [Vint _ | Vabstr Tint] 
  | (Plus | Minus | Mul | Div | Mod | Expo), 
     [Vint _ | Vabstr Tint; Vint _ | Vabstr Tint] 
     -> heap, Vabstr Tint

  | _ -> 
    let s = Ocaml.string_of_v (Meta_ast_js.vof_special spec) in
    let strs = vs |> List.map (Env.string_of_value heap) |> Common.join "," in
    failwith (spf "Special: unsupported op: %s(%s)" s strs)

(* ---------------------------------------------------------------------- *)
(* Lvalue *)
(* ---------------------------------------------------------------------- *)
(* will return the lvalue, that is the pointer value, and not
 * the actual value, so that the caller can modify it.
 *)
and lvalue env heap x =
  match x with
  | Id (name, resolved) ->
     (match !resolved with
     | Local | Param | NotResolved ->
        let s = Ast.str_of_name name in
        Var.get env heap s
     | Global _qualified ->
        (* do lazily? if Not_found then evalue for first time *)
        raise Todo
     )
  | _ ->
    raise Todo
(*
    if !strict then failwith "lvalue not handled";
    heap, false, Vany
*)

(* ---------------------------------------------------------------------- *)
(* Assign *)
(* ---------------------------------------------------------------------- *)
and assign _env heap root(*lvalue*) v_root(*rvalue*) =
  let ptr = root in
  let heap, v' = Ptr.get heap ptr in
  let heap, v = Unify.value heap v_root v' in
  let heap = Ptr.set heap ptr v in
  heap, v

(* ---------------------------------------------------------------------- *)
(* Call *)
(* ---------------------------------------------------------------------- *)

and call_qualified _env _heap _qualified_name _el =
  raise Todo

and call env heap v el =
  match v with
  | Vsum l -> sum_call env heap l   el
  | x      -> sum_call env heap [x] el

and sum_call _env _heap _v _el =
  raise Todo

(* ---------------------------------------------------------------------- *)
(* Class *)
(* ---------------------------------------------------------------------- *)
