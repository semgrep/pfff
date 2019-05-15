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
 * TODO with hooks for tainting analysis, callgraph generation, typing
 *
 * An abstract interpreter kinda mimics a normal interpreter by
 * also executing a program, in a top-down manner, modifying a heap,
 * managing local and global variables, etc, but maintains abstract
 * values for variables instead of concrete values as in a regular
 * interpreter. See abstract_interpreter_js_env.ml
 *
 * For instance on 'if(cond()) { x = 42; } else { x = 3;}'
 * the abstract interpreter will actually execute both branches and
 * merge/unify the different values for the variable in a more
 * abstract value. So, while processing the first branch the interpreter
 * will add a new local variable $x, allocate space in the abstract
 * heap, and sets its value to the precise (Vint 42). But after
 * both branches, the abstract interpreter will unify/merge/abstract
 * the different values for $x to a (Vabstr Tint) and from now on,
 * the value for $x will be that abstract.
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
 *    see max_depth.
 *
 * To help you debug the interpreter you can put some
 * 'var_dump(x)' in the Javascript file to see the abstract
 * value of a variable at a certain point in the program.
 *
 * pad's notes:
 *  - strings are sometimes (ab)used to not only represent
 *    variables and entities but also special variables:
 *    * "*return*", to communicate the return value to the caller
 *    * "*array*, to build an array
 *    * "*myobj*, to build an object
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
 *  - the id() function semantic is hardcoded
 *
 * TODO:
 *  - ask juju about the many '??' in this file
 *  - the places where expect a VPtr, and so need to call Ptr.get,
 *    or even a VptrVptr and so where need to call Ptr.get two times,
 *    and the places where expect a final value is not clear.
 *  - before processing the file, maybe should update the code database
 *    with all the entities in the file, cos when one process a script,
 *    many scripts have a main() or usage() but the code database
 *    stores only one.
 *  - $x++ is ignored (we don't really care about int for now)
 *  - many places where play with $ in s.(0)
 *  - C-s for Vany, it's usually a Todo
 *
 *
 * TODO long term:
 *  - we could use the ia also to find bugs that my current
 *    checkers can't find (e.g. undefined methods in $o->m() because
 *    of the better interprocedural class analysis, wrong type,
 *    passing null, use of undeclared field in $o->fld, etc).
 *    But the interpreter first needs to be correct
 *    and to work on www/ without so many exceptions.
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
 *
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

let show_vardump = ref false

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
let save_path env target =
  if !extract_paths
  then ()
 (*graph := CG.add_graph (List.hd !(env.path)) target !graph *)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(* module Interp = functor (Taint: Env_interpreter_php.TAINT) -> struct *)

let rec program env heap program =
  raise Todo


(* ---------------------------------------------------------------------- *)
(* Stmt *)
(* ---------------------------------------------------------------------- *)

(* ---------------------------------------------------------------------- *)
(* Expr *)
(* ---------------------------------------------------------------------- *)

(* ---------------------------------------------------------------------- *)
(* Lvalue *)
(* ---------------------------------------------------------------------- *)

(* ---------------------------------------------------------------------- *)
(* Assign *)
(* ---------------------------------------------------------------------- *)

(* ---------------------------------------------------------------------- *)
(* Call *)
(* ---------------------------------------------------------------------- *)

(* ---------------------------------------------------------------------- *)
(* Class *)
(* ---------------------------------------------------------------------- *)
