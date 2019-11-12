(* Yoann Padioleau
 * 
 * Copyright (C) 2009, 2010, 2011 Facebook
 * Copyright (C) 2019 r2c
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

open Ast_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A Control Flow Graph (CFG) for AST generic.
 *
 * Note that this is just for intra-procedural analysis. The CFG covers
 * just one function. For inter-procedural analysis you may want to look
 * at pfff/graph_code/ (or invest in learning datalog).
 *
 * history:
 *  - CFG for C for coccinelle
 *  - CFG for PHP for checkModule at facebook
 *  - CFG for AST generic for scheck at r2c
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type node = {
  (* later: For now we just have node_kind, but with some data-flow
   * analysis or with temporal logic we may want to add extra information
   * in each CFG nodes. 
   * alt: We could also record such extra information in an external table
   * that maps Ograph_extended.nodei, that is nodeid, to some information.
   *)
  n: node_kind;

  (* for error report *)
  i: Parse_info.t option;
} 

  and node_kind = 

      (* special fake cfg nodes *)
      | Enter
      | Exit 

      (* alt: An alternative is to store such information in the edges, but
       * experience shows it's easier to encode it via regular nodes
       *)
      | TrueNode
      | FalseNode

      | IfHeader of expr
      | WhileHeader of expr
      | DoHeader
      | DoWhileTail of expr
      | ForHeader
      | ForeachHeader (* TODO  of foreach_variable list *)

      | SwitchHeader of expr
      | SwitchEnd
      | Case
      | Default

      | Return of expr
      | Break of expr option
      | Continue of expr option

      | TryHeader
      | CatchStart
      | Catch
      | TryEnd
      | Throw of expr

      | Join

      | Parameter of parameter

      (* statements without multiple outgoing or ingoing edges, such
       * as echo, expression statements, etc.
       *)
      | SimpleStmt of simple_stmt

    (* not used for now, was used in coccinelle:
      | BlockStart of tok (* { *)
      | BlockEnd of tok (* } *)
      | Else
      | Elsif
    *)

     and simple_stmt = 
         | ExprStmt of expr
         | TodoSimpleStmt
         (* TODO? expr includes Exit, Eval, Include, etc which
          * also have an influence on the control flow ...
          * We may want to uplift those constructors here and have
          * a better expr type
          *)


(* For now there is just one kind of edge. Later we may have more, 
 * see the ShadowNode idea of Julia Lawall.
 *)
type edge = Direct 

type flow = (node, edge) Ograph_extended.ograph_mutable

type nodei = Ograph_extended.nodei

(*****************************************************************************)
(* String of *)
(*****************************************************************************)

let short_string_of_node_kind nkind = 
  match nkind with
  | Enter -> "<enter>"
  | Exit -> "<exit>"
  | SimpleStmt _ -> "<simplestmt>"
  | Parameter _ -> "<parameter>"
  | WhileHeader _ -> "while(...)"

  | TrueNode -> "TRUE path"
  | FalseNode -> "FALSE path"

  | IfHeader _ -> "if(...)"
  | Join -> "<join>"

  | Return _ -> "return ...;"

  | DoHeader -> "do"
  | DoWhileTail _ -> "while(...);"

  | Continue _ -> "continue ...;"
  | Break _ -> "break ...;"

  | ForHeader -> "for(...)"
  | ForeachHeader  -> "foreach(...)"

  | SwitchHeader _ -> "switch(...)"
  | SwitchEnd -> "<endswitch>"

  | Case -> "case: ..."
  | Default -> "default:"

  | TryHeader -> "try"
  | CatchStart -> "<catchstart>"
  | Catch -> "catch(...)"
  | TryEnd -> "<endtry>"

  | Throw _ -> "throw ...;"

let short_string_of_node node =
  short_string_of_node_kind node.n 

(*****************************************************************************)
(* Accessors *)
(*****************************************************************************)

let find_node f cfg =
  cfg#nodes#tolist +> Common.find_some (fun (nodei, node) ->
    if f node then Some nodei else None
  )

let find_exit cfg = find_node (fun node -> node.n = Exit) cfg
let find_enter cfg = find_node (fun node -> node.n = Enter) cfg

(* using internally graphviz dot and ghostview on X11 *)
let (display_flow: flow -> unit) = fun flow ->
  flow |> Ograph_extended.print_ograph_mutable_generic  
    ~s_of_node:(fun (_nodei, node) -> 
      short_string_of_node_kind node.n, None, None
    )

