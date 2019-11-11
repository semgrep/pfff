open Ast_generic

type node = {
  n: node_kind;
  i: Parse_info.t option;
} 
  and node_kind = 
      | Enter | Exit 
      | TrueNode | FalseNode

      | IfHeader of expr
      | WhileHeader of expr
      | DoHeader | DoWhileTail of expr
      | ForHeader | ForeachHeader

      | SwitchHeader of expr | SwitchEnd
      | Case | Default

      | Return of expr
      | Break of expr option  | Continue of expr option

      | TryHeader | CatchStart | Catch | TryEnd
      | Throw of expr

      | Join

      | Parameter of parameter

      | SimpleStmt of simple_stmt

     and simple_stmt = 
         | ExprStmt of expr * use_status
         | TodoSimpleStmt

       and use_status = 
       | Normal
       | SpecialMaybeUnused

(* For now there is just one kind of edge. Later we may have more, 
 * see the ShadowNode idea of Julia Lawall.
 *)
type edge = Direct 

type flow = (node, edge) Ograph_extended.ograph_mutable


val find_node: (node -> bool) -> flow -> Ograph_extended.nodei
val find_enter: flow -> Ograph_extended.nodei
val find_exit: flow -> Ograph_extended.nodei

(* using internally graphviz 'dot' and ghostview 'gv' on X11 *)
val display_flow: flow -> unit

val short_string_of_node_kind: node_kind -> string
val short_string_of_node: node -> string
