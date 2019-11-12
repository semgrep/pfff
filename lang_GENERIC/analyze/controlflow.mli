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
         | ExprStmt of expr
         | TodoSimpleStmt

(* For now there is just one kind of edge. Later we may have more, 
 * see the ShadowNode idea of Julia Lawall.
 *)
type edge = Direct 

type flow = (node, edge) Ograph_extended.ograph_mutable

type nodei = Ograph_extended.nodei

val find_node: (node -> bool) -> flow -> nodei
val find_enter: flow -> nodei
val find_exit: flow -> nodei

(* using internally graphviz 'dot' and ghostview 'gv' on X11 *)
val display_flow: flow -> unit

val short_string_of_node_kind: node_kind -> string
val short_string_of_node: node -> string
