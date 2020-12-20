(* Iago Abal
 *
 * Copyright (C) 2020 r2c
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
open IL

module G = AST_generic
module F = IL
module D = Dataflow
module VarMap = Dataflow.VarMap

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* The type of an unknown constant. *)
type ctype =
  | Cbool
  | Cint
  | Cstr
  | Cany

type constness = Lit of AST_generic.literal | Cst of ctype | Dyn

(* map for each node/var whether a variable is constant *)
type mapping = constness Dataflow.mapping

module DataflowX = Dataflow.Make (struct
    type node = F.node
    type edge = F.edge
    type flow = (node, edge) Ograph_extended.ograph_mutable
    let short_string_of_node n =
      Display_IL.short_string_of_node_kind n.F.n
  end)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let string_of_ctype = function
  | Cbool -> "bool"
  | Cint  -> "int"
  | Cstr  -> "str"
  | Cany  -> "???"

let string_of_constness = function
  | Dyn   -> "dyn"
  | Cst t -> Printf.sprintf "cst(%s)" (string_of_ctype t)
  | Lit l ->
      match l with
      | G.Bool (b, _)   -> Printf.sprintf "lit(%b)" b
      | G.Int (s, _)    -> Printf.sprintf "lit(%s)" s
      | G.String (s, _) -> Printf.sprintf "lit(\"%s\")" s
      | ___else___      -> "lit(???)"

let str_of_name ((s, _tok), sid) =
  spf "%s:%d" s sid

(*****************************************************************************)
(* Constness *)
(*****************************************************************************)

(* TODO: Use Lib_AST.abstract_position_info_any (G.E (G.Literal l1)) and =*=. *)
let eq_literal l1 l2 =
  match l1, l2 with
  | G.Bool  (b1, _),  G.Bool  (b2, _)  -> b1 =:= b2
  | G.Int   (s1, _),  G.Int   (s2, _)
  | G.Float (s1, _),  G.Float (s2, _)
  | G.Char  (s1, _),  G.Char   (s2, _)
  | G.String (s1, _), G.String (s2, _) -> s1 =$= s2
  (* add more cases if needed *)
  | ___else___ -> false

let eq_ctype t1 t2 = t1 = t2

let ctype_of_literal = function
  | G.Bool _   -> Cbool
  | G.Int _    -> Cint
  | G.String _ -> Cstr
  | ___else___ -> Cany

let eq c1 c2 =
  match c1, c2 with
  | Lit l1, Lit l2 -> eq_literal l1 l2
  | Cst t1, Cst t2 -> eq_ctype t1 t2
  | Dyn,    Dyn    -> true
  | ___else___     -> false

let union_ctype t1 t2 = if eq_ctype t1 t2 then t1 else Cany

let union c1 c2 =
  match c1, c2 with
  | _ when eq c1 c2 -> c1
  | _any,   Dyn
  | Dyn,    _any    -> Dyn
  | Lit l1, Lit l2  ->
      let t1 = ctype_of_literal l1
      and t2 = ctype_of_literal l2 in
      Cst (union_ctype t1 t2)
  | Lit l1, Cst t2
  | Cst t2, Lit l1 ->
      let t1 = ctype_of_literal l1 in
      Cst (union_ctype t1 t2)
  | Cst t1, Cst t2 ->
      Cst (union_ctype t1 t2)
(*
let refine c1 c2 =
  match c1, c2 with
  | _ when eq c1 c2 -> c1
  | c,   Dyn
  | Dyn,    c    -> c
  | Lit _, Lit _
  | Lit _, Cst _
  | Cst _, Cst _ -> c1
  | Cst _, Lit _ -> c2

let refine_constness c_ref c' =
  match !c_ref with
  | None -> c_ref := Some c'
  | Some c -> c_ref := Some (refine c1 c2) *)

(*****************************************************************************)
(* Constness evaluation *)
(*****************************************************************************)

let literal_of_bool b =
  let b_str = string_of_bool b in
  (* TODO: use proper token when possible? *)
  let tok   = Parse_info.fake_info b_str in
  G.Bool(b, tok)

let literal_of_int i =
  let i_str = string_of_int i in
  (* TODO: use proper token when possible? *)
  let tok   = Parse_info.fake_info i_str in
  G.Int(i_str, tok)

let int_of_literal = function
  | G.Int (str, _) -> int_of_string_opt str
  | ___else___     -> None

let literal_of_string s =
  (* TODO: use proper token when possible? *)
  let tok = Parse_info.fake_info s in
  G.String(s, tok)

let eval_unop_bool op b =
  match op with
  | G.Not -> Lit (literal_of_bool (not b))
  | _else -> Cst Cbool

let eval_binop_bool op b1 b2 =
  match op with
  | G.Or  -> Lit (literal_of_bool (b1 || b2))
  | G.And -> Lit (literal_of_bool (b1 && b2))
  | _else -> Cst Cbool

let eval_unop_int op opt_i =
  match op, opt_i with
  | G.Plus,  Some i -> Lit (literal_of_int i)
  | G.Minus, Some i -> Lit (literal_of_int (-i))
  | ___else____     -> Cst Cint

let eval_binop_int op opt_i1 opt_i2 =
  match op, opt_i1, opt_i2 with
  (* TODO: Handle overflows and division by zero. *)
  | G.Plus,  Some i1, Some i2 -> Lit (literal_of_int (i1 + i2))
  | G.Minus, Some i1, Some i2 -> Lit (literal_of_int (i1 - i2))
  | G.Mult,  Some i1, Some i2 -> Lit (literal_of_int (i1 * i2))
  | G.Div,   Some i1, Some i2 -> Lit (literal_of_int (i1 / i2))
  | ___else____     -> Cst Cint

let eval_binop_string op s1 s2 =
  match op with
  | G.Plus
  | G.Concat -> Lit (literal_of_string (s1 ^ s2))
  | __else__ -> Cst Cstr

let rec eval (env :constness D.env) exp : constness =
  match exp.e with
  | Lvalue lval -> eval_lval env lval
  | Literal li -> Lit li
  | Operator (op, args) -> eval_op env op args
  | Composite _
  | Record _
  | Cast _
  | TodoExp _
    -> Dyn

and eval_lval env lval =
  match lval with
  | { base=Var x; offset=NoOffset; constness=_; } ->
      let opt_c = D.VarMap.find_opt (str_of_name x) env in
      opt_c ||| Dyn
  | ___else___ ->
      Dyn

and eval_op env op args =
  let cs = List.map (eval env) args in
  match fst op, cs with
  | G.Plus, [c1]             -> c1
  | op,     [Lit (G.Bool (b, _))] -> eval_unop_bool op b
  | op,     [Lit (G.Int _ as li)] -> eval_unop_int op (int_of_literal li)
  | op,     [Lit (G.Bool (b1, _)); Lit (G.Bool (b2, _))] ->
      eval_binop_bool op b1 b2
  | op,     [Lit (G.Int _ as li1); Lit (G.Int _ as li2)] ->
      eval_binop_int op (int_of_literal li1) (int_of_literal li2)
  | op,     [Lit (G.String (s1, _)); Lit (G.String (s2, _))] ->
      eval_binop_string op s1 s2
  | _op,    [Cst _ as c1]    -> c1
  | _op,    [Cst t1; Cst t2] -> Cst (union_ctype t1 t2)
  | _op,    [Lit l1; Cst t2]
  | _op,    [Cst t2; Lit l1] ->
      let t1 = ctype_of_literal l1 in
      Cst (union_ctype t1 t2)
  | ___else___ -> Dyn

(*****************************************************************************)
(* Transfer *)
(*****************************************************************************)

let literal_of_ctype = function
  (* TODO: This is just a hack so that Semgrep's "..." can match it. *)
  | Cstr  -> Some (literal_of_string "...")
  | _else -> None

let literal_of_constness = function
  | Lit l -> Some l
  | Cst t -> literal_of_ctype t
  | Dyn   -> None

let union_env =
  Dataflow.varmap_union union

let (transfer: flow:F.cfg -> constness Dataflow.transfn) =
  fun ~flow ->
  (* the transfer function to update the mapping at node index ni *)
  fun mapping ni ->

  let in' =
    (flow#predecessors ni)#fold (fun acc (ni_pred, _) ->
      union_env acc mapping.(ni_pred).D.out_env
    ) VarMap.empty
  in
  let node = flow#nodes#assoc ni in

  (* Set the constness of variables in the RHS according to in'. *)
  lvals_of_node node.n
  |> List.iter (function
    | {base = Var var; constness; _} ->
        (match D.VarMap.find_opt (str_of_name var) in' with
         | None   -> constness := None
         | Some c -> constness := literal_of_constness c
        )
    | ___else___ -> ()
  );

  (* Compute output mapping. *)
  let out' =
    match node.F.n with
    | Enter | Exit | TrueNode | FalseNode | Join
    | NCond _ | NGoto _ | NReturn _ | NThrow _ | NOther _
    | NTodo _
      -> in'
    | NInstr instr ->
        match instr.i with
        (* TODO: Handle base=Mem e case. *)
        | Assign ({base=Var var; offset=NoOffset; constness;}, exp) ->
            let cexp = eval in' exp in
            constness := literal_of_constness cexp;
            D.VarMap.add (str_of_name var) cexp in'
        | ___else___ ->
            let lvar_opt = IL.lvar_of_instr_opt instr in
            match lvar_opt with
            | None      -> in'
            | Some lvar -> D.VarMap.add (str_of_name lvar) Dyn in'
  in

  {D. in_env = in'; out_env = out'}

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let (fixpoint: F.cfg -> mapping) = fun flow ->
  DataflowX.fixpoint
    ~eq
    ~init:(DataflowX.new_node_array flow (Dataflow.empty_inout ()))
    ~trans:(transfer ~flow)
    (* constness is a forward analysis! *)
    ~forward:true
    ~flow

let propagate_constants ast =
  (* TODO: Handle constants defined in higher scopes. *)
  let module V = Visitor_AST in
  let v = V.mk_visitor
      { V.default_visitor with
        V.kfunction_definition = (fun (_k, _) def ->
          let xs = AST_to_IL.stmt def.G.fbody in
          let flow = CFG_build.cfg_of_stmts xs in
          let mapping = fixpoint flow in
          ignore(mapping)
        );
      } in
  v (G.Pr ast)
