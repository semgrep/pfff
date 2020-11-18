(* generated by ocamltarzan with: camlp4o -o /tmp/yyy.ml -I pa/ pa_type_conv.cmo pa_vof.cmo  pr_o.cmo /tmp/xxx.ml  *)

open AST_generic


let vof_arithmetic_operator =
  function
  | Elvis -> OCaml.VSum (("Elvis", []))
  | NotNullPostfix -> OCaml.VSum (("NotNullPostfix", []))
  | Nullish -> OCaml.VSum (("Nullish", []))
  | Range -> OCaml.VSum (("Range", []))
  | RegexpMatch -> OCaml.VSum (("RegexpMatch", []))
  | NotMatch -> OCaml.VSum (("NotMatch", []))
  | Concat -> OCaml.VSum (("Concat", []))
  | Append -> OCaml.VSum (("Append", []))
  | Plus -> OCaml.VSum (("Plus", []))
  | Minus -> OCaml.VSum (("Minus", []))
  | Mult -> OCaml.VSum (("Mult", []))
  | Div -> OCaml.VSum (("Div", []))
  | Mod -> OCaml.VSum (("Mod", []))
  | Pow -> OCaml.VSum (("Pow", []))
  | FloorDiv -> OCaml.VSum (("FloorDiv", []))
  | MatMult -> OCaml.VSum (("MatMult", []))
  | LSL -> OCaml.VSum (("LSL", []))
  | LSR -> OCaml.VSum (("LSR", []))
  | ASR -> OCaml.VSum (("ASR", []))
  | BitOr -> OCaml.VSum (("BitOr", []))
  | BitXor -> OCaml.VSum (("BitXor", []))
  | BitAnd -> OCaml.VSum (("BitAnd", []))
  | BitNot -> OCaml.VSum (("BitNot", []))
  | BitClear -> OCaml.VSum (("BitClear", []))
  | And -> OCaml.VSum (("And", []))
  | Or -> OCaml.VSum (("Or", []))
  | Xor -> OCaml.VSum (("Xor", []))
  | Not -> OCaml.VSum (("Not", []))
  | Eq -> OCaml.VSum (("Eq", []))
  | NotEq -> OCaml.VSum (("NotEq", []))
  | PhysEq -> OCaml.VSum (("PhysEq", []))
  | NotPhysEq -> OCaml.VSum (("NotPhysEq", []))
  | Lt -> OCaml.VSum (("Lt", []))
  | LtE -> OCaml.VSum (("LtE", []))
  | Gt -> OCaml.VSum (("Gt", []))
  | GtE -> OCaml.VSum (("GtE", []))
  | Cmp -> OCaml.VSum (("Cmp", []))

let vof_incr_decr =
  function
  | Incr -> OCaml.VSum (("Incr", []))
  | Decr -> OCaml.VSum (("Decr", []))

let vof_prepost =
  function
  | Prefix -> OCaml.VSum (("Prefix", []))
  | Postfix -> OCaml.VSum (("Postfix", []))

let vof_inc_dec (v1, v2) =
      let v1 = vof_incr_decr v1
      and v2 = vof_prepost v2
      in OCaml.VTuple [ v1; v2 ]

