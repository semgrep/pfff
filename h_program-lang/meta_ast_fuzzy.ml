open Ast_fuzzy

let vof_token t =
  Ocaml.VString (Parse_info.str_of_info t)
  (* Parse_info.vof_token t*)

let rec vof_multi_grouped =
  function
  | Braces ((v1, v2, v3)) ->
      let v1 = vof_token v1
      and v2 = Ocaml.vof_list vof_multi_grouped v2
      and v3 = vof_token v3
      in Ocaml.VSum (("Braces", [ v1; v2; v3 ]))
  | Parens ((v1, v2, v3)) ->
      let v1 = vof_token v1
      and v2 = Ocaml.vof_list (Ocaml.vof_either vof_trees vof_token) v2
      and v3 = vof_token v3
      in Ocaml.VSum (("Parens", [ v1; v2; v3 ]))
  | Angle ((v1, v2, v3)) ->
      let v1 = vof_token v1
      and v2 = Ocaml.vof_list vof_multi_grouped v2
      and v3 = vof_token v3
      in Ocaml.VSum (("Angle", [ v1; v2; v3 ]))
  | Bracket ((v1, v2, v3)) ->
      let v1 = vof_token v1
      and v2 = Ocaml.vof_list vof_multi_grouped v2
      and v3 = vof_token v3
      in Ocaml.VSum (("Bracket", [ v1; v2; v3 ]))
  | Metavar v1 -> let v1 = vof_wrap v1 in Ocaml.VSum (("Metavar", [ v1 ]))
  | Dots v1 -> let v1 = vof_token v1 in Ocaml.VSum (("Dots", [ v1 ]))
  | Tok v1 -> let v1 = vof_wrap v1 in Ocaml.VSum (("Tok", [ v1 ]))
and vof_wrap (s, _x) = Ocaml.VString s
and vof_trees xs =
  Ocaml.VList (xs |> List.map vof_multi_grouped)
