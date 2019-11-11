open Ast_generic

let test_cfg_generic file =
  let ast = Parse_generic.parse_program file in
  ast |> List.iter (fun item ->
   (match item with
   | IDef (_ent, FuncDef def) ->
     (try 
       let flow = Controlflow_build.cfg_of_func def in
       Controlflow.display_flow flow;
      with Controlflow_build.Error err ->
         Controlflow_build.report_error err
      )
    | _ -> ()
   )
 )


let actions () = [
  "-cfg_generic", " <file>",
  Common.mk_action_1_arg test_cfg_generic;
]
