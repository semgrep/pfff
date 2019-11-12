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

let test_dfg_generic file =
  let ast = Parse_generic.parse_program file in
  ast |> List.iter (fun item ->
   (match item with
   | IDef (_ent, FuncDef def) ->
      let flow = Controlflow_build.cfg_of_func def in
      let mapping = Dataflow_reaching.reaching_fixpoint flow in
      Dataflow.display_mapping flow mapping Dataflow.ns_to_str;
      Dataflow_reaching.display_reaching_dflow flow mapping;
    | _ -> ()
   )
 )



let actions () = [
  "-cfg_generic", " <file>",
  Common.mk_action_1_arg test_cfg_generic;
  "-dfg_generic", " <file>",
  Common.mk_action_1_arg test_dfg_generic;
]
