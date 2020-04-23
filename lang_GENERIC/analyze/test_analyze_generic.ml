open Common
open Ast_generic
module V = Visitor_ast

let test_cfg_generic file =
  let ast = Parse_generic.parse_program file in
  ast |> List.iter (fun item ->
   (match item with
   | DefStmt (_ent, FuncDef def) ->
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
   | DefStmt (_ent, FuncDef def) ->
      let flow = Controlflow_build.cfg_of_func def in
      pr2 "Reaching definitions";
      let mapping = Dataflow_reaching.fixpoint flow in
      Dataflow.display_mapping flow mapping Dataflow.ns_to_str;
      pr2 "Liveness";
      let mapping = Dataflow_liveness.fixpoint flow in
      Dataflow.display_mapping flow mapping (fun () -> "()");

    | _ -> ()
   )
 )

let test_naming_generic file =
  let ast = Parse_generic.parse_program file in
  let lang = List.hd (Lang.langs_of_filename file) in
  Naming_ast.resolve lang ast;
  let v = Meta_ast.vof_any (Ast_generic.Pr ast) in
  let s = Ocaml.string_of_v v in
  pr2 s

let test_il_generic file =
  let ast = Parse_generic.parse_program file in
  let lang = List.hd (Lang.langs_of_filename file) in
  Naming_ast.resolve lang ast;

  let v = V.mk_visitor { V.default_visitor with
      V.kfunction_definition = (fun (_k, _) def ->
          let s = Meta_ast.vof_any (S def.fbody) |> Ocaml.string_of_v in
          pr2 s;
          pr2 "==>";

          let xs = Ast_to_il.stmt def.fbody in
          let v = Meta_il.vof_any (Il.Ss xs) in
          let s = Ocaml.string_of_v v in
          pr2 s
      );
   } in
  v (Pr ast)

let test_cfg_il file =
  let ast = Parse_generic.parse_program file in
  let lang = List.hd (Lang.langs_of_filename file) in
  Naming_ast.resolve lang ast;
  
  ast |> List.iter (fun item ->
   (match item with
   | DefStmt (_ent, FuncDef def) ->
     let xs = Ast_to_il.stmt def.fbody in
     let cfg = Ilflow_build.cfg_of_stmts xs in
     Meta_il.display_cfg cfg;
    | _ -> ()
   )
 )


let actions () = [
  "-cfg_generic", " <file>",
  Common.mk_action_1_arg test_cfg_generic;
  "-dfg_generic", " <file>",
  Common.mk_action_1_arg test_dfg_generic;
  "-naming_generic", " <file>",
  Common.mk_action_1_arg test_naming_generic;
  "-il_generic", " <file>",
  Common.mk_action_1_arg test_il_generic;
  "-cfg_il", " <file>",
  Common.mk_action_1_arg test_cfg_il;
]
