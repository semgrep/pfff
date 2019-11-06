open Common

let parse_with_lang lang file = 
  match lang with
  | Lang.Javascript ->
    let cst = Parse_js.parse_program file in
    let ast = Ast_js_build.program cst in
    Js_to_generic.program ast
  | Lang.Python ->
    let ast = Parse_python.parse_program file in
    Resolve_python.resolve ast;
    Python_to_generic.program ast
  | Lang.C ->
    let ast = Parse_c.parse_program file in
    C_to_generic.program ast
  | Lang.Java ->
    raise Todo
  | Lang.ML ->
    raise Todo

let parse_program file =
  match Lang.lang_of_filename_opt file with
  | Some x -> parse_with_lang x file
  | None -> failwith (spf "unsupported file for AST generic: %s" file)


let parse_pattern lang str =
  Common.save_excursion Flag_parsing.sgrep_mode true (fun () ->
  match lang with
  | Lang.Python ->
      let any = Parse_python.any_of_string str in
      Python_to_generic.any any
  | Lang.Javascript ->
      let any_cst = Parse_js.any_of_string str in
      let any = Ast_js_build.any any_cst in
      Js_to_generic.any any
  | Lang.C 
  | Lang.Java
  | Lang.ML
   -> raise Todo
  )

