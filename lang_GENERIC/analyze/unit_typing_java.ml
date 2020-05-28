open Common
open OUnit
module V = Visitor_AST
module A = AST_generic

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

let unittest =
  "typing_java" >::: [

    "regression files" >:: (fun () ->
      let file = Filename.concat Config_pfff.path "/tests/GENERIC/typing/VarDef.java" in
        try
          let ast = Parse_generic.parse_program file in
            let lang = List.hd (Lang.langs_of_filename file) in 
             Naming_AST.resolve lang ast;

            let v = V.mk_visitor { V.default_visitor with
                V.kexpr = (fun (_k, _) exp ->
                    match exp with
                      | A.Id(_, {A.id_type=id_type; _}) -> (
                            match !id_type with 
                              | Some(A.TyName((("String", _)), _)) -> ()
                              | _ -> assert_failure("Variable referenced did not have expected type String"))
                      | _ -> ()
                );
            } in
            v (A.Pr ast)
        with Parse_info.Parsing_error _ ->
          assert_failure (spf "it should correctly parse %s" file)
    );

  ]
