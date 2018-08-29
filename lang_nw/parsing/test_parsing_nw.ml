open Common

module Ast = Ast_nw
module Flag = Flag_parsing_nw

(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

let test_tokens_nw file = 
  if not (file =~ ".*\\.nw") 
  then pr2 "warning: seems not a noweb file";

  Flag.verbose_lexing := true;
  Flag.verbose_parsing := true;

  let toks = Parse_nw.tokens file in
  toks +> List.iter (fun x -> pr2_gen x);
  ()

let test_parse_nw file =
  Parse_nw.parse file |> ignore

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () = [
  "-tokens_nw", "   <file>", 
  Common.mk_action_1_arg test_tokens_nw;
  "-parse_nw", "   <file>", 
  Common.mk_action_1_arg test_parse_nw;
]
