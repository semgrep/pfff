(* Yoann Padioleau
 *
 * Copyright (C) 2010-2012 Facebook
 * Copyright (C) 2019 r2c
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * A driver for our different checkers:
 * 
 * todo:
 *  - use/def of global entities (functions, classes)
 *  - use/def of local variables
 *  - function/method call arity
 *  - dataflow based useless assignments
 *  - type checker (e.g. wrong type of argument, expr is not a bool,
 *    use of array instead of scalar, etc)
 *  - record checker (fields)
 *  - protocol checker, statistical analysis a la Engler
 *  - ...
 *)

(*****************************************************************************)
(* Main entry points *)
(*****************************************************************************)

let check_file ?(verbose=true) ?(find_entity=None) ast =

 Common.save_excursion Flag_linter.verbose_checking verbose (fun() ->

  (* todo? some unsugaring? *)

  (* even if find_entity=None, check_and_annotate_program can find
   * interesting bugs on local variables. There will be false positives
   * but it's better than nothing.
   *)
  (* Check_variables_php.check_and_annotate_program find_entity ast; *)

  Check_cfg_generic.check_program ast;

(*
  (* not ready yet: Check_dfg_php.check_program ?find_entity ast; *)
  Check_micro_clones_php.check ast;
*)
  (* work only when have a find_entity; requires a global view of the code *)
  find_entity |> Common.do_option (fun _find_entity ->
(*
    Check_functions_php.check_program find_entity ast;
    Check_classes_php.check_program   find_entity ast;
*)
    (* could have a Check_typedefs_php.check_program but hack will
     * already check the important things so no point doing redundant
     * checks.
     *)
    ()
  );
  ()
 )
