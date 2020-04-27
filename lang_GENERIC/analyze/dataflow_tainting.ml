(* Yoann Padioleau
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
open Il

module F = Il
module D = Dataflow
module VarMap = Dataflow.VarMap

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Tainting dataflow analysis.
 *
 * This is a very rudimentary tainting analysis. Just intraprocedural,
 * very coarse grained (taint whole array/object).
 * This is step1 for taint tracking support in semgrep.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type mapping = unit Dataflow.mapping

type config = {
  is_source: Il.instr -> bool;
  is_sink: Il.instr -> bool;
  is_sanitizer: Il.instr -> bool;

  found_tainted_sink: Il.instr -> unit Dataflow.env -> unit;
}

module DataflowX = Dataflow.Make (struct
  type node = F.node
  type edge = F.edge
  type flow = (node, edge) Ograph_extended.ograph_mutable
  let short_string_of_node n = 
        Meta_il.short_string_of_node_kind n.F.n
end)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let str_of_name ((s, _tok), sid) =
    spf "%s:%d" s sid

let option_to_varmap = function
  | None -> VarMap.empty
  | Some lvar -> VarMap.singleton (str_of_name lvar) ()

(*****************************************************************************)
(* Transfer *)
(*****************************************************************************)
(* Not sure we can use the Gen/Kill framework here.
 *)

let union = 
  Dataflow.varmap_union (fun () () -> ())
let diff =
  Dataflow.varmap_diff (fun () () -> ()) (fun () -> true)

let (transfer: config -> flow:F.cfg -> unit Dataflow.transfn) =
 fun config ~flow ->
  (* the transfer function to update the mapping at node index ni *)
  fun mapping ni ->

  let in' = 
    (flow#predecessors ni)#fold (fun acc (ni_pred, _) ->
       union acc mapping.(ni_pred).D.out_env
     ) VarMap.empty 
  in
  let node = flow#nodes#assoc ni in

  (* TODO: do that later? once everything if finished? *)
  (match node.F.n with
  | NInstr x ->
    if config.is_sink x
    then begin
       (* TODO: use metavar in sink to know which argument we should check
        * for taint?
        *)
       let rvars = Il.rvars_of_instr x in
       if rvars |> List.exists (fun rvar -> VarMap.mem (str_of_name rvar) in')
       then config.found_tainted_sink x in'
    end
  | Enter | Exit | TrueNode | FalseNode | Join
  | NCond _| NReturn _ | NThrow _ | NOther _ -> ()
  );


  let gen_ni_opt = 
    match node.F.n with
    | NInstr x ->
       (match x.i with
       | Call (Some ({base=Var lvar; _}), 
                    {e=Lvalue({base=Var(("source",_),_);_}); _}, [])->
          Some lvar
      (* this can use semgrep patterns under the hood to find source
       * functions instead of the hardcoded source() above.
       *)
       | _ when config.is_source x ->
           Il.lvar_of_instr_opt x
       | _ ->
           let lvar_opt = Il.lvar_of_instr_opt x in
           let rvars = Il.rvars_of_instr x in
           (match lvar_opt with
           | None -> None
           | Some lvar ->
               (* one taint argument propagate the taint to the lvar *)
               if rvars |> List.exists (fun rvar -> 
                                VarMap.mem (str_of_name rvar) in')
               then Some lvar
               else None
            )
       )

    | Enter | Exit | TrueNode | FalseNode | Join
    | NCond _| NReturn _ | NThrow _ | NOther _ -> None
  in
  let kill_ni_opt = 
    (* if there was a source(), no need to look for a sanitize() given
     * an instr can be at most one call?
     * old:
     *  if gen_ni_opt <> None
     *  then None
     * but now gen_ni <> None does not necessarily mean we had a source().
     * It can also be one tainted rvars which propagate to the lvar
     *)
    match node.F.n with
    | NInstr x ->
       (match x.i with
       | Call (Some ({base=Var _lvar; _}), 
                    {e=Lvalue({base=Var(("source",_),_);_}); _}, [])->
          None
       | _ when config.is_source x -> None

       | Call (Some ({base=Var lvar; _}), 
               {e=Lvalue({base=Var(("sanitize",_),_);_}); _}, [])->
          Some lvar
       | _ when config.is_sanitizer x ->
           Il.lvar_of_instr_opt x
       | _ ->
           let lvar_opt = Il.lvar_of_instr_opt x in
           let rvars = Il.rvars_of_instr x in
           (match lvar_opt with
           | None -> None
           | Some lvar ->
               (* all clean arguments should reset the taint *)
               if rvars |> List.for_all (fun rvar -> 
                                not (VarMap.mem (str_of_name rvar) in'))
               then Some lvar
               else None
            )
       )
    | Enter | Exit | TrueNode | FalseNode | Join
    | NCond _| NReturn _ | NThrow _ | NOther _ -> None
  in
  let gen_ni = option_to_varmap gen_ni_opt in
  let kill_ni = option_to_varmap kill_ni_opt in

  let out' = diff (union in' gen_ni) kill_ni in
  {D. in_env = in'; out_env = out'}


(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let (fixpoint: config -> F.cfg -> mapping) = fun config flow ->
  DataflowX.fixpoint
    ~eq:(fun () () -> true)
    ~init:(DataflowX.new_node_array flow (Dataflow.empty_inout ()))
    ~trans:(transfer config ~flow)
    (* tainting is a forward analysis! *)
    ~forward:true
    ~flow
