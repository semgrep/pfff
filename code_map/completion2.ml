(*s: completion2.ml *)
(*s: Facebook copyright *)
(* Yoann Padioleau
 * 
 * Copyright (C) 2010-2012 Facebook
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
(*e: Facebook copyright *)
open Common

module G = Gui
module E = Entity_code
module Db = Database_code
module BG = Big_grep
module Flag = Flag_visual

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Gtk (and lablgtk) is very fragile; if you change what looks like
 * an innocent line, you might suddenly get performance regressions
 * or new Gtk warnings at runtime. So take care when changing this file.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(*  *)
(*****************************************************************************)

(*s: build_completion_defs_index *)
(* I was previously using a prefix-clustering optimisation but it
 * does not allow suffix search. Moreover it was still slow so
 * Big_grep is just simpler and better.
 *)

let build_completion_defs_index all_entities = 
  BG.build_index all_entities
(*e: build_completion_defs_index *)


(*****************************************************************************)
(* Model *)
(*****************************************************************************)

let icon_of_kind kind has_test =
  match kind with
  | E.Function -> 
      if has_test then `YES else `NO
  
  (* less: do different symbols for unit tested class and methods ? 
   * or add another column in completion popup
   * todo? class vs interface ?
   *)
  | E.Class -> `CONNECT
  | E.Module -> `DISCONNECT
  | E.Package -> `DIRECTORY
  | E.Type -> `PROPERTIES
  | E.Constant -> `CONNECT
  | E.Global -> `MEDIA_RECORD
  | E.Method -> `CONVERT

  | E.File -> `FILE
  | E.Dir -> `DIRECTORY
  | E.MultiDirs -> `QUIT

  (* todo *)
  | E.ClassConstant -> `CONNECT
  | E.Field -> `CONNECT
  | E.Macro -> `CONNECT
  | E.Exception -> `CONNECT
  | E.Constructor -> `CONNECT
  | E.Prototype -> `CONNECT
  | E.GlobalExtern -> `CONNECT

  | (E.TopStmts | E.Other _ ) -> raise Todo

(* less: could get rid of, just used in model_of_list_pair_string_with_icon *)
type t = { 
  mutable id: int;
  mutable entity: Database_code.entity;
  mutable text: string; 
  mutable file: string; 
  mutable count: string;
  mutable kind: string;
  mutable icon: GtkStock.id;
}

let column_list = new GTree.column_list
(* old:
 *  let col_full = (column_list#add Gobject.Data.caml: t GTree.column)
 * this works only with a custom list model :( but those models seem to not
 * work anymore inside completion boxes :( so instead I use a col_id
 * and a store_id_to_entity hash to get equivalent functionality.
 *)
let col_id   = column_list#add Gobject.Data.int
let col_text = column_list#add Gobject.Data.string
let col_file = column_list#add Gobject.Data.string
let col_count = column_list#add Gobject.Data.string
let _col_kind = column_list#add Gobject.Data.string
let col_icon = column_list#add GtkStock.conv


let model_of_list_pair_string_with_icon2 xs =

  (* old: I was using originally a custom_list 
   * (see custom_list_generic.ml in lablgtk2/examples)
   * to optimize things because GTree.tree_store used to be really slow.
   * However, I am now unable to make it work with recent gtk/lablgtk2
   * (I get some failure assertions regarding GtkModelFilter)
   * so I reverted back to using the simpler tree_store.
   *)
  let model = GTree.tree_store column_list in
  let store_id_to_entity = Hashtbl.create 101 in

  pr2 (spf "Size of model = %d" (List.length xs));
  xs |> Common2.index_list_0 |> List.iter (fun (e, id) ->
    let kind = e.Db.e_kind in
    
    let has_unit_test =
      List.length e.Db.e_good_examples_of_use >= 1
    in
    let name = e.Db.e_name in
    (* if the string is too long, we will not see the other properties *)
    let final_name = 
      try (String.sub name 0 30) ^ "..."
      with Invalid_argument _ -> name
    in
    let t = { 
      id = id;
      entity = e;                     

      (* had originally an ugly hack where we would artificially create
       * a text2 field with always set to query. Indeed
       * gtk seems to be confused if the column referenced
       * by set_text_column contains a string that is not matching
       * the current query. So here we were building this fake text entry.
       * In fact as explained on the pygtk entry of entry_completion
       * you don't have to use set_text_column if you provide
       * your own set_match_func, which we do.
       * Maybe we should just not use Entrycompletion at all and build
       * our own popup.
       *)
      text = final_name;


      file = e.Db.e_file;
      count = i_to_s (e.Db.e_number_external_users);
      kind = E.string_of_entity_kind kind;
      icon = icon_of_kind kind has_unit_test;
    }
    in 
    (* custom_list#insert *)
    let row = model#append () in

    model#set ~row ~column:col_id t.id;
    model#set ~row ~column:col_text t.text;
    model#set ~row ~column:col_count t.count;
    model#set ~row ~column:col_file t.file;
    model#set ~row ~column:col_icon t.icon;

    Hashtbl.add store_id_to_entity id t.entity;
  );
  model, store_id_to_entity


let model_of_list_pair_string_with_icon a =
  Common.profile_code "Completion2.model_of_list" (fun () ->
    model_of_list_pair_string_with_icon2 a
  )

let model_col_of_prefix prefix_or_suffix idx =
  let xs = 
    BG.top_n_search 
      ~top_n:!Flag.top_n
      ~query:prefix_or_suffix 
      idx
  in
  model_of_list_pair_string_with_icon xs

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let add_renderer (completion : GEdit.entry_completion) =
  
  let renderer = GTree.cell_renderer_pixbuf [ `STOCK_SIZE `LARGE_TOOLBAR ] in
  completion#pack (renderer :> GTree.cell_renderer);
  completion#add_attribute (renderer :> GTree.cell_renderer) 
    "stock_id" col_icon;

  let renderer = GTree.cell_renderer_text [] in
  completion#pack (renderer :> GTree.cell_renderer);
  completion#add_attribute (renderer :> GTree.cell_renderer) 
    "text" col_text;

  let renderer = GTree.cell_renderer_text [`WIDTH 80] in
  completion#pack (renderer :> GTree.cell_renderer);
  completion#add_attribute (renderer :> GTree.cell_renderer) 
    "text" col_count;

  let renderer = GTree.cell_renderer_text [] in
  completion#pack (renderer :> GTree.cell_renderer);
  completion#add_attribute (renderer :> GTree.cell_renderer) 
    "text" col_file;
  ()

let fake_entity = {Database_code.
     e_name = "foobar";
     e_fullname = "";
     e_file = "foo.php";
     e_kind = E.Function;
     e_pos = { Common2.l = -1; c = -1 };
     e_number_external_users = 0;
     e_good_examples_of_use = [];
     e_properties = [];
}

let my_entry_completion_eff2 ~callback_selected ~callback_changed fn_idx = 
  
  let entry = GEdit.entry ~width:1100 () in
  let completion = GEdit.entry_completion () in
  entry#set_completion completion;

  let xs = [ fake_entity ] in
  let model_dumb, h_dumb = model_of_list_pair_string_with_icon xs in
  let model = ref model_dumb in
  let store_id_to_entity = ref h_dumb in

  add_renderer completion;
  completion#set_model (!model :> GTree.model);

  (* we don't use the builtin gtk completion mechanism because we
   * recompute the model each time using big_grep so here
   * we just always return true. Moreover, the builtin gtk
   * function would do a is_prefix check between the row
   * and the current query which in our case would fail because
   * we want the suffix-search ability of big_grep.
   *)
  completion#set_match_func (fun _key _row ->
    true
  );
  completion#set_minimum_key_length 2;

  completion#connect#match_selected (fun _model_filter row ->
     (* note: the code below used to not work; the row was relative to the
      * model_filter.
      *  let str = !model#get ~row ~column:col1 in
      *  let file = !model#get ~row ~column:col2 in
      * update: I now get assetions failures when using model_filter so
      *  I am now using back again !model.
      * old: 
      *  let model = model_filter#child_model in
      *  let row = model_filter#convert_iter_to_child_iter row in
      * does not work anymore
      *)

      let str =  !model#get ~row ~column:col_text in
      let file = !model#get ~row ~column:col_file in

      (* old: 
       * let t =   model#get ~row ~column:col_full in
       * unfortunately this does seem to work only with custom list
       *)
      let id = !model#get ~row ~column:col_id in
      let entity = Hashtbl.find !store_id_to_entity id in

      pr2 str;
      pr2 file;
      callback_selected entry str file entity

  ) +> ignore;

  let current_timeout = ref None in

  entry#connect#changed (fun () -> 
    let s = entry#text in
    pr2 s;
    if s <> "" then begin
      !current_timeout +> Common.do_option (fun x ->
        GMain.Timeout.remove x;
      );
      current_timeout :=
        Some 
          (G.gmain_timeout_add ~ms:250
           ~callback:(fun _ -> 
            pr2 "changing model";
            let idx = fn_idx () in
            let (m,h) = model_col_of_prefix s idx in
            model := m;
            store_id_to_entity := h;
            completion#set_model (!model :> GTree.model);

            callback_changed s;
            (* bugfix: reset it otherwise get Glib errors *)
            current_timeout := None;
            false
          ));
    end
    else callback_changed s
  ) +> ignore;

  (* return the entry so someone can hook another signal *)
  entry
 
let my_entry_completion_eff ~callback_selected ~callback_changed x = 
  my_entry_completion_eff2 ~callback_selected ~callback_changed x

(*e: completion2.ml *)
