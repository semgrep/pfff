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
module HC = Highlight_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Gtk (and lablgtk) is very fragile; if you change what looks like
 * an innocent line, you might suddenly get performance regressions
 * or new Gtk warnings at runtime. So take care when changing this file.
 *
 * todo:
 *  - http://jeremymikkola.com/posts/2019_03_19_rules_for_autocomplete.html
 *)

(*****************************************************************************)
(* Code indexing *)
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
(* Icons *)
(*****************************************************************************)

(* Many IDEs use icons to represent entities:
 *  - https://docs.microsoft.com/en-us/visualstudio/ide/class-view-and-object-browser-icons?view=vs-2015
 *  - http://help.eclipse.org/kepler/index.jsp?topic=/org.eclipse.jdt.doc.user/reference/ref-icons.htm
 *  - https://www.jetbrains.com/help/idea/symbols.html
 *  - https://github.com/patrys/vscode-code-outline/tree/master/resources/light
 *    (just 8 icons, and not great one)
 *  - https://atom.io/packages/structure-view
 *    (very few icons too)
 *
 * I originally used icons to reprensent entities, first from Gtk Stock icons
 * (ugly) and later from Eclipse, but in the end those icons were not
 * super readable not universal, and were mostly for OO languages.
 * Using the color scheme of info_of_entity_kind_and_usedef2 is simpler
 * and better.
 * If you want to bring back icons, look at the history of the file to
 * get back the relevant code.
 *)

(*****************************************************************************)
(* Colors *)
(*****************************************************************************)

let color_of_entity_kind kind =
  (* less: could use count to even have color depending on count *)
  let use = HC.SomeUse in
  let props = HC.info_of_entity_kind_and_usedef2 kind (HC.Def2 use) in
  let color = 
    try props |> Common.find_some (function
        | `FOREGROUND s -> Some s
        | _ -> None
      )
    with Not_found -> 
        failwith (spf "could not find a FOREGROUND color for entity %s"
          (Entity_code.string_of_entity_kind kind))
  in
  color

(*****************************************************************************)
(* Model *)
(*****************************************************************************)

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
(* To colorize the foreground of col_text.
 * less: could also colorize the background and could also colorize
 * col_file (using dircolors.ml color coding)
 *)
let col_color_fg = column_list#add Gobject.Data.string
let col_color_bg = column_list#add Gobject.Data.string


let model_of_list_pair_string_with_icon2 xs =

  (* old: I was using before a custom_list 
   * (see custom_list_generic.ml in lablgtk2/examples)
   * to be faster because GTree.tree_store used to be really slow.
   * However, I am now unable to make it work with recent gtk/lablgtk2
   * (I get some failure assertions regarding GtkModelFilter)
   * so I reverted back to using the simpler tree_store which in 2019
   * seems ok.
   *)
  let model = GTree.tree_store column_list in
  let store_id_to_entity = Hashtbl.create 101 in

  pr2 (spf "Size of model = %d" (List.length xs));
  xs |> Common2.index_list_0 |> List.iter (fun (e, id) ->
   
    let _has_unit_test =
      List.length e.Db.e_good_examples_of_use >= 1
    in
    let name = e.Db.e_name in

    (* if the string is too long, we will not see the other properties *)
    let final_name = 
      try (String.sub name 0 30) ^ "..."
      with Invalid_argument _ -> name
    in
    let color = color_of_entity_kind e.Db.e_kind in

    let row = model#append () in

    model#set ~row ~column:col_id id;
    (* I had originally an ugly hack where I would artificially create
     * a text2 field with always set to query. Indeed
     * gtk seems to be confused if the column referenced
     * by set_text_column contains a string that is not matching
     * the current query. So here I was building this fake text entry.
     * In fact as explained on the pygtk entry of entry_completion
     * you don't have to use set_text_column if you provide
     * your own set_match_func, which I do now.
     * Maybe we should just not use Entrycompletion at all and build
     * our own popup.
     *)
    model#set ~row ~column:col_text final_name;
    model#set ~row ~column:col_count (i_to_s (e.Db.e_number_external_users));
    model#set ~row ~column:col_file e.Db.e_file;
    model#set ~row ~column:col_color_fg color;
    model#set ~row ~column:col_color_bg "DarkSlateGray";

    Hashtbl.add store_id_to_entity id e;
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
  
  let renderer = GTree.cell_renderer_text [] in
  completion#pack (renderer :> GTree.cell_renderer);
  completion#add_attribute (renderer :> GTree.cell_renderer) 
    "text" col_text;
  completion#add_attribute (renderer :> GTree.cell_renderer) 
    "foreground" col_color_fg;
  completion#add_attribute (renderer :> GTree.cell_renderer) 
    "background" col_color_bg;

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

  ) |> ignore;

  let current_timeout = ref None in

  entry#connect#changed (fun () -> 
    let s = entry#text in
    pr2 s;
    if s <> "" then begin
      !current_timeout |> Common.do_option (fun x ->
        GMain.Timeout.remove x;
      );
      current_timeout :=
        Some 
          (G.gmain_timeout_add ~ms:250
           ~callback:(fun _ -> 
            (* bugfix: reset it otherwise get Glib errors *)
            current_timeout := None;
            pr2 "changing model";
            let idx = fn_idx () in
            let (m,h) = model_col_of_prefix s idx in
            model := m;
            store_id_to_entity := h;
            completion#set_model (!model :> GTree.model);

            callback_changed s;
            false
          ));
    end
    else callback_changed s
  ) |> ignore;

  (* return the entry so someone can hook another signal *)
  entry
 
let my_entry_completion_eff ~callback_selected ~callback_changed x = 
  my_entry_completion_eff2 ~callback_selected ~callback_changed x

(*e: completion2.ml *)
