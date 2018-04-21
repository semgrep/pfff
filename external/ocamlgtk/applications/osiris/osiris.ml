(**************************************************************************)
(*     Lablgtk - Applications                                             *)
(*                                                                        *)
(*    * You are free to do anything you want with this code as long       *)
(*      as it is for personal use.                                        *)
(*                                                                        *)
(*    * Redistribution can only be "as is".  Binary distribution          *)
(*      and bug fixes are allowed, but you cannot extensively             *)
(*      modify the code without asking the authors.                       *)
(*                                                                        *)
(*    The authors may choose to remove any of the above                   *)
(*    restrictions on a per request basis.                                *)
(*                                                                        *)
(*    Authors:                                                            *)
(*      Jacques Garrigue <garrigue@kurims.kyoto-u.ac.jp>                  *)
(*      Benjamin Monate  <Benjamin.Monate@free.fr>                        *)
(*      Olivier Andrieu  <oandrieu@nerim.net>                             *)
(*      Jun Furuse       <Jun.Furuse@inria.fr>                            *)
(*      Hubert Fauque    <hubert.fauque@wanadoo.fr>                       *)
(*      Koji Kagawa      <kagawa@eng.kagawa-u.ac.jp>                      *)
(*                                                                        *)
(**************************************************************************)

type handle = Gtk.widget Gobject.obj
type menuhandle = Gtk.menu_shell Gobject.obj
type richcolor = Gdk.Color.t
type hres
type hresources

type richeffect =
    FE_AUTOCOLOR
  | FE_BOLD
  | FE_ITALIC
  | FE_STRIKEOUT
  | FE_UNDERLINE
  | FE_PROTECTED

type stringtype =
    ST_ALPHANUM
  | ST_SPACES
  | ST_OTHER

type show_window_state = Maximize | Minimize | Normal
type align = AlNone | AlLeft | AlRight | AlTop | AlBottom | AlClient | AlCenter
type border = BdNone | BdSingle | BdNormal

type question_result = QR_YES | QR_NO | QR_CANCEL

type sel = {
    mutable smin : int;
    mutable smax : int
}

type keys = {
    k_del : int;
    k_enter : int;
    k_esc : int;
    k_fun : int
}

type richformat = {
    rf_effects : richeffect list;
    rf_height : int option;
    rf_color : richcolor option;
    rf_fontface : string option;
}

type 'a tree = Empty | Tree of ('a * 'a tree list)

type event = (unit -> unit) -> unit
type event_bool = (unit -> bool) -> unit
type event_key = (int -> unit) -> unit

class type mouse_events =
  object
    method on_click : event
    method on_dblclick : event
    method on_rdblclick : event
    method on_mdblclick : event
    method on_ldown : event
    method on_rdown : event
    method on_mdown : event
    method on_lup : event
    method on_rup : event
    method on_mup : event
    method on_mousemove : (int -> int -> unit) -> unit
  end

class type key_events =
  object    
    method on_key_down : event_key
    method on_key_up : event_key
  end

class font w =
  object (self)
    val desc =
      Pango.Context.get_font_description
	(GtkBase.Widget.get_pango_context w)
    method private update =
      GtkBase.Widget.modify_font w desc
    method face s =
      Pango.Font.set_family desc s; self#update
    method size n =
      Pango.Font.set_size desc n; self#update
    method bold b =
      Pango.Font.set_weight desc (if b then `BOLD else `NORMAL);
      self#update
    method italic b =
      Pango.Font.set_style desc (if b then `ITALIC else `NORMAL);
      self#update
    (* underline : not on GTK *)
    (* color : GDI low-level *)
  end

class type virtual component =
  object
    method handle : handle
    method parent : container option
    method visible : bool -> unit
    method is_visible : bool
    method enable : bool -> unit
    method is_enabled : bool
    method caption : string -> unit
    method get_caption : string
    method x : int -> unit
    method get_x : int
    method y : int -> unit
    method get_y : int
    method width : int -> unit
    method get_width : int
    method height : int -> unit
    method get_height : int
    method align : align -> unit
    method get_align : align
    method virtual update : unit
    method component : component
    method destroy : unit
    method is_destroyed : bool
    method font : font
    method focus : unit

    method set_custom : (unit -> unit) -> unit
    method call_custom : unit
  end
and
 virtual container =
  object
    inherit component

    method span : int -> unit
    method space : int -> unit
    method container : container
  end

class type menu =
  object
    method handle : menuhandle
    method destroy : unit
    method popup : unit
  end

class type window =
  object
    inherit container
    inherit mouse_events
    inherit key_events

    method process : bool -> bool
    method update : unit
    method close : unit

    method state : show_window_state -> unit
    method get_state : show_window_state
    method on_top : bool -> unit

    method menu : menu  
    method on_closequery : event_bool
    method on_destroy : event
    method on_resize : event
    method on_move : event
  end

class type panel =
  object
    inherit container
    inherit mouse_events
    inherit key_events

    method update : unit
    method clear : unit
    method border : border -> unit
    method get_border : border
  end

class type button =
  object
    inherit component
    inherit mouse_events

    method update : unit    
  end

class type genlist =
  object
    inherit component
    inherit mouse_events
    inherit key_events

    method listprint : (unit -> string list) -> unit

    method redraw : unit
    method update : unit

    method selected : int option
    method is_selected : int -> bool
    method set_selected : int option -> unit

    method on_selchange : event
    method genlist : genlist
  end

class type listbox =
  object
    inherit genlist
    method multiselect : bool -> unit
    method is_multiselect : bool
    method top_index : int
    method set_top_index : int -> unit
  end

class type combobox =
  object
    inherit genlist
  end

class type ['a] litems =
  object
    method redraw : unit
    method list : (unit -> 'a list) -> unit
    method print : ('a -> string) -> unit
    method equal : ('a -> 'a -> bool) -> unit
    method sort : ('a -> 'a -> int) option -> unit
    method curlist : 'a list
    method selection : 'a list
    method selected : 'a option
    method is_selected : 'a -> bool
    method set_selected : 'a option -> unit
    method set_selection : 'a list -> unit  
  end

class type virtual toggle =
  object
    inherit component
    inherit mouse_events

    method state : bool -> unit
    method get_state : bool
    method on_statechange : event
    method update : unit
    method toggle : toggle
  end

class type radiobutton =
  object
    inherit toggle
  end

class type checkbox =
  object
    inherit toggle
  end

class type label =
  object
    inherit component
    inherit mouse_events
    inherit key_events

    method update : unit
  end

class type edit =
  object
    inherit component
    inherit mouse_events
    inherit key_events

    method on_change : event
    method undo : unit  
    method update : unit
  end

class type richedit =
  object
    inherit component

    method update : unit
    
    method tabsize : int -> unit
    method wordwrap : bool -> unit
    method selection : sel -> unit
    method get_selection : sel
    method get_tabsize : int
    method get_wordwrap : bool

    method line_from_char : int -> int
    method char_from_line : int -> int
    method set_format : bool -> richformat -> unit
    method get_format : bool -> richformat
    method get_text : sel -> string

    method colorize : (stringtype -> string -> int -> richcolor option) -> sel option -> unit

    method on_change : event

    method undo : unit
    method redo : unit
  end


class type resources =
  object
    method add_source : string -> unit

    method add_icon : string -> hres
    method add_bitmap : string -> hres

    method handle : hresources
  end

class type treeview =
  object
    inherit component
    inherit mouse_events
    inherit key_events

    method update : unit
    method redraw : unit

    method treeprint : (unit -> (string*hres option) tree) -> unit

    method icons : resources option
    method set_icons : resources option -> unit
    method on_selchange : event

    method selected : int option
    method set_selected : int option -> unit
    method hittest : int -> int -> int option

  end

let bts x =
  let x = if x < 0 then 0 else if x > 255 then 255 else x in
  x * 257

let make_richcolor ~red ~green ~blue =
  let colormap = Gdk.Color.get_system_colormap () in
  Gdk.Color.alloc ~colormap (`RGB(bts red, bts green, bts blue))

open GdkKeysyms
let keys = {
    k_del = _Delete;
    k_enter = _Return;
    k_esc = _Escape;
    k_fun = _F1;
}

let no_event _ = ()
let no_event_2 _ _ = ()
let event_true _ = true
let event_false _ = false

open StdLabels
open GtkBase
open GtkButton
open GtkData
open GtkMain
open GtkMenu
open GtkMisc
open GtkPack
open GtkWindow

type button_id = [`APPLY|`CANCEL|`CLOSE|`HELP|`NO|`OK|`YES]

let make_dialog ~title ~message ~buttons ?(no_delete=false) () = 
  let dia = Dialog.create () in
  Window.set_title dia title;
  let label = Label.create message in
  Label.set_line_wrap label true;
  Misc.set_padding label ~x:10 ~y:10 ();
  Container.add (Dialog.vbox dia) label;
  Widget.show label;
  List.iter buttons ~f:
    (fun (#button_id as id) ->
      Dialog.add_button dia
        (GtkStock.convert_id id) (Dialog.std_response id));
  begin match buttons with
    (#button_id as id) :: _ ->
      Dialog.set_default_response dia (Dialog.std_response id)
  | _ -> ()
  end;
  let rec check resp =
    if no_delete && resp = Dialog.std_response `DELETE_EVENT
    then check (Dialog.run dia)
    else resp
  in
  let r = check (Dialog.run dia) in
  Object.destroy dia;
  r

let message_box message =
  ignore (make_dialog ~title:"Message:" ~message ~buttons:[`OK] ())

let question_box message =
  let r =
    make_dialog ~title:"Message:" ~message ~buttons:[`YES;`NO]
      ~no_delete:true () in
  r = Dialog.std_response `YES

let question_cancel_box message =
  let r =
    make_dialog ~title:"Message:" ~message ~buttons:[`YES;`NO;`CANCEL] () in
  if r = GtkWindow.Dialog.std_response `YES then QR_YES else
  if r = GtkWindow.Dialog.std_response `NO then QR_NO else
  QR_CANCEL

let file_dialog ~title ?default () =
  let sel = FileSelection.create title in
  FileSelection.hide_fileop_buttons sel;
  Gaux.may default ~f:(FileSelection.set_filename sel);
  Window.set_modal sel true;
  let name = ref None in
  GtkSignal.connect sel ~sgn:Object.Signals.destroy ~callback:Main.quit;
  GtkSignal.connect (FileSelection.get_cancel_button sel)
    ~sgn:Button.Signals.clicked
    ~callback:(fun () -> Object.destroy sel);
  GtkSignal.connect (FileSelection.get_ok_button sel)
    ~sgn:Button.Signals.clicked
    ~callback:
    (fun () ->
      name := Some (FileSelection.get_filename sel); Object.destroy sel);
  Widget.show sel;
  Main.main ();
  !name

let open_file _ =
  file_dialog ~title:"Osiris Open Dialog" ()

let create_file ~default ~exts =
  file_dialog ~title:"Osiris Create Dialog" ~default ()

let select_directory ~title =
  file_dialog ~title ()

let mouse_pos () =
  Gdk.Window.get_pointer_location (Gdk.Window.root_parent ())

let exit_application = Main.quit

class menuitem parent item =
  object
    val item = item
    val mutable label = None
    val mutable enabled = true
    val mutable break = true
    val mutable submenu = None
    method caption s =
      break <- false;
      match label with
        Some l ->
          Label.set_text l s
      | None ->
          let l = Label.create s in
          label <- Some l;
          Widget.show l;
          Container.add item l
    method get_caption =
      match label with
        Some l -> Label.get_text l
      | None   -> ""
    method parent : menu = parent
    method enable b =
      Widget.set_sensitive item b;
      enabled <- b
    method is_enabled =
      enabled
    method check b =
      break <- false;
      CheckMenuItem.set_show_toggle item true;
      CheckMenuItem.set_active item b
    method is_checked =
      CheckMenuItem.get_active item
    method break b =
      break <- b;
      if b then begin
        CheckMenuItem.set_show_toggle item false;
        match label with None -> ()
        | Some l ->
            Container.remove item l;
            Object.destroy l;
            label <- None
      end
    method is_break = break
    method submenu opt =
      if submenu <> None then MenuItem.remove_submenu item;
      match opt with
        None -> ()
      | Some (sub : menu) ->
          break <- false;
          let menu =
            try Menu.cast sub#handle
            with _ -> failwith "Cannot add a window menu as submenu"
          in
          MenuItem.set_submenu item menu;
          submenu <- Some sub
    method get_submenu = submenu
    method on_click : event = fun callback ->
      ignore (GtkSignal.connect item ~sgn:MenuItem.Signals.activate ~callback)
  end

let new_menuitem (menu : menu) =
  let item = CheckMenuItem.create () in
  CheckMenuItem.set_show_toggle item false;
  MenuShell.append menu#handle item;
  Widget.show item;
  new menuitem menu item

let current_button_event = ref None

class imenu menu =
  object
    method handle : menuhandle = menu
    method destroy = GtkBase.Object.destroy menu
    method popup =
      let menu =
	try Menu.cast menu
        with _ -> failwith "Cannot popup a window menu"
      and (button, time) =
	match !current_button_event with
	  Some ev -> GdkEvent.Button.button ev, GdkEvent.Button.time ev
	| None -> 0, Int32.zero
      in Menu.popup menu ~button ~time
  end

let new_menu () =
  let m = Menu.create () in
  Widget.show m;
  new imenu (m :> menuhandle)

class virtual icomponent ?parent w =
  object (self)
    val w = w
    val parent = (parent : #container option :> container option)
    val mutable caption = ""
    val mutable align = AlNone
    val mutable destroyed = false
    val mutable custom = fun () -> ()
    val mutable x = 0
    val mutable y = 0
    method handle = (w :> Gtk.widget Gobject.obj)
    method parent = parent
    method visible b =
      if b then Widget.show w else Widget.hide w
    method is_visible = Object.get_flag w `VISIBLE
    method enable = Widget.set_sensitive w
    method is_enabled = Object.get_flag w `SENSITIVE
    method caption c = caption <- c
    method get_caption = caption
    method x x0 = x <- x0
    method y y0 = y <- y0
    method get_x = x
    method get_y = y
    method width width = Widget.set_usize w ~width ~height:(-2)
    method height height = Widget.set_usize w ~width:(-2) ~height
    method get_width = (Widget.allocation w).Gtk.width
    method get_height = (Widget.allocation w).Gtk.height
    method align al =
      align <- al;
      match parent with None -> ()
      | Some ct -> ct#update
    method get_align = align
    method virtual update : unit
    method component = (self :> component)
    method destroy = Object.destroy w
    method is_destroyed = destroyed
    initializer
      ignore (GtkSignal.connect w ~sgn:Object.Signals.destroy
		~callback:(fun () -> destroyed <- true))
    method font = new font w
    method focus = Widget.grab_focus w
    method set_custom f = custom <- f
    method call_custom = custom ()
  end

class icontainer ?parent w =
  object
    inherit icomponent ?parent w
    val mutable space = 0
    val mutable span = 0
    val mutable children = []
    val mutable boxes = []
    method space n = space <- n
    method span n = span <- n
    method update =
      List.iter boxes ~f:
	begin fun b ->
	  List.iter (Container.children b) ~f:(Container.remove b);
	  Object.destroy b
	end;
      let fixed = Fixed.create () in
      boxes <- [(fixed :> Gtk.container Gobject.obj)];
      let rec align box horiz = function
	  [] -> ()
	| (al, comp) :: rem ->
	    let box', horiz' =
	      if horiz && (al = AlTop || al = AlBottom) then
		Box.create `VERTICAL ~spacing:space (), false
	      else if (al = AlLeft || al = AlRight) then
		Box.create `HORIZONTAL ~spacing:space (), true
	      else box, horiz
	    in
	    if Gobject.get_oid box <> Gobject.get_oid box' then begin
	      boxes <- (box' :> Gtk.container Gobject.obj) :: boxes;
	      Widget.show box';
	      Box.pack box box';
	    end;
	    let w = comp#handle in
	    begin match al with
	      AlClient -> Container.add box' w
	    | AlCenter -> Box.pack box' w ~fill:true
	    | AlTop | AlLeft -> Box.pack box' w
	    | AlBottom | AlRight -> Box.pack box' w ~from:`END
	    | AlNone -> Fixed.put fixed w ~x:comp#get_x ~y:comp#get_y
	    end;
	    align box' horiz' rem
      in
      let hbox = Box.create `HORIZONTAL () in
      Container.set_border_width hbox space;
      Widget.show hbox;
      Container.add w hbox;
      let vbox = Box.create `VERTICAL () in
      Widget.show vbox;
      Container.add hbox vbox;
      align vbox false children
  end

let press_events = ref []

let do_action o x =
  match o with
    None -> false
  | Some f -> f x; true

class imouse_events w : mouse_events =
  object (self)
    val mutable on_click = None
    val on_dblclick = Array.create 3 None
    val on_down = Array.create 3 None
    val on_up = Array.create 3 None
    val mutable on_mousemove = None
    val mutable button_press_installed = false
    val mutable button_release_installed = false
    val mutable mousemove_installed = false
    method private button_event ev =
      let b = GdkEvent.Button.button ev - 1 in
      if b < 0 || b > 2 then false else
      let action =
        match GdkEvent.get_type ev with
          `BUTTON_PRESS -> on_down.(b)
        | `TWO_BUTTON_PRESS -> on_dblclick.(b)
        | `THREE_BUTTON_PRESS -> None
        | `BUTTON_RELEASE ->
            if b <> 0 then on_up.(b) else
            match on_up.(b), on_click with
            | Some f1, Some f2 -> f1 (); on_click
            | None, a -> a
            | a, None -> a
      in
      do_action a ()
          
    method private install_button_press =
      if not button_press_installed then begin
        GtkSignal.connect w ~sgn:Widget.Signals.Event.button_press
          ~callback:self#button_event;
        button_press_installed <- true;
      end
    method private install_button_release =
      if not button_release_installed then begin
        button_press_installed <- true;
        ignore (GtkSignal.connect w ~sgn:Widget.Signals.Event.button_release
                  ~callback:self#button_event)
      end

    method on_mousemove f =
      if not mousemove_installed then begin
        GtkSignal.connect w ~sgn:Widget.Signals.Event.motion_notify ~callback:
          begin fun ev ->
            match on_mousemove with
              None -> false
            | Some f ->
                f (truncate (GdkEvent.Motion.x ev))
                  (truncate (GdkEvent.Motion.y ev));
                true
          end;
        mousemove_installed <- true;
      end;
      on_mousemove <- Some f

    method on_click f =
      self#install_button_release;
      on_click <- Some f
    method private on_up n f =
      self#install_button_release;
      on_up.(n) <- Some f
    method on_lup = self#on_up 0
    method on_mup = self#on_up 1
    method on_rup = self#on_up 2
    method private on_down n f =
      self#install_button_press;
      on_down.(n) <- Some f
    method on_ldown = self#on_down 0
    method on_mdown = self#on_down 1
    method on_rdown = self#on_down 2
    method private on_dblclick0 n f =
      self#install_button_press;
      on_dblclick.(n) <- Some f
    method on_dblclick = self#on_dblclick0 0
    method on_mdblclick = self#on_dblclick0 1
    method on_rdblclick = self#on_dblclick0 2
  end

class ikey_events w : key_events =
  object
    val mutable on_key_down = None
    val mutable on_key_up = None
    val mutable key_down_installed = false
    val mutable key_up_installed = false
    method on_key_down f =
      if not key_down_installed then begin
        GtkSignal.connect w ~sgn:Widget.Signals.Event.key_press ~callback:
          begin fun ev ->
            do_action on_key_down (GdkEvent.Key.keyval ev)
          end;
        key_down_installed <- true;
      end;
      on_key_down <- Some f
    method on_key_release f =
      if not key_up_installed then begin
        GtkSignal.connect w ~sgn:Widget.Signals.Event.key_release ~callback:
          begin fun ev ->
            do_action on_key_up (GdkEvent.Key.keyval ev)
          end;
        key_up_installed <- true;
      end;
      on_on_key_up <- Some f
  end

class iwindow w =
  object
    inherit icontainer w
    inherit imouse_events w
    inherit ikey_events w

    method process = Glib.Main.iteration
    method close = Object.destroy w
    method window_state = function
        Maximize ->
