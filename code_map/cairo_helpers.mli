(*s: cairo_helpers.mli *)

(*s: cairo helpers functions sig *)
val fill_rectangle:
  ?alpha:float ->
  cr:Cairo.context -> 
  x:float -> y:float -> w:float -> h:float ->
  color:Simple_color.emacs_color -> 
  unit ->
  unit

val draw_rectangle_figure:
  cr:Cairo.context -> 
  color:Simple_color.emacs_color -> 
  Figures.rectangle -> unit

val draw_rectangle_bis:
  cr:Cairo.context -> 
  color:Simple_color.color -> 
  line_width:float ->
  Figures.rectangle -> unit


val prepare_string : string -> string
val origin : Figures.point

val device_to_user_distance_x : Cairo.context -> float -> float
val device_to_user_distance_y : Cairo.context -> float -> float
val user_to_device_distance_x : Cairo.context -> float -> float
val user_to_device_distance_y : Cairo.context -> float -> float

val device_to_user_size : Cairo.context -> float -> float
val user_to_device_font_size : Cairo.context -> float -> float
val cairo_point_to_point : float * float -> Figures.point

val show_text : Cairo.context -> string -> unit
val text_extents : Cairo.context -> string -> Cairo.text_extents
val set_font_size: Cairo.context -> float -> unit

val clear : Cairo.context -> unit

val surface_of_pixmap :
  < pixmap : [> `drawable ] Gobject.obj; .. > -> Cairo.Surface.t

val distance_points : Figures.point -> Figures.point -> float

(*e: cairo helpers functions sig *)
(*e: cairo_helpers.mli *)
