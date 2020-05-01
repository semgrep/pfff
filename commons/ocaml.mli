(*s: pfff/commons/ocaml.mli *)

(* 
 * OCaml hacks to support reflection (works with ocamltarzan).
 *
 * See also sexp.ml, json.ml, and xml.ml for other "reflective" techniques.
 *)

(*s: type [[Ocaml.t]] *)
(* OCaml core type definitions (no objects, no modules) *)
type t =
  | Unit 
  | Bool | Float | Char | String | Int

  | Tuple of t list
  | Dict of (string * [`RW|`RO] * t) list (* aka record *)
  | Sum of (string * t list) list         (* aka variants *)

  | Var of string
  | Poly of string
  | Arrow of t * t

  | Apply of string * t

  (* special cases of Apply *) 
  | Option of t
  | List of t 

  | TTODO of string
(*e: type [[Ocaml.t]] *)

(*s: signature [[Ocaml.add_new_type]] *)
val add_new_type: string -> t -> unit
(*e: signature [[Ocaml.add_new_type]] *)
(*s: signature [[Ocaml.get_type]] *)
val get_type: string -> t
(*e: signature [[Ocaml.get_type]] *)

(*s: type [[Ocaml.v]] *)
(* OCaml values (a restricted form of expressions) *)
type v = 
  | VUnit 
  | VBool of bool | VFloat of float | VInt of int
  | VChar of char | VString of string

  | VTuple of v list
  | VDict of (string * v) list
  | VSum of string * v list

  | VVar of (string * int64)
  | VArrow of string

  (* special cases *) 
  | VNone | VSome of v
  | VList of v list
  | VRef of v

  | VTODO of string
(*e: type [[Ocaml.v]] *)

(*s: signature [[Ocaml.vof_unit]] *)
(* building blocks, used by code generated using ocamltarzan *)
val vof_unit   : unit -> v
(*e: signature [[Ocaml.vof_unit]] *)
(*s: signature [[Ocaml.vof_bool]] *)
val vof_bool   : bool -> v
(*e: signature [[Ocaml.vof_bool]] *)
(*s: signature [[Ocaml.vof_int]] *)
val vof_int    : int -> v
(*e: signature [[Ocaml.vof_int]] *)
(*s: signature [[Ocaml.vof_float]] *)
val vof_float   : float -> v
(*e: signature [[Ocaml.vof_float]] *)
(*s: signature [[Ocaml.vof_string]] *)
val vof_string : string -> v
(*e: signature [[Ocaml.vof_string]] *)
(*s: signature [[Ocaml.vof_list]] *)
val vof_list   : ('a -> v) -> 'a list -> v
(*e: signature [[Ocaml.vof_list]] *)
(*s: signature [[Ocaml.vof_option]] *)
val vof_option : ('a -> v) -> 'a option -> v
(*e: signature [[Ocaml.vof_option]] *)
(*s: signature [[Ocaml.vof_ref]] *)
val vof_ref    : ('a -> v) -> 'a ref -> v
(*e: signature [[Ocaml.vof_ref]] *)
(*s: signature [[Ocaml.vof_either]] *)
val vof_either    : ('a -> v) -> ('b -> v) -> ('a, 'b) Common.either -> v
(*e: signature [[Ocaml.vof_either]] *)
(*s: signature [[Ocaml.vof_either3]] *)
val vof_either3    : ('a -> v) -> ('b -> v) -> ('c -> v) -> 
  ('a, 'b, 'c) Common.either3 -> v
(*e: signature [[Ocaml.vof_either3]] *)

(*s: signature [[Ocaml.int_ofv]] *)
val int_ofv:    v -> int
(*e: signature [[Ocaml.int_ofv]] *)
(*s: signature [[Ocaml.float_ofv]] *)
val float_ofv:  v -> float
(*e: signature [[Ocaml.float_ofv]] *)
(*s: signature [[Ocaml.unit_ofv]] *)
val unit_ofv: v -> unit
(*e: signature [[Ocaml.unit_ofv]] *)
(*s: signature [[Ocaml.string_ofv]] *)
val string_ofv: v -> string
(*e: signature [[Ocaml.string_ofv]] *)
(*s: signature [[Ocaml.list_ofv]] *)
val list_ofv: (v -> 'a) -> v -> 'a list
(*e: signature [[Ocaml.list_ofv]] *)
(*s: signature [[Ocaml.option_ofv]] *)
val option_ofv: (v -> 'a) -> v -> 'a option
(*e: signature [[Ocaml.option_ofv]] *)

(*s: signature [[Ocaml.string_of_v]] *)
(* regular pretty printer (not via sexp, but using Format) *)
val string_of_v: v -> string
(*e: signature [[Ocaml.string_of_v]] *)

(* sexp converters *)
(*
val sexp_of_t: t -> Sexp.t
val t_of_sexp: Sexp.t -> t
val sexp_of_v: v -> Sexp.t
val v_of_sexp: Sexp.t -> v
val string_sexp_of_t: t -> string
val t_of_string_sexp: string -> t
val string_sexp_of_v: v -> string
val v_of_string_sexp: string -> v
*)

(* json converters *)
(*
val v_of_json: Json_type.json_type -> v
val json_of_v: v -> Json_type.json_type
val save_json: Common.filename -> Json_type.json_type -> unit
val load_json: Common.filename -> Json_type.json_type
*)

(*s: signature [[Ocaml.map_v]] *)
(* mapper/visitor *)
val map_v: 
  f:( k:(v -> v) -> v -> v) -> 
  v -> 
  v
(*e: signature [[Ocaml.map_v]] *)

(*s: signature [[Ocaml.map_of_unit]] *)
(* other building blocks, used by code generated using ocamltarzan *)
val map_of_unit: unit -> unit
(*e: signature [[Ocaml.map_of_unit]] *)
(*s: signature [[Ocaml.map_of_bool]] *)
val map_of_bool: bool -> bool
(*e: signature [[Ocaml.map_of_bool]] *)
(*s: signature [[Ocaml.map_of_int]] *)
val map_of_int: int -> int
(*e: signature [[Ocaml.map_of_int]] *)
(*s: signature [[Ocaml.map_of_float]] *)
val map_of_float: float -> float
(*e: signature [[Ocaml.map_of_float]] *)
(*s: signature [[Ocaml.map_of_char]] *)
val map_of_char: char -> char
(*e: signature [[Ocaml.map_of_char]] *)
(*s: signature [[Ocaml.map_of_string]] *)
val map_of_string: string -> string
(*e: signature [[Ocaml.map_of_string]] *)
(*s: signature [[Ocaml.map_of_ref]] *)
val map_of_ref: ('a -> 'b) -> 'a ref -> 'b ref
(*e: signature [[Ocaml.map_of_ref]] *)
(*s: signature [[Ocaml.map_of_ref_do_nothing_share_ref]] *)
val map_of_ref_do_nothing_share_ref: ('a -> 'a) -> 'a ref -> 'a ref
(*e: signature [[Ocaml.map_of_ref_do_nothing_share_ref]] *)
(*s: signature [[Ocaml.map_of_option]] *)
val map_of_option: ('a -> 'b) -> 'a option -> 'b option
(*e: signature [[Ocaml.map_of_option]] *)
(*s: signature [[Ocaml.map_of_list]] *)
val map_of_list: ('a -> 'a) -> 'a list -> 'a list
(*e: signature [[Ocaml.map_of_list]] *)
(*s: signature [[Ocaml.map_of_either]] *)
val map_of_either: 
  ('a -> 'b) -> ('c -> 'd) -> ('a, 'c) Common.either -> ('b, 'd) Common.either
(*e: signature [[Ocaml.map_of_either]] *)
(*s: signature [[Ocaml.map_of_either3]] *)
val map_of_either3: 
  ('a -> 'b) -> ('c -> 'd) -> ('e -> 'f) -> 
  ('a, 'c, 'e) Common.either3 -> ('b, 'd, 'f) Common.either3
(*e: signature [[Ocaml.map_of_either3]] *)

(*s: signature [[Ocaml.v_unit]] *)
(* pure visitor building blocks, used by code generated using ocamltarzan *)
val v_unit: unit -> unit
(*e: signature [[Ocaml.v_unit]] *)
(*s: signature [[Ocaml.v_bool]] *)
val v_bool: bool -> unit
(*e: signature [[Ocaml.v_bool]] *)
(*s: signature [[Ocaml.v_int]] *)
val v_int: int -> unit
(*e: signature [[Ocaml.v_int]] *)
(*s: signature [[Ocaml.v_float]] *)
val v_float: float -> unit
(*e: signature [[Ocaml.v_float]] *)
(*s: signature [[Ocaml.v_string]] *)
val v_string: string -> unit
(*e: signature [[Ocaml.v_string]] *)
(*s: signature [[Ocaml.v_option]] *)
val v_option: ('a -> unit) -> 'a option -> unit
(*e: signature [[Ocaml.v_option]] *)
(*s: signature [[Ocaml.v_list]] *)
val v_list: ('a -> unit) -> 'a list -> unit
(*e: signature [[Ocaml.v_list]] *)
(*s: signature [[Ocaml.v_ref]] *)
val v_ref: ('a -> unit) -> 'a ref -> unit
(*e: signature [[Ocaml.v_ref]] *)
(*s: signature [[Ocaml.v_either]] *)
val v_either: 
  ('a -> unit) -> ('b -> unit) -> 
  ('a, 'b) Common.either -> unit
(*e: signature [[Ocaml.v_either]] *)
(*s: signature [[Ocaml.v_either3]] *)
val v_either3: 
  ('a -> unit) -> ('b -> unit) -> ('c -> unit) ->
  ('a, 'b, 'c) Common.either3 -> unit
(*e: signature [[Ocaml.v_either3]] *)
(*e: pfff/commons/ocaml.mli *)
