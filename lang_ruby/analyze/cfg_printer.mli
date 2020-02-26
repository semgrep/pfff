
open Cfg

(** The signature for a CFG pretty printer.  Where appropraite, type
    declaration present in the Cfg module have a corresponding format
    function here.  The naming scheme is meant to exactly match that
    of the type signatures.  Thus if there is a type foo_bar, then the
    corresponding formatting function is named format_foo_bar.  Note
    that some intermediate/helper types in Cfg do not have their own
    formatting function, and this signature includes a few helpers of
    its own, which have no corresponding Cfg types. 
 *)
module type CfgPrinter = sig
  val format_unary_op : Format.formatter -> unary_op -> unit
  val format_binary_op : Format.formatter -> binary_op -> unit
  val format_identifier : Format.formatter -> identifier -> unit
  val format_msg_id : Format.formatter -> msg_id -> unit
  val format_star_expr : Format.formatter -> star_expr -> unit
  val format_expr : Format.formatter -> expr -> unit
  val format_literal : Format.formatter -> literal -> unit
  val format_tuple_expr : Format.formatter -> tuple_expr -> unit
  val format_lhs : Format.formatter -> lhs -> unit
  val format_def_name : Format.formatter -> def_name -> unit
  val format_any_formal : Format.formatter -> any_formal -> unit
  val format_class_kind : Format.formatter -> class_kind -> unit
  val format_method_call : Format.formatter -> method_call -> unit
  val format_case : Format.formatter -> case_block -> unit
  val format_codeblock : Format.formatter -> block_formal_param list * stmt -> unit
  val format_rescue_guard : Format.formatter -> rescue_guard -> unit 
  val format_stmt : Format.formatter -> stmt -> unit
  val format_cfg : Format.formatter -> stmt -> unit

  (* internal helpers, but may be of general use *) 
  val format_def_id : Format.formatter -> identifier -> unit
  val format_formal_tuple : Format.formatter -> any_formal list -> unit
  val format_formals : Format.formatter -> any_formal list -> unit
  val format_target_and_msg : Format.formatter -> expr option * msg_id -> unit

  val string_of_expr : expr -> string
  val string_of_cfg : stmt -> string
  val print_stmt : out_channel -> stmt -> unit
end

(** This module formats CFGs so that they are valid Ruby code. *)
module CodePrinter : CfgPrinter

(** This module shares most of the implementation of [CodePrinter],
    but simplifies away some of the translations done such as
    temporary variables and implicit returns.  Code formatted by this
    module is, in general, NOT valid Ruby code, but may be easier for
    humans to interpret.
*)
module ErrorPrinter : CfgPrinter



(** Functional unparsing in the style of Danvy.  Here, unparsing is
    performed by specifying a format descriptor using abstract syntax,
    as opposed to the specialized strings of the Printf module.
    Unlike the printing functions above, these modules allow the
    injection of string literals, allowing you to express Ruby syntax
    as a composition of strings and CFG datatypes.  These descriptors
    construct a curried function which accept values corresponding the
    types in the descriptor.  Finally, these constructs can be passed
    to one of the emitting functions below to actually perform the
    unparsing.

    For example,  

    [sprintf (s"x = " ++ expr ++ s" * 2 - " ++ identifier) e i]

    returns the string "x = #{e} * 2 - #{i}"with the values of [e] and
    [i] substituted for the nested #{} expressions.
*)
module type UnParser = sig
  
  type ('a,'b) fmt
    (** The type of format descriptors.  The first parameter ('a) is
        an accumulator which is ultimately returned by one of the
        emitting functions below.  The second parameter ('b) is the
        return type of this particular descriptor, which typically is
        a function type that returns 'a. *)

  val s : string -> ('a, 'a) fmt
    (** A descriptor that simply includes the string argument as is.
        Thus, (sprintf s) is the identity function over strings.  *)

  (** These provide formatting actions for each of the standard CFG
      datatypes.  For example, [sprintf unary_op Op_Plus] returns
      "+". 
  *)
  val unary_op : ('a, unary_op -> 'a) fmt
  val binary_op : ('a, binary_op -> 'a) fmt
  val identifier : ('a, identifier -> 'a) fmt
  val msg_id : ('a, msg_id -> 'a) fmt
  val star_expr : ('a, star_expr -> 'a) fmt
  val expr : ('a, expr -> 'a) fmt
  val literal : ('a, literal -> 'a) fmt
  val tuple_expr : ('a, tuple_expr -> 'a) fmt
  val lhs : ('a, lhs -> 'a) fmt
  val def_name : ('a, def_name -> 'a) fmt
  val any_formal : ('a, any_formal -> 'a) fmt
  val class_kind : ('a, class_kind -> 'a) fmt
  val method_call : ('a, method_call -> 'a) fmt
  val case : ('a, case_block -> 'a) fmt
  val codeblock : ('a, block_formal_param list * stmt -> 'a) fmt
  val rescue_guard : ('a, rescue_guard -> 'a) fmt 
  val stmt : ('a, stmt -> 'a) fmt

  val comma_list : (unit,'elt -> unit) fmt -> ('a, 'elt list -> 'a) fmt
  val option : (unit,'elt -> unit) fmt -> ('a, 'elt option -> 'a) fmt

  val string : ('a, string -> 'a) fmt
  val of_fmt : (Format.formatter -> 'b -> unit) -> ('a, 'b -> 'a) fmt
  val to_fmt : (unit, 'b -> unit) fmt -> (Format.formatter -> 'b -> unit)

  (** Emitting functions *)

  val format : Format.formatter -> (unit,'a) fmt -> 'a
    (** Unparse the descriptor into the formatter supplied as the
        first argument.  Ultimately returns unit. *)

  val sprintf : (string,'a) fmt -> 'a
    (** Unparse the descriptor into a string, returning it after it
        has been fully applied. *)

  val kformat : (Format.formatter -> 'a) -> Format.formatter -> ('a,'b) fmt -> 'b
    (** Like [format] above, except that at the end of unparsing, it
        passes the formatter to the function given as the first
        argument and returns its result.  *)

  val ksformat : (string -> 'a) -> ('a,'b) fmt -> 'b
    (** Like [sprintf] above, except it passes the final string to the
        function given as the first argument and returns its result.
    *)

  val (++) : ('a, 'b) fmt -> ('c, 'a) fmt -> ('c, 'b) fmt
    (** Combinator for composing two unparsing descriptors.  For
        example, [unary_op ++ s"x"]
    *)
end

(** An implementation of the UnParser module that uses the CodePrinter
    module to unparse the datatypes.  Thus, this implementation will
    emit valid Ruby code for any of the combinators above, with the
    possible exception of [s], since this may contain any string. 
*)
module CodeUnparser : UnParser

(** An implementation of the UnParser module that uses the
    ErrorPrinter module to unparse the datatypes.  Thus, this formats
    the strings using a more readle, but in general, not valid for
    Ruby form. 
*)
module ErrorUnparser : UnParser
