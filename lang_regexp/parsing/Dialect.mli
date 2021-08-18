(*
   Some predefined dialects. They can be used as a base for further dialects
   using the 'with' syntax, e.g.

     open Regexp.Conf
     let perl_xx = {
       Regex.Dialect.perl with
       ignore_whitespace = true;
       ignore_whitespace_in_char_classes = true;
     }
*)

(* The default is the same as PCRE with the default options. *)
val default : Conf.t

val pcre : Conf.t

(* PCRE with the PCRE_EXTENDED flag *)
val pcre_extended : Conf.t

(* Same as PCRE *)
val perl : Conf.t

(* Perl with the '/x' option, same as PCRE_EXTENDED *)
val perl_x : Conf.t

(* Perl with the '/xx' option *)
val perl_xx : Conf.t

(*
   The following variants are meant to match the standard regexp
   implementation provided by these languages or their standard library.
*)
val python : Conf.t
val go : Conf.t
val java : Conf.t
val javascript : Conf.t
