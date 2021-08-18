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

val go : Conf.t
val java : Conf.t
val javascript : Conf.t
val pcre : Conf.t
val perl : Conf.t
val python : Conf.t
