(*
   Some predefined dialects.
*)

open Conf

let default = {
  dotall = false;
  multiline = false;
  ucp = false;
  with_comment_groups = true;
  ignore_whitespace = false;
  ignore_whitespace_in_char_classes = false;
  ignore_hash_comments = false;
}

(* https://www.pcre.org/original/doc/html/pcrepattern.html *)
let pcre = default

let pcre_extended = {
  default with
  ignore_whitespace = true;
  ignore_hash_comments = true;
}

(* https://perldoc.perl.org/perlre *)
let perl = pcre

let perl_x = pcre_extended

let perl_xx = {
  pcre_extended with
  ignore_whitespace_in_char_classes = true;
}

(* https://docs.python.org/3/library/re.html *)
let python = default

(* https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions/Cheatsheet *)
let javascript = default

(* https://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html *)
let java = default

(* https://github.com/google/re2/wiki/Syntax *)
let go = {
  default with with_comment_groups = false
}
