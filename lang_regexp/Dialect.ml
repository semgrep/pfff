(*
   Some predefined dialects.
*)

open Conf

let default = {
  with_comment_groups = true;
  ignore_whitespace = false;
  ignore_whitespace_in_char_classes = false;
  ignore_hash_comments = false;
}

(* https://perldoc.perl.org/perlre *)
let perl = default

(* https://www.pcre.org/original/doc/html/pcrepattern.html *)
let pcre = perl

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
