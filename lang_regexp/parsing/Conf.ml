(*
   Runtime configuration options defining a regexp dialect.
*)

type t = {
  dotall: bool;
  multiline: bool;
  ucp: bool;
  with_comment_groups: bool;
  ignore_whitespace: bool;
  ignore_whitespace_in_char_classes: bool;
  ignore_hash_comments: bool;
}
