

type t = {
  (* support '(?# this is a comment)' *)
  with_comment_groups: bool;

  (* Ignore whitespace outside of character classes.
     Must use '\s' to match a space character etc.
     This corresponds to '/x' in perl and PCRE_EXTENDED in PCRE. *)
  ignore_whitespace: bool;

  (* Ignore whitespace in character classes. *)
  ignore_whitespace_in_char_classes: bool;

  (* Ignore any '#' character and what follows until the end of the line. *)
  ignore_hash_comments: bool;
}
