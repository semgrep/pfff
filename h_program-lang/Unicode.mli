(*
   Utilities for dealing with Unicode issues.
*)

(*
   Convert each non-ascii byte by one ascii byte.
   The default replacement byte is 'X'.

   This is meant to be used as a hack to tolerate non-ascii input in
   lexers that only support ascii.
*)
val input_and_replace_non_ascii :
  ?replacement_byte:char -> in_channel -> string
