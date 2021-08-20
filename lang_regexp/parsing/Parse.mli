(*
   Public entry point for parsing regexps.

   The default implementation tries to be compatible with the latest
   version of PCRE, documented at
   https://www.pcre.org/original/doc/html/pcrepattern.html

   To define or select a particular regexp dialect, see Dialect.mli.

   For a description of all the features and which dialects support what,
   consult the site regular-expressions.info.
   - cheatsheet: https://www.regular-expressions.info/refquick.html
   - dialect comparison: https://www.regular-expressions.info/refcharacters.html

   Main to-dos:
   - javascript extensions: \U is not special, \u1234
   - backslash assertions like \A, \b, etc.
   - backreferences \2
   - hexadecimal character notation \xFF
   - octal character notation \011, \o{11}
   - [add feature request here]
*)

(* Parse a file *)
val file : ?conf:Conf.t -> string -> AST.t

(* Parse a string *)
val string : ?conf:Conf.t -> string -> AST.t

(* Alias for 'file' *)
val parse : ?conf:Conf.t -> string -> AST.t
