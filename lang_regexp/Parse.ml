(*
   Public entry point for parsing regexps.
*)

let parse lexbuf =
  let conf = Dialect.default in
  Parser.main (Lexer.token conf) lexbuf

let channel ic =
  Lexing.from_channel ic
  |> parse

let file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
       Lexing.from_channel ic
       |> parse
    )

let string s =
  Lexing.from_string s
  |> parse
