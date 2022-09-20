
(* This module governs a path type, which can only produce values which
   are for normalized paths to existent files or directories.
   These paths are always canonically kept at their absolute forms, in order to
   maintain a canonicity property. Due to the `Path` constructor being private, we
   can enforce this invariant at the type level.
*)
type path = Path of string [@@deriving show, eq]
type t = path [@@deriving show, eq]
let of_string s = Path (Unix.realpath s)
let to_string (Path s) = s
let canonical s = to_string (of_string s)

let (/) (Path s1) s2 = of_string (Filename.concat s1 s2)
let concat = (/)

let apply ~f (Path s) = f s

let file_exists (Path s) = Sys.file_exists s

let cat = apply ~f:Common.cat
let read_file ?max_len = apply ~f:(Common.read_file ?max_len)
let write_file ~file:(Path s1) = Common.write_file ~file:s1
let is_directory = apply ~f:Common.is_directory

let basename (Path s) = Filename.basename s
let dirname (Path s) = Filename.dirname s |> of_string
let extension (Path s) = Filename.extension s
