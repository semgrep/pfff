type t = 
  (* Python will start with Python3 mode and fall back to Python2 in case
   * of error. Python2 and Python3 are for specific version of Python 
   * (no fallback) *)
  | Python | Python2 | Python3
  | Javascript
  | Java
  | Go
  | C
  | ML

val lang_of_string_map: (string, t) Hashtbl.t
val lang_of_string_opt: string -> t option

val langs_of_filename: Common.filename -> t list

val files_of_dirs_or_files: t -> Common.path list -> 
  Common.filename list

val string_of_lang: t -> string
val ext_of_lang: t -> string list
