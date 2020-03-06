type t = 
  | Python
  | Javascript
  | Java
  | Go
  | C
  | ML

val lang_of_string_map: (string, t) Hashtbl.t
val lang_of_string_opt: string -> t option
val lang_of_filename_opt: Common.filename -> t option

val files_of_dirs_or_files: t -> Common.path list -> 
  Common.filename list

val string_of_lang: t -> string
val ext_of_lang: t -> string list
