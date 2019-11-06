type t = 
  | Python
  | Javascript
  | Java
  | C
  | ML

val lang_of_string_opt: string -> t option
val lang_of_filename_opt: Common.filename -> t option

val files_of_dirs_or_files: t -> Common.path list -> 
  Common.filename list
