
(* the string parameter is the "checker id" *)
val error_to_json: string -> Error_code.error -> Json_type.t

val string_of_errors: string -> Error_code.error list -> string
