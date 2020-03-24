module StrSet = Set.Make(String)

external id : 'a -> 'a = "%identity"

(* Use Obj.* to build a "shallow" order based on constructor
   representation (subterms are not examined).  This should be
   completely safe since we are only peeking into the constructor tag,
   not doing any magic casting. *)
let cmp_ctors e1 e2 = 
  let o1 = Obj.repr e1 in
  let o2 = Obj.repr e2 in
    match Obj.is_int o1, Obj.is_int o2 with
      | true,true -> Pervasives.compare e1 e2
      | true,false -> -1
      | false,true -> 1
      | false,false -> Pervasives.compare (Obj.tag o1) (Obj.tag o2)

let cmp2 c1 f a1 a2 = match c1 with
  | 0 -> f a1 a2
  | _ -> c1

let cmp_list cmp el1 el2 = 
  try List.fold_left2 (fun acc e1 e2 -> cmp2 acc cmp e1 e2) 0 el1 el2
  with Invalid_argument _ -> 
    (* different sized lists *)
    Pervasives.compare (List.length el1) (List.length el2)

let default_opt def opt = match opt with 
  | None -> def
  | Some x -> x

let rec last = function
  | [] -> raise (Invalid_argument "last")
  | [x] -> x
  | _x::tl -> last tl

let format_delim_list delim format_f ppf = function
  | [] -> ()
  | hd::[] -> format_f ppf hd
  | hd::tl -> format_f ppf hd;
      List.iter (Format.fprintf ppf (delim ^^ "%a") format_f) tl

let format_comma_list f ppf lst = format_delim_list ",@ " f ppf lst
let format_break_list f ppf lst = format_delim_list "@," f ppf lst

let format_to_string f e = 
  let buf = Buffer.create 128 in
  let ppf = Format.formatter_of_buffer buf in
    f ppf e;
    Format.pp_print_flush ppf ();
    Buffer.contents buf


let format_option format_e ppf = function
  | None -> ()
  | Some e -> format_e ppf e

      
let string_fold_left f acc s = 
  let len = String.length s in
    if len < 1 then acc
    else 
      let rec work idx acc = 
        if idx < len then work (idx+1) (f acc s.[idx])
        else acc
      in work 0 acc

let do_opt ~none:none ~some:some opt = match opt with
  | None -> none
  | Some s -> some s

let eq_opt f o1 o2 = match o1, o2 with
  | None, None -> true
  | Some x1, Some x2 -> f x1 x2
  | _ -> false

let map_preserve map f t = 
  let changed = ref false in
  let t' = 
    map (fun v ->
           let v' = f v in
             if v != v'
             then changed := true;
             v'
        ) t
  in if !changed then t' else t

let map_opt_preserve f = function
  | None -> None
  | (Some x) as s -> 
      let x' = f x in 
        if x != x' then Some x' else s

(* escapes specified character(s) by going over each character of the string *)
type esc_mode = 
    NonEsc of string
  | Esc of string

let escape_chars haystack esc_chars = 
  let bslash = String.make 1 (Char.chr 92) in (* back slash *)
  let rec fold_left f (a : esc_mode) (s : string) : esc_mode = match s with
    | "" -> a
    | s ->
      let acc = (f a (String.sub s 0 1)) in
        try
          fold_left f acc (String.sub s 1 ((String.length s) - 1))
        with Invalid_argument(_) -> acc
  in
  let result =
    fold_left 
      (fun (mode : esc_mode) (c : string) -> match mode with
        | NonEsc(s) -> (* nonescaping mode *)
            if List.mem (String.get c 0) esc_chars then NonEsc(s ^ bslash ^ c)
            else if c = "\\" then Esc(s ^ c)
            else NonEsc(s ^ c) 
        | Esc(s) -> (* escaping mode; only one char is allowed anyway *)
            NonEsc(s ^ c) 
      ) (NonEsc("")) haystack
  in
    match result with
      | NonEsc(s) -> s
      | Esc(s) -> 
          Printf.fprintf stderr 
            "[WARN] string has inappropriate format (Esc) orig:%s res:%s\n" 
            haystack s;
          haystack (* but supposed to be an error *)

let is_tmp_var s = 
  let re = Str.regexp "__tmp_" in
    if Str.string_match re s 0 then true
    else s = "_"
