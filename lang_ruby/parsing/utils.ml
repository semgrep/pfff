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

let format_to_string f e = 
  let buf = Buffer.create 128 in
  let ppf = Format.formatter_of_buffer buf in
    f ppf e;
    Format.pp_print_flush ppf ();
    Buffer.contents buf
      
let string_fold_left f acc s = 
  let len = String.length s in
    if len < 1 then acc
    else 
      let rec work idx acc = 
        if idx < len then work (idx+1) (f acc s.[idx])
        else acc
      in work 0 acc
