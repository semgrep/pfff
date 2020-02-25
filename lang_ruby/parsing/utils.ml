
module StrSet = Set.Make(String)
module StrMap = Map.Make(String)

module Int = struct
  type t = int
  let compare (x:int) (y:int) = compare x y
  let equal (x:int) (y:int) = x == y
  let hash = Hashtbl.hash
end

module IntSet = Set.Make(Int)
module IntMap = Map.Make(Int)

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

module type Comparable = sig type t val compare : t -> t -> int end
module Pair(X:Comparable)(Y:Comparable) = struct
  type t = X.t * Y.t
  let compare (x1,y1) (x2,y2) = 
    cmp2 (X.compare x1 x2) Y.compare y1 y2
end
      
module StrPairSet = Set.Make(Pair(String)(String))

let default_opt def opt = match opt with 
  | None -> def
  | Some x -> x

let do_opt ~none:none ~some:some opt = match opt with
  | None -> none
  | Some s -> some s

let map_opt f = function
  | None -> None
  | Some x -> Some (f x)

let map_opt_preserve f = function
  | None -> None
  | (Some x) as s -> 
      let x' = f x in 
        if x != x' then Some x' else s

let cmp_opt f o1 o2 = match o1, o2 with
  | None, None -> 0
  | Some _, None -> -1
  | None, Some _ -> 1
  | Some x, Some y -> f x y

let eq_opt f o1 o2 = match o1, o2 with
  | None, None -> true
  | Some x1, Some x2 -> f x1 x2
  | _ -> false

(* [try_each f lst] Try to apply [f] to each element of the list [lst] 
  until it does not throw an exception.  The first such value
   returned from [f] is returned by [try_each].  If [f] raises an
   exception on every element in the list (or the list is empty),
   [try_each] raises [Not_found].
*)
let rec try_each f = function
  | [] -> raise Not_found
  | hd::tl -> try f hd with _ -> try_each f tl

let try_not_found ~not_found:nf f x =
  try f x 
  with Not_found -> nf x

let not_found_opt f x = 
  try Some(f x)
  with Not_found -> None

let strmap_size m = StrMap.fold (fun k v acc -> acc+1) m 0

let substr s1 s2 = 
  let len1 = String.length s1 in
  let len2 = String.length s2 in
    if len1 > len2 
    then false
    else
      let sub2 = String.sub s2 0 len1 in
        (String.compare s1 sub2) = 0

let strmap_union join m1 m2 = 
  StrMap.fold
    (fun k v acc ->
         try
           let v' = StrMap.find k acc in
             StrMap.add k (join v v') acc
         with Not_found -> StrMap.add k v acc
    ) m1 m2

let strmap_inter meet m1 m2 = 
  StrMap.fold
    (fun k v acc ->
         try
           let v' = StrMap.find k acc in
             StrMap.add k (meet v v') acc
         with Not_found -> acc
    ) m1 m2

let strmap_diff m1 m2 = 
  StrMap.fold (fun k v acc -> StrMap.remove k acc) m2 m1

let intmap_union join m1 m2 = 
  IntMap.fold
    (fun k v acc ->
         try
           let v' = IntMap.find k acc in
             IntMap.add k (join v v') acc
         with Not_found -> IntMap.add k v acc
    ) m1 m2

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

let map_to_string fold k_s v_s map = 
  let res, _ = fold
    (fun k v (acc,first) ->
      if first
      then (Printf.sprintf "%s => %s" (k_s k) (v_s v)), false
      else (Printf.sprintf "%s, %s => %s" acc (k_s k) (v_s v)), false
    ) map ("",true)
  in
    res

let strmap_to_string to_s map = map_to_string StrMap.fold id to_s map
let intmap_to_string to_s map = 
  map_to_string IntMap.fold string_of_int to_s map

let list_to_string to_s = function
  | [] -> ""
  | [x] -> to_s x
  | x::tl ->
      let first = to_s x in
        List.fold_left
          (fun acc t -> acc ^ ", " ^ (to_s t)
          ) first tl

let list_is_empty = function [] -> true | _ -> false

let rec last = function
  | [] -> raise (Invalid_argument "last")
  | [x] -> x
  | x::tl -> last tl

let env_is_set env_name = 
  try ignore(Sys.getenv env_name);true
  with Not_found -> false

let format_delim_list delim format_f ppf = function
  | [] -> ()
  | hd::[] -> format_f ppf hd
  | hd::tl -> format_f ppf hd;
      List.iter (Format.fprintf ppf (delim ^^ "%a") format_f) tl

let format_comma_list f ppf lst = format_delim_list ",@ " f ppf lst
let format_break_list f ppf lst = format_delim_list "@," f ppf lst

let format_option format_e ppf = function
  | None -> ()
  | Some e -> format_e ppf e

let format_to_string f e = 
  let buf = Buffer.create 128 in
  let ppf = Format.formatter_of_buffer buf in
    f ppf e;
    Format.pp_print_flush ppf ();
    Buffer.contents buf

let format_stringf f () x = format_to_string f x

let format_map iter format_k format_v ppf map =
  let format_inside ppf map = 
    iter
      (fun k v ->
         Format.fprintf ppf "@ @[(%a => %a)@]" format_k k format_v v
      ) map
  in
    Format.fprintf ppf "@[%a@]" format_inside map

let format_strmap fmtf ppf map = 
  format_map StrMap.iter Format.pp_print_string fmtf ppf map
let format_intmap fmtf ppf map = 
  format_map IntMap.iter Format.pp_print_int fmtf ppf map

let file_contents file = 
  let ic = open_in file in
  let len = in_channel_length ic in
  let buf = String.create len in
    really_input ic buf 0 len;
    close_in ic;
    buf

let string_map f s = 
  let s = String.copy s in
    for i = 0 to (String.length s-1) do
      s.[i] <- f s.[i]
    done;
    s

(* reverse composition.  useful for "pipelining" computations:
   e1 |> e2 |> e3
   is
   e3 (e2 (e1))
*)
let (<@) x f = f x

let (@<) f x = f x

let (%) f g x = f (g x)


let timef counter f arg = 
  let start = Unix.gettimeofday () in
  let ret = f arg in
  let fin = Unix.gettimeofday () in
    counter := !counter +. (fin -. start);
    ret

let is_capital s = 
  if String.length s < 1 then false
  else match s.[0] with
  | 'A'..'Z' -> true
  | _ -> false

(* return an option type with the file extension.
   e.g., abc.txt returns Some ".txt" 
*)
let extension s = 
  let s = Filename.basename s in
    try
      let idx = String.rindex s '.' in
      let len = String.length s in
        Some (String.sub s idx (len-idx))
    with Not_found -> None

let string_chomp s = 
  if s.[String.length s - 1] == '\n'
  then String.sub s 0 (String.length s - 1)
  else s
      
let string_fold_left f acc s = 
  let len = String.length s in
    if len < 1 then acc
    else 
      let rec work idx acc = 
        if idx < len then work (idx+1) (f acc s.[idx])
        else acc
      in work 0 acc

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

