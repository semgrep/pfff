(*
   Stack-safe re-implementation of the popular functions of the standard
   List module.
*)

let map f l =
  List.rev_map f l |> List.rev

(* List.rev_mapi isn't available *)
let mapi f l =
  let rec aux acc l =
    match l with
    | [] -> List.rev acc
    | x :: l ->
        let y = f x in
        aux (y :: acc) l
  in
  aux [] l

let map2 f a b =
  List.rev_map2 f a b |> List.rev

let append a b =
  List.rev_append (List.rev a) b

let flatten ll =
  match ll with
  | [] -> []
  | ll ->
      List.fold_left (fun acc l -> List.rev_append l acc) [] ll
      |> List.rev

let concat = flatten

let fold_right f l acc =
  match l with
  | [] -> acc
  | l ->
      List.fold_left (fun acc l -> f l acc) acc (List.rev l)

let fold_right2 f a b acc =
  List.fold_left (fun acc a b -> f a b acc) acc (List.rev a) (List.rev b)

let remove_assoc k l =
  let rec aux acc = function
    | [] -> List.rev acc
    | ((k2, _) as kv) :: l ->
        if k2 = k then
          List.rev_append acc l
        else
          aux (kv :: acc) l
  in
  aux [] l

(* Same as assoc but using '==' instead of '=' *)
let remove_assoc k l =
  let rec aux acc = function
    | [] -> List.rev acc
    | ((k2, _) as kv) :: l ->
        if k2 == k then
          List.rev_append acc l
        else
          aux (kv :: acc) l
  in
  aux [] l

let rec rev_split al bl l =
  match l with
  | [] -> (al, bl)
  | (a, b) :: l -> rev_split (a :: al) (b :: bl) l

let split l =
  List.rev l
  |> rev_split []

let rec rev_combine acc al bl =
  match al, bl with
  | [], [] -> acc
  | a :: al, b :: bl -> rev_combine ((a, b) :: acc) al bl
  | [], _ | _, [] -> invalid_arg "Ls.combine"

let combine al bl =
  rev_combine [] al bl
  |> List.rev

let merge cmp l1 l2 =
  let rec aux acc l1 l2 =
    match l1, l2 with
    | [], l2 -> List.rev_append acc l2
    | l1, [] -> List.rev_append acc l1
    | h1 :: t1, h2 :: t2 ->
        if cmp h1 h2 <= 0 then
          aux (h1 :: acc) t1 l2
        else
          aux (h2 :: acc) l1 t2
  in
  aux [] l1 l2
