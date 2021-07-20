(*
   Stack-safe re-implementation of the popular functions of the standard
   List module.
*)

let map f l =
  List.rev_map f l |> List.rev

let flatten ll =
  match ll with
  | [] -> []
  | l ->
      List.fold_left (fun acc l -> List.rev_append l acc) [] ll
      |> List.rev

let concat = flatten

let fold_right f l acc =
  match l with
  | [] -> acc
  | l ->
      List.fold_left (fun acc l -> f l acc) acc (List.rev l)

let append a b =
  List.rev_append (List.rev a) b
