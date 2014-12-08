(*
 * ocamlbuild c3.native
 *)

type 'a hierarchy = Class of ('a * 'a hierarchy list)

let head = function
  | [] -> []
  | x -> [List.hd x]

let tail = function
  | [] -> []
  | x -> [List.tl x]

let concat_map f l = List.concat @@ List.map f l

let head_not_in_tails (l : 'a hierarchy list list) =
  let heads = concat_map head l in
  let tails = concat_map tail l in
  let find_a_head_that_is_not_in_tails acc v = match acc with
    | Some x -> Some x
    | None -> if List.exists (List.mem v) tails then None else Some v
  in
  List.fold_left find_a_head_that_is_not_in_tails None heads

let remove to_remove l =
  List.filter (fun e -> e != []) (List.map (List.filter (fun e -> e != to_remove)) l)

exception No_linearization

let rec merge (l : 'a hierarchy list list) =
  match head_not_in_tails l with
  | Some c -> (match remove c l with
    | [] -> [c]
    | n -> c :: merge n)
  | None -> raise No_linearization

let rec c3_exn = function
  | Class (_, []) as res -> [res]
  | Class (_, parents) as res -> res :: (merge @@ (List.map c3_exn parents) @ [parents])

let c3 cls = match c3_exn cls with
  | exception No_linearization -> None
  | v -> Some v

let () =
  let rec show_hierarchy = function
    | Class (n, _) -> n
  and show_hierarchy_list lat =
    "[" ^ String.concat ", " (List.map show_hierarchy lat) ^ "]"
  and o = Class ("O", [])
  and a = Class ("A", [o])
  and b = Class ("B", [o])
  and c = Class ("C", [o])
  and d = Class ("D", [o])
  and e = Class ("E", [o])
  and k1 = Class ("K1", [a; b; c])
  and k2 = Class ("K2", [d; b; e])
  and k3 = Class ("K3", [d; a])
  and z = Class ("Z", [k1; k2; k3])
  in
  print_endline @@ show_hierarchy z;
  match c3 z with
  | Some v -> print_endline @@ show_hierarchy_list v
  | None -> print_endline "No linearization"
