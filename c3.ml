(*
 * ocamlbuild c3.native -use-ocamlfind -package ppx_deriving.show
 *)

type hierarchy = Class of (string * hierarchy list) [@@deriving show]

let o = Class ("O", [])
let a = Class ("A", [o])
let b = Class ("B", [o])
let c = Class ("C", [o])
let d = Class ("D", [o])
let e = Class ("E", [o])
let k1 = Class ("K1", [a; b; c])
let k2 = Class ("K2", [d; b; e])
let k3 = Class ("K3", [d; a])
let z = Class ("Z", [k1; k2; k3])

let hds = function
  | [] -> []
  | x -> [List.hd x]

let tls = function
  | [] -> []
  | x -> [List.tl x]

let head_not_in_tails (l : hierarchy list list) =
  let heads = List.flatten @@ List.map hds l in
  let tails = List.flatten @@ List.map tls l in
  let find_a_head_that_is_not_in_tails acc v = match acc with
    | Some x -> Some x
    | None -> if List.exists (List.mem v) tails then None else Some v
  in
  List.fold_left find_a_head_that_is_not_in_tails None heads

let remove to_remove l =
  List.filter (fun e -> e != []) (List.map (List.filter (fun e -> e != to_remove)) l)

let rec merge (l : hierarchy list list) =
  match head_not_in_tails l with
  | Some c -> (match remove c l with
    | [] -> [c]
    | n -> c :: (merge n))
  | None -> failwith "Can't be linearized"

let rec c3 = function
  | Class (_, []) as res -> [res]
  | Class (_, parents) as res -> res :: (merge @@ (List.map c3 parents) @ [parents])

let show_hierarchy = function
  | Class (n, _) -> n

let show_hierarchy_list lat =
  "[" ^ String.concat ", " (List.map show_hierarchy lat) ^ "]"

let () =
  print_endline @@ show_hierarchy z;
  print_endline @@ show_hierarchy_list @@ c3 z
