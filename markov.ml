(*
 * ocamlbuild markov.native -package batteries
 *)

open Batteries

module KKMap = Map.Make(struct
  type t = string * string

  let compare (a1, a2) (b1, b2) =
    if compare a1 b1 == 0 then compare a2 b2 else compare a1 b1
end)

let words () =
  let words = IO.read_all IO.stdin in
  String.nsplit words ~by:" "

let rec partition n step coll =
  match List.take n coll with
    | taken when List.length taken == n ->
        taken::partition n step (List.drop step coll)
    | _ -> []

let markov_chain tbl =
  let markov_link (k1, k2) =
    match KKMap.find (k1, k2) tbl with
      | xs -> let next = Random.choice (List.enum xs) in
        (next, (k2, next))
      | exception Not_found -> raise LazyList.No_more_elements in

  let (start_key, _) = Random.choice (KKMap.enum tbl) in
  LazyList.from_loop start_key markov_link

let prepare_chain triplets =
  let triplet_to_tuple = function
    | [k1; k2; v] -> ((k1, k2), v)
    | _ -> failwith "Not a triplet" in

  let add_to_table tbl (k, v) =
    let oldval = match KKMap.find k tbl with
      | xs -> xs
      | exception Not_found -> [] in
    KKMap.add k (v::oldval) tbl in

  List.map triplet_to_tuple triplets
    |> List.fold_left add_to_table KKMap.empty

let () =
  let number = ref 25 in
  let speclist = [
    ("-n", Arg.Int (fun x -> number := x), "Number of words to generate")
    ] in
  Arg.parse speclist print_endline "Markov chain generator";

  words ()
    |> partition 3 1
    |> prepare_chain
    |> markov_chain
    |> LazyList.take !number
    |> LazyList.to_list
    |> String.join " "
    |> print_endline
