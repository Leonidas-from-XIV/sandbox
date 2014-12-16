(*
 * ocamlbuild markov.native
 *)

module LazyList = BatLazyList
module IO = BatIO
module String = BatString
module List = BatList
module Map = BatMap
module Random = BatRandom

module KK = struct
  type t = string * string

  let compare (a1, a2) (b1, b2) =
    if compare a1 b1 == 0 then compare a2 b2 else compare a1 b1
end

module KKMap = Map.Make(KK)

let words () =
  let words = IO.read_all IO.stdin in
  String.nsplit words ~by:" "

let rec partition n step coll =
  match List.take n coll with
    | taken when List.length taken == n -> taken::partition n step (List.drop step coll)
    | _ -> []

let kv = function
  | [k1; k2; v] -> ((k1, k2), v)
  | _ -> failwith "oh well"

let add_to_table tbl (k, v) =
  let oldval = match KKMap.find k tbl with
    | xs -> xs
    | exception Not_found -> [] in
  KKMap.add k (v::oldval) tbl

let markov_link tbl ((_, k1), k2) =
  match KKMap.find (k1, k2) tbl with
    | xs -> let next = Random.choice xs in
      ((k1, k2), next)
    | exception Not_found -> raise LazyList.No_more_elements

let markov_chain tbl =
  let linked = markov_link tbl in
  LazyList.from_loop (("a", "b"), "c") linked

let transitions triplets =
  let parts = List.map kv triplets in
  List.fold_left add_to_table KKMap.empty parts
