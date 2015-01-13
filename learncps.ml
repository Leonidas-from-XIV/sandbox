(*
 * In which I implement stuff in Continuation Passing Style
 * ocamlbuild learncps.native
 *)

let sum lat =
  let rec sum' lat k =
    match lat with
    | [] -> k 0
    | x::xs -> sum' xs (fun a -> k (a + x)) in
  sum' lat (fun x -> x)

let mul lat =
  let rec mul' lat k =
    match lat with
    | [] -> k 0
    | [x] -> k x
    | x::xs -> mul' xs (fun a -> k (x * a)) in
  mul' lat (fun x -> x)

let map f lat =
  let rec map' lat k =
    match lat with
    | [] -> k []
    | x::xs -> map' xs (fun a -> k (f x::a)) in
  map' lat (fun x -> x)

let filter f lat =
  let rec filter' lat k =
    match lat with
    | [] -> k []
    | x::xs -> filter' xs (fun a -> k (if f x then x::a else a)) in
  filter' lat (fun x -> x)

(* let rec fold f acc lat = *)
(*   match lat with *)
(*   | [] -> acc *)
(*   | x::xs -> fold f (f x acc) xs *)

let foldr f lat acc =
  let rec foldr' lat k =
    match lat with
    | [] -> k acc
    | x::xs -> foldr' xs (fun a -> k (f x a)) in
  foldr' lat (fun x -> x)

let intercalate e lat =
  let rec intercalate' lat k =
    match lat with
    | [] -> k []
    | [x] -> k [x]
    | x::xs -> intercalate' xs (fun a -> k (x::e::a)) in
  intercalate' lat (fun x -> x)

let sum lat =
  List.fold_left (+) 0 lat

let rec accumulate n k = match n with
  | 0 -> k [0]
  | n -> accumulate (n-1) (fun x -> k (n::x))

(* accumulate 42 sum *)

let () =
  let is_capitalized s =
    s |> String.lowercase <> s
  in
  let v = ["   Hello "; "whatever "; " CPS" ; " World  "]
    |> map String.trim
    |> filter is_capitalized
    |> intercalate " "
  in
  foldr (^) v "!"
    |> print_endline
