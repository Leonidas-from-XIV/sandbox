(* Calculates Euler number
 *
 * Requires Gmp library, compile it with
 * ocamlbuild -pkg gmp -pkg num euler.byte
 * -or-
 * ocamlbuild -pkg gmp -pkg num euler.native
 *)

let prec = 1_000_000
let max_n = 205_211
let to_string = Gmp.F.to_string_base_digits ~base:10 ~digits:1000

let euler_fraction n =
  let open Big_int in
  let one = big_int_of_int 1 in
  let numerator = ref one in
  let denominator = ref one in
  for i = 1 to n do
    numerator := add_int_big_int 1 (mult_int_big_int i !numerator);
    denominator := mult_int_big_int i !denominator;
  done;
  (!numerator, !denominator)

let f () =
  let (num, den) = euler_fraction max_n in
  let znum = Gmp.F.from_string (Big_int.string_of_big_int num) in
  let zden = Gmp.F.from_string (Big_int.string_of_big_int den) in
  let euler = Gmp.F.div_prec ~prec znum zden in
  print_endline @@ to_string euler

let () = f ()
