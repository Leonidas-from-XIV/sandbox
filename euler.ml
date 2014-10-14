(* Calculates Euler number
 *
 * Requires Gmp library, compile it with
 * ocamlbuild -pkg gmp euler.byte
 * -or-
 * ocamlbuild -pkg gmp euler.native
 *)

let prec = 1_000_000
let max_n = 205_211
let to_string = Gmp.F.to_string_base_digits ~base:10 ~digits:1000

let euler_fraction n =
  let open Gmp.Z in
  let one = from_int 1 in
  let numerator = ref one in
  let denominator = ref one in
  for i = 1 to n do
    numerator := add_ui (mul_ui !numerator i) 1;
    denominator := mul_ui !denominator i;
  done;
  (!numerator, !denominator)

let f () =
  let (num, den) = euler_fraction max_n in
  let znum = Gmp.F.from_z_prec ~prec num in
  let zden = Gmp.F.from_z_prec ~prec den in
  let euler = Gmp.F.div_prec ~prec znum zden in
  print_endline @@ to_string euler

let _ =
  f ()
