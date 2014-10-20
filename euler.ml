(* Calculates Euler number
 *
 * Requires Gmp library, compile it with
 * ocamlbuild -pkg gmp -pkg zarith euler.byte
 * -or-
 * ocamlbuild -pkg gmp -pkg zarith euler.native
 *)

let prec = 1_000_000
let max_n = 205_211
let mode = Gmp.GMP_RNDN

let euler_fraction n =
  let open Z in
  let numerator = ref one in
  let denominator = ref one in
  for i = 1 to n do
    numerator := succ (!numerator * (of_int i));
    denominator := (of_int i) * !denominator;
  done;
  (!numerator, !denominator)

let f () =
  let (num, den) = euler_fraction max_n in
  let znum = Gmp.Z.from_string @@ Z.to_string num in
  let zden = Gmp.Z.from_string @@ Z.to_string den in
  let euler = Gmp.FR.from_q_prec ~mode ~prec @@ Gmp.Q.from_zs znum zden in
  print_endline @@ Gmp.FR.to_string_base_digits ~mode ~base:10 ~digits:0 euler

let () = f ()
