(* Calculates Euler number
 *
 * Requires Gmp library, compile it with
 * ocamlbuild -package gmp euler.byte
 * -or-
 * ocamlbuild -package gmp euler.native
 *)

let one = Gmp.F.from_int 1
let prec = 1_000_000
let max_n = 205_211
let fmul = Gmp.F.mul_prec_ui ~prec
let fdiv = Gmp.F.div_prec ~prec
let fadd = Gmp.F.add_prec ~prec
let to_string = Gmp.F.to_string_base_digits ~base:10 ~digits:1000

let f () =
  let x = ref one in
  let euler = ref one in
  for n = 1 to max_n do
    x := fmul !x n;
    euler := fadd !euler @@ fdiv one !x;
  done;
  print_endline @@ to_string !euler

let _ =
  f ()
