extern crate num;
use num::gcd;
use num::Integer;
use std::num::{Zero, one};

fn phi<T: Integer + Clone + ToPrimitive>(n: T) -> T {
	let mut hits: T = Zero::zero();
	let one: T = one();
	for a in range(one.clone(), n + one) {
		if gcd(a, n.clone()) == one {
			hits = hits + one;
		}
	}
	hits
}

fn extended_euclid(a: i64, b: i64) -> (i64, i64, i64) {
	if b == 0 {
		return (a, 1, 0);
	}
	let (d1, s1, t1) = extended_euclid(b, a % b);
	let (d, s, t) = (d1, t1, s1 - (a/b) * t1);
	(d, s, t)
}

fn gen_keys(p: u64, q: u64) {
	let n = p*q;
	let p = (p-1)*(q-1);

	let mut e = 0;
	for i in range(2, p) {
		if gcd(i, p) == 1 {
			e = i;
			break;
		}
	}

	let (_, d, _) = extended_euclid(e as i64, p as i64);
	println!("phi(n) {}", p);
	println!("e {}", e);
	println!("d {}", d);

}

#[test]
fn key_generation() {
	gen_keys(11, 13);
	assert!(false);
}
