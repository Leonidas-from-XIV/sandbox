/* java -cp mjc-1_3_2.jar org.multijava.mjc.Main -G ExpMultiJava.java */
abstract class Exp {
	static class Const extends Exp {
		int value;
		public Const(int val) {
			value = val;
		}
	}

	static abstract class BinExp extends Exp {
		Exp left;
		Exp right;

		public BinExp(Exp l, Exp r) {
			left = l;
			right = r;
		}
	}

	static class Sum extends BinExp {
		public Sum(Exp l, Exp r) {
			super(l, r);
		}
	}

	static class Prod extends BinExp {
		public Prod(Exp l, Exp r) {
			super(l, r);
		}
	}

	static class Evaluator {
		int eval(Exp e) { return 0; }

		int eval(Exp@Const c) {
			return c.value;
		}

		int eval(Exp@Sum s) {
			return eval(s.left) + eval(s.right);
		}

		int eval(Exp@Prod p) {
			return eval(p.left) * eval(p.right);
		}
	}
}

class ExpMultiJava {
	public static void main(String[] args) {
		Exp e = new Exp.Sum(new Exp.Const(1),
				new Exp.Prod(new Exp.Sum(new Exp.Const(2),
						new Exp.Const(5)),
					new Exp.Const(3)));
		System.out.println(new Exp.Evaluator().eval(e));
	}
}
