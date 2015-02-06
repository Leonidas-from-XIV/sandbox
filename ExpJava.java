abstract class Exp {
	public abstract int accept(EvalVisitorJava v);

	static class Const extends Exp {
		int value;
		public Const(int val) {
			value = val;
		}
		public int accept(EvalVisitorJava v) {
			return v.visit(this);
		}
	}

	static abstract class BinExp extends Exp {
		Exp left;
		Exp right;

		public BinExp(Exp l, Exp r) {
			left = l;
			right = r;
		}

		public int accept(EvalVisitorJava v) {
			return v.visit(this);
		}
	}

	static class Sum extends BinExp {
		public Sum(Exp l, Exp r) {
			super(l, r);
		}

		public int accept(EvalVisitorJava v) {
			return v.visit(this);
		}
	}

	static class Prod extends BinExp {
		public Prod(Exp l, Exp r) {
			super(l, r);
		}

		public int accept(EvalVisitorJava v) {
			return v.visit(this);
		}
	}

	static class EvalVisitorJava {
		int visit(Const c) {
			return c.value;
		}

		int visit(Sum s) {
			return s.left.accept(this) + s.right.accept(this);
		}

		int visit(Prod p) {
			return p.left.accept(this) * p.right.accept(this);
		}

		int visit(Exp e) {
			return e.accept(this);
		}
	}
}

class ExpJava {
	public static void main(String[] args) {
		Exp e = new Exp.Sum(new Exp.Const(1),
				new Exp.Prod(new Exp.Sum(new Exp.Const(2),
						new Exp.Const(5)),
					new Exp.Const(3)));
		System.out.println(new Exp.EvalVisitorJava().visit(e));
	}
}
