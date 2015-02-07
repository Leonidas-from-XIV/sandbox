/* so, traits are not mixins… */

trait Foo {
  def f = println("Foo.f");
}

trait Bar {
  def f = println("Bar.f");
}

trait Baz {
  def f = println("Baz.f");
  // does not work because can't be statically bound
  // def f = super.f;
}

class FooBarBaz extends Foo with Bar with Baz {
  override def f = super.f;
}

class FooBazBar extends Foo with Baz with Bar {
  override def f = super.f;
}

object Traitest {
  def main(args: Array[String]) {
    val frz = new FooBarBaz();
    frz.f;
    val fzr = new FooBazBar();
    fzr.f;
  }
}
