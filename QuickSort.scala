/* Natural number type */
class Nat(val value: Int) extends Ordered[Nat] {
  require(value >= 0)
  
  // define comparison operators in one step
  override def compare(that: Nat): Int = this.value - that.value

  def toInt() = this.value
  override def toString() = this.value.toString()
}

object QuickSort {

  def main(args: Array[String]) {
    val unsorted_ints = List(3, 5, 7, 2, 9, 8, 1, 6, 4)
    val unsorted = (for(item <- unsorted_ints) yield new Nat(item))
    println(quicksort(unsorted))
  }

  def kls(s: List[Nat], p: Nat): List[Nat] =
    if (s.length == 0) s else
      if (s.first <= p) s.first :: kls(s.tail, p) else kls(s.tail, p)
	
  def gls(s: List[Nat], p: Nat): List[Nat] =
    if (s.length == 0) s else
      if (s.first >= p) s.first :: gls(s.tail, p) else gls(s.tail, p)

  def quicksort(s: List[Nat]): List[Nat] =
    if (s.length <= 1) s else {
      val p = s.first
      quicksort(kls(s.tail, p)) ++ List(p) ++ quicksort(gls(s.tail, p))
    }
}
