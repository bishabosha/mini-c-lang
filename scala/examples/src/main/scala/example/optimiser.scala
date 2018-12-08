package example

case class CC(a: Int, b: Object)

object Optimise {
  def foo(x: Any): Int = {
    val (a, b) = x match {
      case CC(s @ 1, CC(t, _)) =>
        (s , 2)
      case _ => (42, 43)
    }
    a + b
  }

  def booleans(a: Object) = {
    val (b1, b2) = (a.isInstanceOf[CC], a.isInstanceOf[List[Int]])
    (b1, b2) match {
      case (true, true) => true
      case (false, false) => true
      case _ => false
    }
  }

  def main(args: Array[String]): Unit = {
    assert(!booleans(CC(1,None)))
    assert(!booleans(List(1)))
    assert(foo(CC(1,CC(99, None))) == 3)
  }
}
