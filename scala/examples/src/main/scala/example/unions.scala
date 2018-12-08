package example

class A()
class B()

object Demo {

  def safe_test(foo: Any): Unit = {
    foo match {
      case ab: (A | B) =>
        println("got A | B")
      case _ =>
        println("got Any")
    }
  }

  type AorB = A | B

  def unsafe_test(foo: Any): Unit = {
    foo match {
      // TODO: Warning is generated that AorB cannot be checked at runtime
      case ab: AorB =>
        println("got AorB")
      case _ =>
        println("got Any")
    }
  }

  def main(args: Array[String]): Unit = {
    val a = new A()
    val b = new B()
    val unit = ()

    println("safe test -")
    safe_test(a)
    safe_test(b)
    safe_test(unit)

    println("unsafe test -")
    unsafe_test(a)
    unsafe_test(b)
    unsafe_test(unit)
  }
}