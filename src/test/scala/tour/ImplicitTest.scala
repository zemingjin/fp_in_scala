package tour

import org.scalatest.{FlatSpec, Matchers}

class ImplicitTest extends FlatSpec with Matchers {
  implicit val intMonoid: Monoid[Int] = new Monoid[Int] {
    def add(x: Int, y: Int): Int = x + y
    def unit: Int = 0
  }

  implicit val stringMonoid: Monoid[String] = new Monoid[String] {
    def add(x: String, y: String): String = x concat y
    def unit: String = ""
  }

  val imp = new Implicit
  it should "return a sum" in {
    assert(imp.sum(List(1, 2, 3)).toString == "6")
    assert(imp.sum(List("a", "b", "c")) == "abc")
  }
}
