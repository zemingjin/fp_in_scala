package coursera.week6

import org.scalatest.{FlatSpec, Matchers}

class PolynomialsTest extends FlatSpec with Matchers {
  import coursera.week6.Polynomials._

  it should "return a Poly" in {
    println(p1)
    println(p2)
    println(p1 + p2)

    assert((p1 + p2).toString == "6.2x^5 + 11.0x^3 + 2.0x^1 + 3.0x^0")
  }

}
