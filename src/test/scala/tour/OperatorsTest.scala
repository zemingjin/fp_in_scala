package tour

import org.scalatest.{FlatSpec, Matchers}

class OperatorsTest extends FlatSpec with Matchers {
  val vector1 = Vec(1.0, 1.0)
  val vector2 = Vec(2.0, 2.0)

  it should "return a sum of 2 vectors" in {
    assert((vector1 + vector2).toString == "Vec(3.0, 3.0)")
  }
}
