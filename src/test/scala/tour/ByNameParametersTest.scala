package tour

import org.scalatest.{FlatSpec, Matchers}

class ByNameParametersTest extends FlatSpec with Matchers {
  val byNameParameters = new ByNameParameters
  it should "run the loop" in {
    var i = 3
    byNameParameters.whileLoop(i > 0)({ println(i); i -= 1 })
  }
}
