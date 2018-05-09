package coursera.week6

import org.scalatest.{FlatSpec, Matchers}

class nqueensTest extends FlatSpec with Matchers {
  import coursera.week6.nqueens._

  it should "return a list of solutions" in {
    assert(queens(4).toString == "Set(List(2, 0, 3, 1), List(1, 3, 0, 2))")
    queens(4).foreach(show)
  }

}
