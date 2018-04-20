package chapter_4

import org.scalatest.{FlatSpec, Matchers}

class EitherTest extends FlatSpec with Matchers {
  it should "return either result from mean" in {
    assert(mean(IndexedSeq()) == Left("mean of empty list!"))
    assert(mean(IndexedSeq(1, 3)) == Right(2.0))
  }

  it should "return eigher result from safeDiv" in {
    assert(safeDiv(5, 2) == Right(2))
    assert(safeDiv(5, 0).isInstanceOf[Left[Exception]])
  }
}
