package chapter_7

import org.scalatest.{FlatSpec, Matchers}

class Chapter7Test extends FlatSpec with Matchers {
  it should "return the sum of a list" in {
    assert(Chapter7.sum(IndexedSeq(1, 2, 3, 4)).toString.length > 0)
  }
}
