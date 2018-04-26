package chapter_4

import org.scalatest.{FlatSpec, Matchers}

class Chapter4Test extends FlatSpec with Matchers {
  it should "return an exception" in {
    assert(Chapter4.failingFn2(12).toString == "43")
  }

}
