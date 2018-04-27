package chapter_5

import chapter_5.Chapter5
import org.scalatest.{FlatSpec, Matchers}

class Chapter5Test extends FlatSpec with Matchers {
  it should "return a list" in {
    assert(Chapter5.list.toString() == "List(36, 42)")
  }

  it should "print a character according to the flag" in {
    assert(Chapter5.if2(true, "a", "b") == "a")
    assert(Chapter5.if2(false, "a", "b") == "b")
  }

  it should "return a value twice as given" in {
    assert(Chapter5.maybeTwice(true, { println("hi"); 1+1}) == 4)
    assert(Chapter5.maybeTwice(false, 2) == 0)
  }

}
