package chapter_4

import org.scalatest.{FlatSpec, Matchers}

class FailingTest extends FlatSpec with Matchers {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!")
    try {
      val x = 42 + 5
      x+y
    }
    catch { case e: Exception => 43 }
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int)
    }
    catch { case e: Exception => 43 }
  }
  it should "return an error code " in assert(failingFn2(12) == 43)
  failingFn(12)

}
