package chapter_4

import org.scalatest.{FlatSpec, Matchers}

class OptionTest extends FlatSpec with Matchers {
  it should "return a mapped option" in assert(Option(2).map(_ => 3) == Option(3))
  it should "return a default value for getOrElse" in assert(None.getOrElse(2) == 2)
  it should "return an Option for orElse" in assert(None.orElse(Option(1)) == Option(1))
  it should "return flatten the option from flatMap" in assert(Option(1).flatMap(_ => Option(2)) == Option(2))
  it should "return the result of filter" in {
    assert(Option(1).filter(a => a % 2 == 0) == None)
    assert(Option(4).filter(a => a % 2 == 0) == Option(4))
  }
}
