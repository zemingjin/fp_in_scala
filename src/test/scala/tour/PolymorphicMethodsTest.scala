package tour

import org.scalatest.{FlatSpec, Matchers}

class PolymorphicMethodsTest extends FlatSpec with Matchers {
  val polymorphicMethods = new PolymorphicMethods
  it should "return a duplicate list" in {
    assert(polymorphicMethods.listOfDuplicates(3, 4).toString() == "List(3, 3, 3, 3)")
    assert(polymorphicMethods.listOfDuplicates("La", 4).toString() == "List(La, La, La, La)")
  }
}
