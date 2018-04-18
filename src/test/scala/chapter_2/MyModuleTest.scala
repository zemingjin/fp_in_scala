package chapter_2

import org.scalatest.{FlatSpec, Matchers}

class MyModuleTest extends FlatSpec with Matchers {

  it should "return the absolute value of charpter_2.MyModuleTest.formatAbs" in MyModule.main(null)

  it should "return the factorial of a given number" in {
    assert(MyModule.factorial(1) == 1)
    assert(MyModule.factorial(2) == 2)
    assert(MyModule.factorial(7) == 5040)
  }

  it should "return the fibonacci number of a given integer" in {
    assert(MyModule.fibonacci(0) == 0)
    assert(MyModule.fibonacci(1) == 1)
    assert(MyModule.fibonacci(2) == 1)
    assert(MyModule.fibonacci(3) == 2)
    assert(MyModule.fibonacci(4) == 3)
    assert(MyModule.fibonacci(5) == 5)
  }

  it should "return the result of whether the given list is sorted" in {
    assert(MyModule.isSorted(Array(1, 3, 4), (a: Int, b: Int) => a <= b))
    assert(!MyModule.isSorted(Array(1, 8, 4), (a: Int, b: Int) => a <= b))
    assert(!MyModule.isSorted(Array(8, 1, 4), (a: Int, b: Int) => a <= b))
  }
}
