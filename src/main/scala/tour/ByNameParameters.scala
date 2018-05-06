package tour

class ByNameParameters {
  def calculate(input: => Int): Int = input * 37

  def whileLoop(condition: => Boolean)(body: => Unit): Unit = {
    if (condition) {
      body
      whileLoop(condition)(body)
    }
  }
}
