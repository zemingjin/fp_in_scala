package chapter_6

import scala.util.Random

object Chapter6 {
  val random = new Random()

  def main(params: Array[String]): Unit = {
    println(random.nextDouble)
    println(random.nextInt)
    println(random.nextInt(5))
  }
}
