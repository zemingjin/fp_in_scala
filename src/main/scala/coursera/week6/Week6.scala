package coursera.week6

import java.io.File

import scala.io.Source

object Week6 {
  val array = Array(1, 2, 3, 44)

  val s = "Hello World"

  def toString[T](a: Array[T]): String = s"Array(${a.foldLeft("")((t, u) => t + u.toString + ",")})"

  val l = List(1, 2, 3)

  def combs(M: Int, N: Int): List[(Int, Int)] = (1 to M).flatMap(x => (1 to N).map(y => (x, y))).toList

  def scalarProduct(l1: Vector[Double], l2: Vector[Double]): Double = (l1 zip l2).map{ case (x, y) => x * y }.sum

  def isPrime(N: Int): Boolean = (2 until N).forall(n => N % n != 0)

  def primePairs(N: Int): IndexedSeq[(Int, Int)] =
    for {
      i <- 1 until N
      j <- 1 until N
      if isPrime(i + j) && i < j
    } yield (i, j)

  def scalarProductViaFor(l1: Vector[Double], l2: Vector[Double]): Double = (for ((x, y) <- l1 zip l2) yield x * y).sum

  val set = (1 to 6).toSet
  val fruits = Set("apple", "banana", "pear")

  val romanNumerals: Map[String, Int] = Map("I" -> 1, "V" -> 5, "X" -> 10)
  val capitalOfCountry: Map[String, String] = Map("US" -> "Washington", "Switzerland" -> "Bern")

  def showCapital(country: String): String = capitalOfCountry.get(country) match {
    case Some(capital) => capital
    case None => s"No capital found for '$country'"
  }

  val in: Source = Source.fromFile(new File("src/main/resources/linuxwords.txt"))
  val words: List[String] = in.getLines.toList filter (_.forall(_.isLetter))

  val mnem = Map('2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
                 '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")
  var charCode: Map[Char, Char] = for {
    (digit, str) <- mnem
    chr <- str
    } yield chr -> digit

  def wordCode(word: String): String = word.toUpperCase map charCode
  val wordsForNum: Map[String, Seq[String]] = words groupBy wordCode withDefaultValue Seq()
  def encode(number: String): Set[List[String]] = // wordsForNum.get(number).map(s => s.toList).toSet
    if (number.isEmpty) Set(List())
    else {
      for {
        split <- 1 to number.length
        word <- wordsForNum(number take split)
        rest <- encode(number drop split)
      } yield word :: rest
    }.toSet

  def transkate(number: String): Set[String] = encode(number).map(_ mkString " ")
}
