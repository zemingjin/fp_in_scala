package chapter_4

class Chapter4 {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def mean_1(xs: IndexedSeq[Double], onEmpty: Double): Double =
    if (xs.isEmpty) onEmpty
    else xs.sum / xs.length

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
    val optAge: Option[Int] = Try(age.toInt)
    val optTickets: Option[Int] = Try(numberOfSpeedingTickets.toInt)
    insuranceRateQuote(optAge, optTickets)
  }

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }
}
