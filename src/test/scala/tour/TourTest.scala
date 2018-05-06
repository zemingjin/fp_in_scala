package tour

import org.scalatest.{FlatSpec, Matchers}

class TourTest extends FlatSpec with Matchers {
  it should "diplay an combined message" in {
    val realBeyonce = new VerifiedTweeter("Beyonce")
    realBeyonce.tweet("Just spilled my glass of lemonade")

  }
}
