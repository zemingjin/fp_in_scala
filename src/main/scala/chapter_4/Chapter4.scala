package chapter_4

object Chapter4 {
  def failingFn2(i: Int): Int = {
    try {
    val x = 42 + 5
    x + ((throw new Exception("fail!")): Int) }
  catch { case e: Exception => 43 }
  }
}
