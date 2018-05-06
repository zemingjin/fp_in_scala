package tour

case class Vec(x: Double, y: Double) {
  def +(that: Vec) = Vec(this.x + that.x, this.y + that.y)

  override def toString: String = s"Vec($x, $y)"
}

case class MyBool(x: Boolean) {
  def and(that: MyBool): MyBool = if (x) that else this
  def or(that: MyBool): MyBool = if (x) this else that
  def negate: MyBool = MyBool(!x)
  def not(x: MyBool): MyBool = x.negate
  def xor(x: MyBool, y: MyBool): MyBool = (x or y) and not(x and y)
}
class Operators {

}
