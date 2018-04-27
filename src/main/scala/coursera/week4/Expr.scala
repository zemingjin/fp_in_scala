package coursera.week4

trait Expr {
  def eval: Int
  def show: String = this match {
    case Number(n) => n.toString
    case Sum(l, r) => l.show + " + " + r.show
    case Prod(l, r) => l.show + " * " + r.show
  }
}

case class Number(n: Int) extends Expr {
  def eval: Int = n
}

case class Sum(e1: Expr, e2: Expr) extends Expr {
  def eval: Int = e1.eval + e2.eval
}

case class Prod(e1: Expr, e2: Expr) extends Expr {
  def eval: Int = e1.eval * e2.eval
}