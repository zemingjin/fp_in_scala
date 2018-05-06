package tour

trait Cloneable extends java.lang.Cloneable {
  override def clone(): Cloneable = {
    super.clone().asInstanceOf[Cloneable]
  }
}
trait Resetable {
  def reset(): Unit = {}
}

trait User {
  def username: String
}

trait Tweeter {
  this: User => def tweet(tweetText: String): Unit = println(s"$username: $tweetText")
}

class VerifiedTweeter(val _username: String) extends Tweeter with User {
  def username = s"real ${_username}"
}

class Tour {
  def cloneAndReset(obj: Cloneable with Resetable): Cloneable = {
    val cloned = obj.clone()
    obj.reset()
    cloned
  }

}

