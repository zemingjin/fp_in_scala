package tour

class GParent
class Parent extends GParent
class Child extends Parent

case class Box[+A]() {
}

case class Box2[-A]()


class Variance {
  def foo(x: Box[Parent]): Box[Parent] = identity(x)
  def bar(x: Box2[Parent]): Box2[Parent] = identity(x)

  foo(Box[Child]())
//  foo(Box[GParent]())

//  bar(Box2[Child]())
  bar(Box2[GParent]())
}
