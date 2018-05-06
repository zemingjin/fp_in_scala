package tour

import scala.language.implicitConversions

class ImplicitConversions {
  implicit def list2ordered[A](x: List[A])(implicit elem2ordered: A => Ordered[A]): Ordered[List[A]] =
    new Ordered[List[A]] {
      //replace with a more useful implementation
      def compare(that: List[A]): Int = 1
    }

}
