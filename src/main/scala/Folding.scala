import cats.Monoid

object Folding extends App {
  object ListExercises {
    def map[A, B](list: List[A])(f: A => B): List[B] =
      list.foldLeft(List.empty[B])((ac, cur) => ac :+ f(cur))

    def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] =
      list.foldLeft(List.empty[B])((ac, cur) => ac ++ f(cur))

    def filter[A](list: List[A])(predicate: A => Boolean): List[A] =
      list.foldLeft(List.empty[A])((ac, cur) => if predicate(cur) then ac :+ cur else ac)

    def combineAll[A](list: List[A])(implicit monoid: Monoid[A]): A =
      list.foldLeft(monoid.empty)(monoid.combine)
  }

  import ListExercises._

  val m = map[Int,Int](List(1,2,3))(_ + 10)
  val f = filter(m)(x => x > 12)
  val ca = combineAll(m)

  println(m)
  println(f)
  println(ca )
}
