import cats.{Applicative, Monad}

object Traversing extends App {

  import cats.syntax.applicative._
  import cats.syntax.flatMap._
  import cats.syntax.functor._
  import cats.syntax.apply._

  def listTraverse[F[_] : Monad, A, B](list: List[A])(f : A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (acc, cur) =>
      for
        a <-  acc
        b <- f(cur)
      yield a :+ b
    }

  def listSequence[F[_] : Applicative, A](list: List[F[A]]): F[List[A]] =
    list.foldLeft(List.empty[A].pure[F]) { (acc, cur) =>
      (acc, cur).mapN((a, b) => a :+ b)
    }
}
