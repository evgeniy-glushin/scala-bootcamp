import cats.{Apply, Semigroupal}

object SemigroupalExercise extends App {
  import cats.Monad
  import cats.instances.list._
  import cats.Semigroupal

  def productWithMonad[F[_], A, B](fa : F[A], fb: F[B])(implicit monad: Monad[F]): F[(A,B)] =
    val fab = monad.map[B, (A, B)](fb)
    monad.flatMap(fa)(a => fab(b => (a, b)))

  val product = productWithMonad(List(1,2,3), List("a", "b"))

  product foreach(println)

  val zipSemigroupal: Semigroupal[List] = new Semigroupal[List] {
    override def product[A, B](fa: List[A], fb: List[B]): List[(A, B)] = fa.zip(fb)
  }

  trait MyFlatMap[M[_]] extends Apply[M]:
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

    def ap[A, B](wf: M[A => B])(wa: M[A]): M[B] =
      flatMap(wa)(a => map(wf)(f => f(a)))
}
