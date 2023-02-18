import cats.Functor
import cats.syntax.functor._


object FunctorExercise extends App {
  trait Tree[+T]
  case class Leaf[+T](value: T) extends Tree[T]
  case class Branch[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]

  implicit object TreeFunctor extends Functor[Tree]:
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match
      case Leaf(v) => Leaf(f(v))
      case Branch(v, left, right) => Branch(f(v), map(left)(f), map(right)(f))

  def do10x[F[_]](container: F[Int])(implicit functor: Functor[F]) = functor.map(container)(_ * 10)

  def do10xV2[F[_] : Functor](container: F[Int]) = container.map(_ * 10)

  val tree: Tree[Int] = Branch (
    10, Leaf(15), Branch(
      20, Leaf(25), Leaf(30)))

  println(tree.map(_ + 1 ))
  println(do10x(tree))
}
