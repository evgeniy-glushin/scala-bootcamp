object Functors extends App {
  trait Functor[C[_]]:
    def map[A, B](container: C[A])(f: A => B): C[B]

  trait Semigroup[A]:
    def combine(a: A, b: A): A

  trait Monoid[A] extends Semigroup[A]:
    def empty: A

  given listFunctor: Functor[List] with
    override def map[A, B](container: List[A])(f: A => B): List[B] = container.map(f)

  // generic code can be applied for any mappable type
  def do10x[C[_]](container: C[Int])(using functor: Functor[C]): C[Int] =
    functor.map(container)(_ * 10)

  // generic map for C[_]
  extension [C[_], A, B](container: C[A])(using functor: Functor[C])
    def map(f: A => B) = functor.map(container)(f)

  val list = List(2,3,4)

  println(do10x(List(3,4,5)))

  object IntInstances:
    given intInstance: Monoid[Int] with
      def combine(a: Int, b: Int): Int = a + b
      def empty: Int = 0

  object StringInstances:
    given stringInstance: Monoid[String] with
      def combine(a: String, b: String): String = a + b
      def empty: String = ""

  import IntInstances.given
  import StringInstances.given

  object SemigroupSyntax:
    extension [T](a: T)
      def |+|(b: T) (using semigroup: Semigroup[T]) = semigroup.combine(a,b)

  import SemigroupSyntax.*
  def sum[A : Semigroup](list: List[A]) =
    list.reduce(_ |+| _)

  println(sum(List(5,6,7)))

  val nOption = Option(2)
  val cOption = Option('c')

  val combo = for
    n <- nOption
    c <- cOption
  yield s"$n - $c"

  val combo2 = nOption.flatMap(n => cOption.map(c => s"$n - $c"))

  println(combo)
  println(combo2)


}
