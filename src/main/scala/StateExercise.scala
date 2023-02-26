import cats.Foldable
import cats.data.{OptionT, Reader, State}

object StateExercise extends App {
  case class ShoppingCart(items: List[String], total: Double)

  def addToCart(item: String, price: Double): State[ShoppingCart, Double] =
    State(cart  => (ShoppingCart(cart.items :+ item, cart.total + price), cart.total + price))

  val myCart = for
    _ <- addToCart("iphone", 999)
    _ <- addToCart("iwatch", 499)
    total <- addToCart("bmw", 60000)
  yield total

  println(myCart.run(ShoppingCart(List(), 0)).value)
  //println(cart.run(ShoppingCart(List("iwatch"), 499)).value)

  def inspect[A, B](f: A => B): State[A, B] = State(s => (s, f(s)))
  def get[A]: State[A, A] = State(s => (s, s))
  def set[A](value: A): State[A, Unit] = State(_ => (value, ()))
  def modify[A](f: A => A): State[A, Unit] = State(s => (f(s) , ()))

  val add1 = State[Int, Unit](s => (s + 1, ()))
  val mul2 = State[Int, Unit](s => (s * 2, ()))

  val numProgram = for
    _ <- add1
    res <- mul2
  yield res

  println(numProgram.run(10).value)

  import cats.syntax.all.*


  val r1 = Reader((n: Int) => n.toString)
  val r2 = Reader((s: String) => List(s))

  val r3 = r1.andThen(r2)

  val program: State[Int, (Int, Int, Int)] = for
    a <- get[Int]
    _ <- set(a + 10)
    b <- get[Int]
    _ <- modify[Int](_ + 43)
    c <- inspect[Int, Int](_ * 2)
    x = 4 + 5
  yield (a, b, x)


  import cats.Traverse
  import cats.syntax.all.*

  val list: List[Option[Int]] = List(Some(1), Some(2), None)
  val list2 = OptionT[List, Int](list)
  val list3 = List("1", "2").traverse(doWork)
  val list4 = list.traverse(identity)
  val list5 = list.sequence

  println(list2.map(x => s"( $x )"))
  println(list4)

  def doWork(s: String): Option[String] = Some(s)

  import cats.instances.vector._

  val intsNested = List(Vector(1, 2, 3), Vector(4, 5, 6))
  (Foldable[List] compose Foldable[Vector]).combineAll(intsNested)

}
