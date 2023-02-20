import cats.data.State

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

  val program: State[Int, (Int, Int, Int)] = for
    a <- get[Int]
    _ <- set(a + 10)
    b <- get[Int]
    _ <- modify[Int](_ + 43)
    c <- inspect[Int, Int](_ * 2)
  yield (a, b, c)

}
