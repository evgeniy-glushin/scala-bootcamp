import cats.Monoid

object MonoidExercise extends App {
  case class Expense2(id: Long, amount: Double)

  def combine(a: Expense2, b: Expense2): Expense2 =
    new Expense2(Math.max(a.id, b.id), a.amount + b.amount)

  implicit val expenseMonoid: Monoid[Expense2] = Monoid.instance[Expense2](Expense2(0, 0), combine)

  def combineFold[T](list: List[T])(using monoid: Monoid[T]) = list.fold(monoid.empty)(monoid.combine)

  println(combineFold(List(
    new Expense2(20, 999),
    new Expense2(30, 1)
  )))

  val phonebook = List(
    Map (
      "Alice" -> 235,
      "Bob" -> 647
    ),
    Map (
      "Charlie" -> 865,
      "Daniel" -> 889
    ),
    Map (
      "Tina" -> 123
    )
  )

  println(combineFold(phonebook))
//  implicit val mapMonoid: Monoid[Map[String, Int]] = Monoid.instance[Map[String, Int]](Map(), (a, b) => a ++ b)
//  def combinePhonebook[T](book: List[T])(using monoid: Monoid[T]): T =
//    book.fold(monoid.empty)(monoid.combine)

  case class ShoppingCart(items: List[String], total: Double)

  implicit val shoppingCartMonoid: Monoid[ShoppingCart] =
    Monoid.instance[ShoppingCart](
      ShoppingCart(List(), 0),
      (a, b) => ShoppingCart(a.items ++ b.items, a.total + b.total)
    )

  println(combineFold(List (
    ShoppingCart(List("iphone"), 49),
    ShoppingCart(List("mac", "a gun"), 99)
  )))
}
