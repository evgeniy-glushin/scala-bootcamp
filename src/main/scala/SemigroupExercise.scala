import cats.{Monoid, Semigroup}
import cats.implicits.catsSyntaxSemigroup

object SemigroupExercise extends App {
  case class Expense2(id: Long, amount: Double)

  def combine(a: Expense2, b: Expense2): Expense2 =
    new Expense2(Math.max(a.id, b.id), a.amount + b.amount)

  implicit val expenseSemigroup: Semigroup[Expense2] = Semigroup.instance[Expense2](combine)

  def reduceThings[T](things: List[T])(using semigroup: Semigroup[T]): T = things.reduce(semigroup.combine)

  println(reduceThings(List(
    new Expense2(20, 999),
    new Expense2(30, 1)
  )))

  import cats.syntax.semigroup._
  println(new Expense2(20, 1999) |+| new Expense2(310, 1))

  def reduceThingsV2[T: Semigroup](things: List[T]): T = things.reduce(_ |+| _)

  println(reduceThingsV2(List(
    new Expense2(20, 999),
    new Expense2(30, 1)
  )))

//  implicit val expenseMonoid: Monoid[Expense2] = Monoid.instance[Expense2](Expense2(0, 0), combine)
//  def combineFold[T](list: List[T])(using monoid: Monoid[T]) = list.fold(monoid.empty)(monoid.combine)
//
//  println(combineFold(List(
//    new Expense2(20, 999),
//    new Expense2(30, 1)
//  )))

}
