package com.rockthejvm

import scala.util.{Failure, Success, Try}

// Type classes:
// - trait with operations
// - implicit trait implementations
// - enrich type with type classes so we can use: yevhen ==! ira (pimp library)

object Implicits extends App {
  implicit def personOrdering: Ordering[Person] = Ordering.fromLessThan(_.name < _.name)

  case class Person(name: String, age: Int)

  val persons = List(
    Person("Steve", 30),
    Person("Amy", 22),
    Person("John", 66)
  )

  println(persons.sorted)

  case class Purchase(nUnits: Int, unitPrice:Double)

  object TotalPriceOrdering {
    implicit def totalPrice: Ordering[Purchase] =
      Ordering.fromLessThan((a, b) => a.nUnits * a.unitPrice < b.nUnits * b.unitPrice)
  }

  object UnitCountOrdering {
    implicit def unitCount: Ordering[Purchase] = Ordering.fromLessThan(_.nUnits < _.nUnits)
  }

  object UnitPriceOrdering {
    implicit def unitPrice: Ordering[Purchase] = Ordering.fromLessThan(_.unitPrice < _.unitPrice)
  }

  val purchases = List(
    Purchase(3, 99.99),
    Purchase(2, 49),
    Purchase(10, 9.99)
  )

  import UnitPriceOrdering._

  println(purchases.sorted)

  case class User(name: String, email: String)

  trait Equal[T] {
    def apply(a: T, b: T): Boolean
  }

   object UserNameComparer extends Equal[User] {
    override def apply(a: User, b: User): Boolean = a.name == b.name
  }

  implicit object UserNameEmailComparer extends Equal[User] {
    override def apply(a: User, b: User): Boolean = UserNameComparer.apply(a, b) && a.email == b.email
  }

//  object Equal  {
//    def apply[T](a: T, b: T)(implicit comparer: Equal[T])  : Boolean = comparer.apply(a,b)
//  }

  implicit class CompareEnrichment[T](self: T) {
    def ===(anotherValue: T)(implicit compare: Equal[T]): Boolean =
      compare(self, anotherValue)

    def ==!(anotherValue: T)(implicit compare: Equal[T]): Boolean =
      !compare(self, anotherValue)
  }

  val yevhen = User("yevhen", "yevhen@gmail.com")
  val yevhenDuplicate = User("yevhen", "yevhen@gmail.com")
  val ira = User("ira", "ira@gmail.com")

  println(yevhen === yevhenDuplicate)
  println(yevhen ==! ira)
  //println(Compare(yevhen, ira))

  implicit class RichString(val self: String) extends AnyVal {
    def asInt: Try[Int] = Try(Integer.valueOf(self))  //Try(self.toInt)
  }

  implicit class RichInt(val self: Int) extends AnyVal {
    def times(f: () => Unit): Unit = (1 to self).foreach(_ => f())

    def *(list: List[Int]) = (1 to  self).flatMap(_ => list)
  }

  println(3 * List(100, 101, 102))
  3.times(() => println("printing N times"))

  implicit def stringToInt(s: String): Int = Integer.valueOf(s)

  println("100" / 4)

  println("33".asInt match {
    case Success(value) => s"value parsed: $value"
    case Failure(exception) => s"Error thrown: $exception"
  })

  class Vehicle
  case class Bike() extends Vehicle
  case class Car() extends Vehicle

  class IParking[T] (things: List[T]){
    def park(vehicle: T): Unit = { }
    def impound(vehicles: List[T]): Unit = { }
    def checkVehicles(conditions: String): List[T] = { List() }
  }

  class CoParking[+T](things: List[T]) {
    def park[A >: T](vehicle: A): Unit = {}

    def impound[A >: T](vehicles: List[A]): Unit = {}

    def checkVehicles(conditions: String): List[T] = List[T]()
  }

  val coParking: CoParking[Vehicle] = new CoParking[Car](List(Car()))
  coParking.park(Bike())
  coParking.impound(List(Bike(), Car()))
  val coCars: List[Vehicle] = coParking.checkVehicles("my")

  class ConParking[-T](things: List[T]) {
    def park[A <: T](vehicle: A): Unit = {}
    def impound[A <: T](vehicles: List[A]): Unit = {}
    def checkVehicles[A <: T](conditions: String): List[A] = List[A]()
  }

  val conParking: ConParking[Car] = new ConParking[Vehicle](List(Car()))
  conParking.park(Car())
  conParking.impound(List(Car()))
  val conCars: List[Vehicle] = conParking.checkVehicles("my")
}
