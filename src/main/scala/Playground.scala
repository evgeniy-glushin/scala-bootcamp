import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success}

object Playground extends App {
  println("I love Scala!")

  def foo(n: Int, v: Int) =
    for (i <- 0 until n;
         j <- 0 until n if i + j == v)
    yield (i, j)

  foo(10, 10).foreach {
    case (i, j) =>
      //println(s"($i, $j) ") // prints (1, 9) (2, 8) (3, 7) (4, 6) (5, 5) (6, 4) (7, 3) (8, 2) (9, 1)
  }


  def max(a: Int, b: Int) =
    if (a > b) a
    else b

  def myF() = {
    val x = 1+1
    x * 2
  }

  //  Write a Scala program to check two given integers, and return true if one of them is 30 or if their sum is 30.
  def check30(a: Int, b: Int) = {
    (a, b) match {
      case (30, _) => true
      case (_, 30) => true
      case (x, y) if x + y == 30 => true
      case _ => false
    }
  }

  println(check30(30,10))
  println(check30(31, 30))
  println(check30(31, -1))
  println(check30(1, 31))

  // Write a Scala program to check a given integer and return true if it is within 20 of 100 or 300
  def checkRange(n: Int) =
    (100 to 300).contains(n)

  println("checkRange:")
  println(checkRange(99))
  println(checkRange(170))
  println(checkRange(301))


  // Write a Scala program to remove the character in a given position of a given string. The given position will be in the range 0...string length -1 inclusive
  def removeChar(str: String, pos: Int) =
//    str.take(pos) + str.drop(pos + 1)
    str.zipWithIndex
      .filter(x => x._2 != pos)
      .map(x => x._1.toString)
      .mkString("")

  val removeCharRes = removeChar("hello 2world",6)

  println(removeCharRes)

  class BasePlanet(name: String)
  class Planet(name: String) extends BasePlanet(name)
  case class PlanetCase(name: String) //extends BaseCase // NOT ALLOWED
  case class BaseCase()

  val cp = PlanetCase("Earth")

  trait MyTrait[T <: String] {
    val age: Int
    val details: T;
  }

  val tr = new MyTrait[String] {
    override val age: Int = 100
    override  val details: _root_.java.lang.String = ""
  }

  def genericF[T <: BigDecimal](input: T) = {

  }

  case class MyClass() extends MyTrait[String]{
    override val age: Int = 10
    override val details: String = age.toString + "years old"
  }

  val l = List(2,3,4)
  val newL = 11 :: l
  val l2 = 1 +: newL :+ 100
  println(l2)


  println(MyClass().details)


//  trait Shape
//  case class Circle()
//  case class Rectangle()
//  val s: Shape = Circle()

  var future = Future[Int] {
    Thread.sleep(3000)
    1016
  }

  var r = for {
    x <- future
  } yield x*x

  val r2 = future.map(x => x * 2).recover {
    case (e: Throwable) => 1000
  }

  val r3 = future.map(x => x * 2).fallbackTo {
    Future(1000)
  }


  // 1)
  val f99 = Future(99)

  // 2)

  def inSequence[T1, T2](fa: Future[T1], fb: Future[T2])= {
    //val r = fa.flatMap(_ => fb)
    for (
      _ <- fa;
      b <- fb
    ) yield b
  }

  val f1 = Future {
    Thread.sleep(206)
    12
  }

  val f2 = Future {
    Thread.sleep(2000)
    22
  }

  // var inSequenceRes = Await.result(inSequence(f1, f2), 5.seconds)

  // println(inSequenceRes)

  def first[A](fa: Future[A], fb: Future[A]) = {
    val p = Promise[A]()
//
//    fa.onComplete{
//      case Success(value) => p.success(value)
//      case Failure(err) => p.failure(err)
//    }
//
//    fb.onComplete {
//      case Success(value) => p.success(value)
//      case Failure(err) => p.failure(err)
//    }

    fa.onComplete(p.tryComplete)
    fb.onComplete(p.tryComplete)

    p.future
  }

  //var firstRes = Await.result(first(f1, f2), 5.seconds)

  // println(firstRes)


  def last[A](fa: Future[A], fb: Future[A]) = {
    val p = Promise[A]()

    fa.onComplete{
      case Success(value) => if (fb.isCompleted) p.success(value)
    }

    val f = (inpt: A) => if(fa.isCompleted) p.success(inpt)

    fb.onComplete{
      case Success(value) => if (fa.isCompleted) p.success(value)
    }

    p.future
  }

//  var lastRes = Await.result(last(f1, f2), 5.seconds)
//
//   println(lastRes)
//}

def retryUntil[A](action: () => Future[A], condition: A => Boolean): Future[A] = {
  action()
    .filter(condition)
    .recoverWith {
      case _ => retryUntil(action, condition)
    };

  //  val p = Promise[A]()
//
//  action().onComplete {
//    case Success(res) => if (condition(res)) p.success(res) else retryUntil(action, condition)
//    case Failure(err) => err
//  }
//
//  p.future
}

}
