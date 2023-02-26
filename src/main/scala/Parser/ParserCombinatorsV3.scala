package Parser

import cats.data.Reader
import cats.{Functor, Semigroup, SemigroupK}
import cats.syntax.all.*

object Combinators:
  type ParseResult[T] = Either[String, (T, String)]
  type Parser[T] = Reader[String, ParseResult[T]]

  implicit object ParserMonad extends
      Semigroup[Parser[List[Char]]],
      SemigroupK[Parser],
      Functor[Parser]:

    override def combine(p1: Parser[List[Char]], p2: Parser[List[Char]]): Parser[List[Char]] =
      Parser { str =>
        for
          res1 <- p1.run(str)
          res2 <- p2.run(res1._2)
        yield (res1._1 ++ res2._1, res2._2)
      }

    override def map[A, B](fa: Parser[A])(f: A => B): Parser[B] =
      fa.map(a => a.map((v, remaining) => (f(v), remaining)))

    override def combineK[A](p1: Parser[A], p2: Parser[A]): Parser[A] =
      Parser { str =>
        p1.run(str) match
          case Right(v) => Right(v)
          case Left(_) => p2.run(str)
      }

  object Parser:
    def apply[T](f: String => ParseResult[T]): Parser[T] = Reader(f)

  def pchar(charToMatch: Char): Parser[List[Char]] =
    Parser { str =>
      if str == "" then Left("No more input")
      else if str.head == charToMatch then Right((List(charToMatch), str.tail))
      else Left(s"Expecting '$charToMatch'. Got '${str.head}'")
    }

  def choice[T](list: List[Parser[T]])(using semigroup: SemigroupK[Parser]): Parser[T] =
    list.reduce((acc, cur) => semigroup.combineK(acc, cur))

  def anyOf(list: List[Char]): Parser[List[Char]] =
    val parsers = list.map(pchar).toList
    choice(parsers)

  extension[T] (p1: Parser[T])
    def mapP[T1](f: T => T1)(using functor: Functor[Parser]) =
      functor.map(p1)(f)


object ParserCombinatorsV3 extends App {

  import Combinators._
  import cats.syntax.all._
  import cats.{Semigroup}

  val pA = pchar('A')
  val pB = pchar('B')
  val pC = pchar('C')

  val bOrElseC = pB <+> pC
  val aAndThenBorC = pA |+| bOrElseC

  println(aAndThenBorC("ABZ")) // Success (('A', 'B'), "Z")
  println(aAndThenBorC("ACZ")) // Success (('A', 'C'), "Z")
  println(aAndThenBorC("QBZ")) // Failure "Expecting 'A'. Got 'Q'"
  println(aAndThenBorC("AQZ")) // Failure "Expecting 'C'. Got 'Q'"

  val parseABC = pA |+| pB |+| pC

  val digitChars = '0' to '9'
  val parseDigit = anyOf(('0' to '9').toList)
  val parseThreeDigits = parseDigit |+| parseDigit |+| parseDigit
  val mapped = parseThreeDigits.mapP(list => list.mkString(";"))

  println(mapped("123dsvc"))
  println(parseThreeDigits("123"))
  println(parseABC("ABC"))
}
