package Parser

import cats.{Functor, Semigroup, SemigroupK}
import cats.data.{NonEmptyList, NonEmptyMap, Reader}

import scala.collection.immutable.List
import scala.compiletime.testing.ErrorKind.Parser

object ParserCombinatorsV2 extends App {
  object Library:
    import cats.syntax.all.*

    type ParseResult[T] = Either[String, (T, String)]
    type Parser[T] = Reader[String, ParseResult[T]]

    implicit object ParserFunctor extends Functor[Parser]:
      override def map[A, B](fa: Parser[A])(f: A => B): Parser[B] =
        fa.map(a => a.map((v, remaining) => (f(v), remaining)))

    implicit val parserSemigroup: Semigroup[Parser[(List[Char])]] = Semigroup.instance[Parser[List[Char]]] { (p1, p2) =>
      Reader { str =>
        for
          res1 <- p1.run(str)
          res2 <- p2.run(res1._2)
        yield (res1._1 ++ res2._1, res2._2)
      }
    }

    implicit val parserSemigroupK: SemigroupK[Parser] = new SemigroupK[Parser]:
      override def combineK[A](p1: Parser[A], p2: Parser[A]): Parser[A] =
        Reader { str =>
          p1.run(str) match
            case Right(v) => Right(v)
            case Left(_) => p2.run(str)
        }

    def pchar[T](charToMatch: Char): Parser[List[Char]] =
      Reader { str =>
        if str == "" then Left("No more input")
        else if str.head == charToMatch then Right((List(charToMatch), str.tail))
        else Left(s"Expecting '$charToMatch'. Got '${str.head}'")
      }

    def choice[T](list: List[Parser[T]])(using semigroup: SemigroupK[Parser]): Parser[T] =
      list.reduce((acc, cur) => semigroup.combineK(acc, cur))

    def anyOf(list: List[Char]): Parser[List[Char]] =
      val parsers = list.map(pchar).toList
      choice(parsers)

    extension[T](p1: Parser[T])
      def mapP[T1](f: T => T1)(using functor: Functor[Parser]) =
        functor.map(p1)(f)

  import Library.*
  import cats.syntax.all.*

  val pA = pchar('A')
  val pB = pchar('B')
  val pC = pchar('C')

  val digitChars = '0' to '9'

  val parseDigit = anyOf(('0' to '9').toList)

  val parseABC = pA |+| pB |+| pC

  val parseThreeDigits = parseDigit |+| parseDigit |+| parseDigit

  val mapped = parseThreeDigits.mapP(list => list.mkString(";"))

  println(mapped("123dsvc"))

  println(parseThreeDigits("123"))
  println(parseABC("ABC"))
}
