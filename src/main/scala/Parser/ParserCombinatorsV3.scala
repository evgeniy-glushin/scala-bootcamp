package Parser

import cats.data.{Reader, ReaderT}
import cats.{Applicative, FlatMap, Functor, Id, Semigroup, SemigroupK}
import cats.syntax.all.*

object Combinators:
  type ParseResult[T] = Either[String, (T, String)]
  type Parser[T] = Reader[String, ParseResult[T]]

  implicit object ParserMonad extends
      SemigroupK[Parser],
      Functor[Parser],
      Applicative[Parser],
      FlatMap[Parser]:

    override def pure[A](x: A): Parser[A] =
      Parser { str => Right((x, str)) }

    override def ap[A, B](ff: Parser[A => B])(fa: Parser[A]): Parser[B] =
      flatMap(ff)(f => map(fa)(f))

    override def map[A, B](fa: Parser[A])(f: A => B): Parser[B] =
      fa.map(a => a.map((v, remaining) => (f(v), remaining)))

    def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] = Parser { str =>
        p(str) match {
          case Left(error) => Left(error)
          case Right((a, remaining)) => f(a)(remaining)
        }
      }

    override def tailRecM[A, B](a: A)(f: A => Parser[Either[A, B]]): Parser[B] = ???

    override def combineK[A](p1: Parser[A], p2: Parser[A]): Parser[A] =
      Parser { str =>
        p1.run(str) match
          case Right(v) => Right(v)
          case Left(_) => p2.run(str)
      }

  end ParserMonad

  object Parser:
    def apply[T](f: String => ParseResult[T]): Parser[T] = Reader(f)

  import ParserMonad.*

  def pchar(charToMatch: Char): Parser[Char] =
    Parser { str =>
      if str == "" then Left("No more input")
      else if str.head == charToMatch then Right(charToMatch, str.tail)
      else Left(s"Expecting '$charToMatch'. Got '${str.head}'")
    }

  def pstring(str: String): Parser[String] =
    val pList: Parser[List[Char]] = str.map(pchar).toList.sequence
    pList.map(_.map((a, b) => (a.mkString(""), b)))

  def choice[T](list: List[Parser[T]])(using semigroup: SemigroupK[Parser]): Parser[T] =
    list.reduce((acc, cur) => semigroup.combineK(acc, cur))

  def anyOf(list: List[Char]): Parser[Char] =
    val parsers = list.map(pchar)
    choice(parsers)

  extension[A] (p1: Parser[A])
    def andThen[B](p2: Parser[B]): Parser[(A, B)]=
      Parser { str =>
        for
          res1 <- p1.run(str)
          res2 <- p2.run(res1._2)
        yield ((res1._1, res2._1), res2._2)
      }

    def |+| (p2: Parser[A]): Parser[(A, A)] = p1.andThen(p2)


object ParserCombinatorsV3 extends App {

  import Combinators._
  import cats.syntax.all._
  import cats.Semigroup

  println(pstring("true1").run("true"))

  val pA = pchar('A')
  val pB = pchar('B')
  val pC = pchar('C')

  val bOrElseC = pB <+> pC
  val aAndThenBorC = pA |+| bOrElseC

  println(aAndThenBorC("ABZ")) // Success (('A', 'B'), "Z")
  println(aAndThenBorC("ACZ")) // Success (('A', 'C'), "Z")
  println(aAndThenBorC("QBZ")) // Failure "Expecting 'A'. Got 'Q'"
  println(aAndThenBorC("AQZ")) // Failure "Expecting 'C'. Got 'Q'"

  val parseABC = pA |+| pB

  val digitChars = '0' to '9'
  val parseDigit = anyOf(('0' to '9').toList)
  //val parseThreeDigits = parseDigit |+| parseDigit |+| parseDigit
  //val mapped = parseThreeDigits.mapP(list => list.mkString(";"))

//  println(mapped("123dsvc"))
  //println(parseThreeDigits("123"))
  println(parseABC("ABC"))

//  type ParseResult[T] = Either[String, (T, String)]
//  type Parser[T] = Reader[String, ParseResult[T]]

  type MyResult[T] = Either[String, T]
  type MyParserT[T] = ReaderT[Id, String, MyResult[T]]
  type MyParser[T] = Reader[String, MyResult[T]]
  val readerT = new MyParserT(s => Right(s))
  //readerT.map(x => )

  val reader = new MyParser(s => Right(s))
  //reader.map(x => )

  given parserCharApplicative: Applicative[MyParserT] with
    override def pure[A](x: A): MyParserT[A] = ???
    override def ap[A, B](ff: MyParserT[A => B])(fa: MyParserT[A]): MyParserT[B] = ???
}
