package Parser

import cats.data.{Reader, ReaderT}
import cats.{Applicative, Functor, Id, Semigroup, SemigroupK}
import cats.syntax.all.*

object Combinators:
  type ParseResult[T] = Either[String, (T, String)]
  type Parser[T] = Reader[String, ParseResult[T]]

  // TODO: Functor
  def map[A, B](container: Parser[A])(f: A => B): Parser[B] =
    container.map {
      case Left(err) => Left(err)
      case Right((x, rem)) => Right((f(x), rem))
    }

  def flatMap[A, B](container: Parser[A])(f: A => Parser[B]) = ???

  implicit object ParserMonad extends
      SemigroupK[Parser],
      Functor[Parser]:

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

  def pchar(charToMatch: Char): Parser[Char] =
    Parser { str =>
      if str == "" then Left("No more input")
      else if str.head == charToMatch then Right(charToMatch, str.tail)
      else Left(s"Expecting '$charToMatch'. Got '${str.head}'")
    }

  import cats.instances.list._
  import cats.syntax.traverse._
  import cats.instances.either._
//
//  given parserResultApplicative: Applicative[ParseResult] with
//    override def pure[A](x: A): ParseResult[A] = Right((x, ""))
//    override def ap[A, B](ff: ParseResult[A => B])(fa: ParseResult[A]): ParseResult[B] = fa.ap()

  // TODO: move to ParserMonad
  given parserCharApplicative: Applicative[Parser] with
    override def pure[A](x: A): Parser[A] = Parser { str => Right((x, str)) }
    override def ap[A, B](ff: Parser[A => B])(fa: Parser[A]): Parser[B] = ???
      //val x = fa.flatMap(a => ff.map(f => f.map(f1 => f1._1(a))))
  //fa.ap(ff)
//      for
//        rf <- ff
//        ra <- fa
//      yield Parser { str =>
//
//      }

  def pstring(str: String): Parser[String] =
    val pList: Parser[List[Char]] = str.map(pchar).toList.sequence
    pList.map(_.map((a, b) => (a.toString, b)))

  def choice[T](list: List[Parser[T]])(using semigroup: SemigroupK[Parser]): Parser[T] =
    list.reduce((acc, cur) => semigroup.combineK(acc, cur))

  def anyOf(list: List[Char]): Parser[Char] =
    val parsers = list.map(pchar)
    choice(parsers)

  extension[A] (p1: Parser[A])
    def mapP[T1](f: A => T1)(using functor: Functor[Parser]): Parser[T1] =
      functor.map(p1)(f)

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

  val pA = pchar('A')
  val pB = pchar('B')
  val pC = pchar('C')

  val bOrElseC = pB <+> pC
  val aAndThenBorC = pA |+| bOrElseC

  println(aAndThenBorC("ABZ")) // Success (('A', 'B'), "Z")
  println(aAndThenBorC("ACZ")) // Success (('A', 'C'), "Z")
  println(aAndThenBorC("QBZ")) // Failure "Expecting 'A'. Got 'Q'"
  println(aAndThenBorC("AQZ")) // Failure "Expecting 'C'. Got 'Q'"

  val parseABC = pA |+| pB //|+| pC

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
