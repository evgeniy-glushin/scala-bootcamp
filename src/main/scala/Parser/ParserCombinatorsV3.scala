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

  def pchar(charToMatch: Char): Parser[Char] = Parser { str =>
      if str == "" then Left("No more input")
      else if str.head == charToMatch then Right(charToMatch, str.tail)
      else Left(s"Expecting '$charToMatch'. Got '${str.head}'")
    }

  def pstring(str: String): Parser[String] =
    val pList: Parser[List[Char]] = str.map(pchar).toList.sequence
    pList |> (a => a.mkString(""))

  def choice[T](list: List[Parser[T]])(using semigroup: SemigroupK[Parser]): Parser[T] =
    list.reduce((acc, cur) => semigroup.combineK(acc, cur))

  def anyOf(list: List[Char]): Parser[Char] =
    val parsers = list.map(pchar)
    choice(parsers)

//  let satisfy predicate label =

  /// matches zero or more occurences of the specified parser
  def many[T](p: Parser[T]): Parser[List[T]] =
    Parser(str => Right(parseZeroOrMore(p)(str)))

  /// matches one or more occurences of the specified parser
  def many1[T](p: Parser[T]): Parser[List[T]] =
    many(p) >>= (tail => p |> (head => head :: tail))

  def manyChars(p: Parser[Char]): Parser[String] =
    many(p) |> (chars => chars.mkString(""))

  def manyChars1(p: Parser[Char]): Parser[String] =
    many1(p) |> (chars => chars.mkString(""))

  def parseZeroOrMore[T](p: Parser[T]) (str: String): (List[T], String) =
    p.run(str) match
      case Left(_) => (List(), str)
      case Right((firstValue, firstRemaining)) =>
        val (subsequentValues, subsequentRemaining) = parseZeroOrMore(p)(firstRemaining)
        val values = firstValue :: subsequentValues
        (values,  subsequentRemaining)

  extension[A] (p1: Parser[A])
    def andThen[B](p2: Parser[B]): Parser[(A, B)]=
      Parser { str =>
        for
          res1 <- p1.run(str)
          res2 <- p2.run(res1._2)
        yield ((res1._1, res2._1), res2._2) // TODO: refactor
      }

    def |+| (p2: Parser[A]): Parser[(A, A)] = p1.andThen(p2)

    def >>= [B](f: A => Parser[B])(using monad: FlatMap[Parser]): Parser[B] =
      monad.flatMap(p1)(f)

    def |> [B](f: A => B)(using functor: Functor[Parser]): Parser[B] =
      functor.map(p1)(f)


object ParserCombinatorsV3 extends App {

  import Combinators._
  import cats.syntax.all._
  import cats.Semigroup

  val p = pstring("true")
  println(parseZeroOrMore(p)("truetru1e"))
  println(pstring("true").run("truecs"))

//  val pA = pchar('A')
//  val pB = pchar('B')
//  val pC = pchar('C')
//
//  println(pA.run("acs"))

//
//  val bOrElseC = pB <+> pC
//  val aAndThenBorC = pA |+| bOrElseC
//
//  println(aAndThenBorC("ABZ")) // Success (('A', 'B'), "Z")
//  println(aAndThenBorC("ACZ")) // Success (('A', 'C'), "Z")
//  println(aAndThenBorC("QBZ")) // Failure "Expecting 'A'. Got 'Q'"
//  println(aAndThenBorC("AQZ")) // Failure "Expecting 'C'. Got 'Q'"
//
//  val parseABC = pA |+| pB
//
//  val digitChars = '0' to '9'
//  val parseDigit = anyOf(('0' to '9').toList)
//  //val parseThreeDigits = parseDigit |+| parseDigit |+| parseDigit
//  //val mapped = parseThreeDigits.mapP(list => list.mkString(";"))
//
////  println(mapped("123dsvc"))
//  //println(parseThreeDigits("123"))
//  println(parseABC("ABC"))

//  type ParseResult[T] = Either[String, (T, String)]
//  type Parser[T] = Reader[String, ParseResult[T]]

}
