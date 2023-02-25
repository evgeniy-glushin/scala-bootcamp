package Parser

import cats.{FlatMap, Semigroup, SemigroupK}
import cats.data.Reader

object ParserCombinators extends App {
  object Library:
    type ParseResult[T] = Either[String, (T, String)]
    type Parser[T] = Reader[String, ParseResult[T]]

    extension[T](p1: Parser[T])
      def orElse(p2: Parser[T]): Parser[T] =
        Reader { str =>
          p1.run(str) match
            case Right(v) => Right(v)
            case Left(_) => p2.run(str)
        }

      def andThen(p2: Parser[T]): Parser[(T, T)] =
        Reader { str =>
          for
            res1 <- p1.run(str)
            res2 <- p2.run(res1._2)
          yield ((res1._1, res2._1), res2._2)
        }

    def choice[T](list: List[Parser[T]]): Parser[T] =
      list.reduce((acc, cur) => acc.orElse(cur))

    def anyOf(list: List[Char]): Parser[Char] =
      val parsers = list.map(c => pchar(c)).toList
      choice(parsers)

    def pchar[T](charToMatch: Char): Parser[Char] =
      def innerF(str: String): ParseResult[Char] =
        if str == "" then Left("No more input")
        else if str.head == charToMatch then Right((charToMatch, str.tail))
        else Left(s"Expecting '$charToMatch'. Got '${str.head}'")
      Reader(innerF)

  import Library.*

  val pA = pchar('A')
  val pB = pchar('B')


  val digitChars = '0' to '9'

  val parseLowercase = anyOf(('a' to 'z').toList)

  val parseDigit = anyOf(('0' to '9').toList)

  //val parseThreeDigits = (parseDigit andThen parseDigit) andThen parseDigit

  println(parseLowercase("aBC")) // Success ('a', "BC")
  println(parseLowercase("ABC")) // Failure "Expecting 'z'. Got 'A'"

  println(parseDigit("1ABC")) // Success ("1", "ABC")
  println(parseDigit("9ABC")) // Success ("9", "ABC")

  // map each char to a Parser using pchar
//  val digitParsers: List[Parser[Char]] = digitChars.map(pchar).toList
  //=> Parser<char> list

  // combine all parsers using choice
  //val parseDigit = choice(digitParsers)
  //=> Parser<char>
//
//
//  println(parseDigit("1ZZ")) // Success ('1', "ZZ")
//  println(parseDigit("2ZZ")) // Success ('2', "ZZ")
//  println(parseDigit("9ZZ")) // Success ('9', "ZZ")
//  println(parseDigit("AZZ")) // Failure "Expecting '9'. Got 'A'"
//
//  parseDigit("1ZZ") // Success ('1', "ZZ")
//  parseDigit("2ZZ") // Success ('2', "ZZ")
//  parseDigit("9ZZ") // Success ('9', "ZZ")
//  parseDigit("AZZ") // Failure "Expecting '9'. Got 'A'"
//
//  val parseA = pchar('A')
//  val parseB = pchar('B')
//  val parseC = pchar('C')
//  val bOrElseC = parseB orElse parseC
//  val aAndThenBorC = parseA andThen bOrElseC


//  println(aAndThenBorC("ABZ")) // Success (('A', 'B'), "Z")
//  println(aAndThenBorC("ACZ")) // Success (('A', 'C'), "Z")
//  println(aAndThenBorC("QBZ")) // Failure "Expecting 'A'. Got 'Q'"
//  println(aAndThenBorC("AQZ")) // Failure "Expecting 'C'. Got 'Q'"

//  val parseAAndThenB = parseA andThen parseB
//  println(parseAAndThenB("ABZ")) // Success ('A', "ZZ")
//  println(parseAAndThenB("BZZ")) // Success ('B', "ZZ")
//  println(parseAAndThenB("CZZ")) // Failure "Expecting 'B'. Got 'C'"

  //val parseA = pchar('A')

  //println(parseA("BBC"))

  //println (parseChar('A')("ABC"))


//  def parseChar(charToMatch: Char)(str: String): ParseResult[Char] =
//    if str == "" then "No more input"
//    else if str.head == charToMatch then (charToMatch, str.tail)
//    else s"Expecting '$charToMatch'. Got '${str.head}'"

//  import cats.syntax.all._
//
//  implicit val parserSemigroup: Semigroup[Parser[(Char)]] = Semigroup.instance[Parser[Char]] { (p1, p2) =>
//    Reader { str =>
//      for
//        res1 <- p1.run(str)
//        res2 <- p2.run(res1._2)
//      yield ((res1._1, res2._1), res2._2)
//    }
//  }
//
//  implicit val parserSemigroupK: SemigroupK[Parser] = new SemigroupK[Parser]:
//
//  override def combineK[A](p1: Parser[A], p2: Parser[A]): Parser[A] =
//    Reader { str =>
//      p1.run(str) match
//        case Right(v) => Right(v)
//        case Left(_) => p2.run(str)
//    }


//  def orElse[T](p1: Parser[T], p2: Parser[T]): Parser[T] =
//    Reader { str =>
//      p1.run(str) match
//        case Right(v) => Right(v)
//        case Left(_) => p2.run(str)
//    }
//
//  def andThen[T](p1: Parser[T], p2: Parser[T]): Parser[(T, T)] =
//    Reader { str =>
//      for
//        res1 <- p1.run(str)
//        res2 <- p2.run(res1._2)
//      yield ((res1._1, res2._1), res2._2)
//    }

}
