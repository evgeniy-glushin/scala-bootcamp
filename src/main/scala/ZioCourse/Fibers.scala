package ZioCourse

import zio.*

import java.io.{File, FileWriter}
import scala.io.Source

object MyFibersApp extends ZIOAppDefault {

  def genFile(path: String) =
    val random = scala.util.Random
    val chars = 'a' to 'z'
    val nWords = random.nextInt(2000)

    val content = (1 to nWords)
      .map(_ => (1 to random.nextInt(10)).map(_ => chars(random.nextInt(26))).mkString)
      .mkString(" ")

    val writer = new FileWriter(new File(path))
    writer.write(content)
    writer.flush()
    writer.close()

  def readContent(path: String): Task[String] =
    ZIO.attempt(
      Source.fromFile(path).getLines.mkString(" ")
    )

  def countWords(s: String): Int = s.split(" ").count(w => w != " ")

  def aggregate(n: Int): Task[Int] =
    (1 to n)
      .map(n => readContent(s"src/main/resources/testfile_$n.txt").map(countWords).fork)
      .map(effectFiber => effectFiber.flatMap(_.join))
      .reduce { (effectA, effectB) =>
        for
          a <- effectA
          b <- effectB
        yield a + b
      }

  def aggregatePar(n: Int): Task[Int] =
    val effects = (1 to n).map(n => readContent(s"src/main/resources/testfile_$n.txt").map(countWords))
    ZIO.reduceAllPar(ZIO.succeed(0), effects)(_ + _)

  def aggregatePar_v2(n: Int): Task[Int] =
    val effects: Seq[ZIO[Any, Throwable, Int]] = (1 to n).map(n => readContent(s"src/main/resources/testfile_$n.txt").map(countWords))
    ZIO.collectAllPar(effects).map(_.sum) // traverse

  override def run: ZIO[Any, Any, Any] = aggregatePar_v2(10).flatMap(n => Console.printLine(n))
    //Console.printLine(readContent("src/main/resources/testfile_7.txt"))
    //ZIO.succeed(
    //(1 to 10).foreach(n => genFile(s"src/main/resources/testfile_$n.txt"))
  //)
}

