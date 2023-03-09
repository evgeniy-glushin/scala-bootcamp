package ZioCourse

import zio.*

import java.io.File
import java.util.Scanner

object Resources extends ZIOAppDefault{
  def openFileScanner(path: String): UIO[Scanner] =
    ZIO.succeed(new Scanner(new File(path)))

  def acquireOpenFile(path: String): UIO[Unit] = ZIO.acquireReleaseWith(openFileScanner(path))(x => ZIO.succeed(x.close)) { scanner =>
    def printNextLine(line: String): UIO[Unit] =
      Console.printLine(line)
      ZIO.sleep(100.milliseconds)
      if (scanner.hasNextLine) printNextLine(scanner.nextLine())
      else ZIO.unit
  }

  var program = for {
    fib <- acquireOpenFile("src/main/scala/ZioCourse/Resources.scala  ").fork
    _ <- ZIO.sleep(2.seconds) *> fib.interrupt
  } yield ()

  override def run: ZIO[Any, Any, Any] = program
}
