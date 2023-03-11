package ZioCourse

import zio.*

import java.io.File
import java.util.Scanner

import ZioCourse._

object Resources extends ZIOAppDefault{
  def openFileScanner(path: String): UIO[Scanner] =
    ZIO.succeed(new Scanner(new File(path)))

  def scanAndPrint(scanner: Scanner): UIO[Unit] =
    def printNextLine(line: String): UIO[Unit] =
      Console.printLine(line) *> ZIO.sleep(100.milliseconds)
      if (scanner.hasNextLine()) printNextLine(scanner.nextLine())
      else ZIO.unit

    printNextLine(scanner.nextLine())

  def acquireOpenFile(path: String): UIO[Unit] = ZIO.acquireReleaseWith(openFileScanner(path))(x => ZIO.succeed(x.close)) (scanAndPrint)

  var program = for {
    fib <- acquireOpenFile("src/main/scala/ZioCourse/Resources.scala").fork
    _ <- ZIO.sleep(2.seconds) *> fib.interrupt
  } yield ()

  override def run: ZIO[Any, Any, Any] = program
}
