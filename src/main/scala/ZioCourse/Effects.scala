package ZioCourse

import scala.io.StdIn
import zio.*

import scala.util.{Failure, Success, Try}

object EffectsApp {
   def sequenceTakeLast[R, E, A, B](za: ZIO[R, E, A], zb: ZIO[R, E, B]): ZIO[R, E, B] =
     za *> zb

  def sequenceTakeFirst[R, E, A, B](za: ZIO[R, E, A], zb: ZIO[R, E, B]): ZIO[R, E, A] =
    za <* zb

  def runForever[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] =
    zio *> runForever(zio) //zio.flatMap(_ => runForever(zio))

  val endlessLoop = runForever {
    ZIO.succeed {
      println("printing...")
      Thread.sleep(1000)
    }
  }

  def sum(n: Int): UIO[Int] =
    if (n == 0) ZIO.succeed(0)
    else ZIO.suspendSucceed(sum(n - 1)).map(_ + n)

  def try2zio[A](t: Try[A]): Task[A] = t match
    case Failure(e) => ZIO.fail(e)
    case Success(value) => ZIO.succeed(value)

  case class MyIO[A](unsafeRun: () => A):
    def map[B](f: A => B): MyIO[B] =
      MyIO(() => f(unsafeRun()))

    def flatMap[B](f: A => MyIO[B]): MyIO[B] =
      MyIO(() => f( unsafeRun()).unsafeRun())
  end MyIO

}

object MyZioApp extends ZIOAppDefault {
  import EffectsApp._

  override def run: ZIO[Any, Any, Any] =
    sum(1200).debug
    //sum(20000).flatMap(s => Console.printLine(s"$s"))
}
