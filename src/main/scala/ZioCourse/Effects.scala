package ZioCourse

import scala.io.StdIn
import zio.*

import java.io.IOException
import scala.util.{Failure, Success, Try}

object EffectsApp {

  def timeout[R, E, A](zio: ZIO[R, E, A], time: Duration): ZIO[R, E, A] = for {
    fib <- zio.fork
    _ <- ZIO.sleep(time) *> fib.interruptFork
    result <- fib.join
  } yield result

  def zip[E, A, B](fi1: Fiber[E, A], fi2: Fiber[E, B]): ZIO[Any, Nothing, Fiber[E, (A, B)]] =
//    val (f1, f2) = (fi1.join, fi2.join)
//    f1.flatMap(f11 => f2.map(f22 => (f11, f22))).fork
    fi1.mapFiber(f1 => fi2.map(f2 => (f1, f2)))

  val aBadFailure: Task[Int] = ZIO.succeed[Int](throw new RuntimeException("this is bad"))
  val aBetterFailure = aBadFailure.sandbox
  var aBetterFailureV2 = aBadFailure.unrefine { case e => e }
  //aBadFailure.for k

  def ioException[R, A](zio: ZIO[R, Throwable, A]): ZIO[R, IOException, A] =
    zio.refineOrDie[IOException] { case e: IOException => e }

  def left[R, E, A, B](zio: ZIO[R, E, Either[A, B]]): ZIO[R, Either[E, A], B] = ???

  def sequenceTakeLast[R, E, A, B](za: ZIO[R, E, A], zb: ZIO[R, E, B]): ZIO[R, E, B] =
    za *> zb

  def sequenceTakeFirst[R, E, A, B](za: ZIO[R, E, A], zb: ZIO[R, E, B]): ZIO[R, E, A] =
    za <* zb

  def runForever[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] =
    zio *> runForever(zio)

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
}

object MyZioApp extends ZIOAppDefault {
  import EffectsApp._

  override def run: ZIO[Any, Any, Any] =
    sum(1200).debug
    aBadFailure.debug
    //sum(20000).flatMap(s => Console.printLine(s"$s"))
}
