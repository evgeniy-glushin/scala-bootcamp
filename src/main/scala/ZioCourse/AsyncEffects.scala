package ZioCourse

import zio._

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Failure
import scala.util.Success

object AsyncEffects extends ZIOAppDefault{
  def future2zio[A](future: => Future[A])(using ec: ExecutionContext): Task[A] = ZIO.async { cb =>
    future.onComplete {
        case Success(value) => cb(ZIO.succeed(value))
        case Failure(exception) => cb(ZIO.fail(exception))
    }
  }

  val demo = {
    val executor = Executors.newFixedThreadPool(8)
    implicit val ec = ExecutionContext.fromExecutorService(executor)
    val mol: Task[Int] = ZIO.fromFuture(Future {
      println(s"[${Thread.currentThread().getName}] computing thread")
      Thread.sleep(1000)
      42
    })

    mol.debugThread.unit
  }

  override def run: ZIO[Any, Any, Any] = demo
}
