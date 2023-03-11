package ZioCourse

import zio.*

import java.util.concurrent.TimeUnit

object Refs extends ZIOAppDefault{
  def tickingClock: ZIO[Any, Nothing, Unit] =

    def ticking(ticks: Ref[Int]): UIO[Unit] = for {
      _ <- ZIO.sleep(1.second)
      _ <- Clock.currentTime(TimeUnit.MILLISECONDS).debugThread
      //value <- ticks.getAndUpdate(_ + 1)
      _ <- ticking(ticks)
    } yield ()

    def printTicks(ticks: Ref[Int]): UIO[Unit] = for {
      _ <- ZIO.sleep(5.seconds)
      v <- ticks.get
      _ <- ZIO.succeed(s"TICKS: $v").debugThread
      _ <- printTicks(ticks)
    } yield ()

    Ref.make(0).flatMap { t =>
      ticking(t).zipPar(printTicks(t))
    }.unit

  override def run: ZIO[Any, Any, Any] = tickingClock *> ZIO.sleep(7.seconds) *> ZIO.interrupt
}
