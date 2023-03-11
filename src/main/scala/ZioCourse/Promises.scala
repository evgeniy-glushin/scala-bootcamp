package ZioCourse

import zio._

object Promises extends ZIOAppDefault {
  /**
   * Exercises
   * 1. Write a simulated "egg boiler" with two ZIOs
   *  - one increments a counter every 1s
   *  - one waits for the counter to become 10, after which it will "ring a bell"
   */

  def eggBoiler(): UIO[Unit] =
    def boil(egg: Ref[Int], signal: Promise[Nothing, Int]): UIO[Unit] = for {
      newValue <- egg.updateAndGet(_ + 1)
      _ <- ZIO.succeed(s"new value: $newValue").debugThread
      _ <- if (newValue >= 10) signal.succeed(newValue) else ZIO.unit
      _ <- ZIO.sleep(1.second)
      _ <- boil(egg, signal)
    } yield ()

    def ringTheBell(signal: Promise[Nothing, Int]): UIO[Unit] = for {
      secondsPassed <- signal.await
      _ <- ZIO.succeed(s"RINGING AFTER $secondsPassed seconds").debugThread
    } yield ()

    for
      ref <- Ref.make(0)
      signal <- Promise.make[Nothing, Int]
      _ <- boil(ref, signal).zipPar(ringTheBell(signal))
    yield ()


  override def run: ZIO[Any, Any, Any] = eggBoiler()
}
