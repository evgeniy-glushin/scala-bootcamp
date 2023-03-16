package ZioCourse

import zio._

object Semaphores extends ZIOAppDefault{
  def doWorkWhileLoggedIn(): UIO[Int] =
    ZIO.sleep(1.second) *> Random.nextIntBounded(100)

  val mySemaphore = Semaphore.make(1) // a mutex
  val tasks = ZIO.collectAllPar((1 to 10).map { id =>
    for {
      sem <- mySemaphore
      _ <- ZIO.succeed(s"[task $id] waiting to log in").debugThread
      res <- sem.withPermit {
        for {
          // critical section start
          _ <- ZIO.succeed(s"[task $id] logged in, working...").debugThread
          res <- doWorkWhileLoggedIn()
          _ <- ZIO.succeed(s"[task $id] done: $res").debugThread
        } yield res
      }
    } yield res
  })

  val fixedTasks = for {
    sem <- mySemaphore
    task <- ZIO.collectAllPar((1 to 10).map { id =>
      for {
        _ <- ZIO.succeed(s"[task $id] waiting to log in").debugThread
        res <- sem.withPermit {
          for {
            // critical section start
            _ <- ZIO.succeed(s"[task $id] logged in, working...").debugThread
            res <- doWorkWhileLoggedIn()
            _ <- ZIO.succeed(s"[task $id] done: $res").debugThread
          } yield res
        }
      } yield res
    })
  } yield task


  override def run: ZIO[Any, Any, Any] = fixedTasks.debugThread
}
