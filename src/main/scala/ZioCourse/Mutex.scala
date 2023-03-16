package ZioCourse

import zio._
import scala.collection.immutable.Queue

abstract class Mutex {
  def acquire: UIO[Unit]
  def release: UIO[Unit]
}

object Mutex {
  private type Signal = Promise[Nothing, Unit]
  final case class State(locked: Boolean, waiting: Queue[Signal])
  val unlocked = State(locked = false, Queue())

  def make: UIO[Mutex] = Ref.make(unlocked).map { state =>
    new Mutex:
      override def acquire: UIO[Unit] = for {
        signal <- Promise.make[Nothing, Unit]
        initState <- state.get
        isLocked = initState.locked
        _ <- state.update(x => x.copy(locked = true, x.waiting.enqueue(signal)))
        _ <- if isLocked then signal.await
             else ZIO.unit
      } yield ()

      override def release: UIO[Unit] = for {
        _ <- state.update { x =>
          val (signal, newQueue) = x.waiting.dequeue
          val newState = x.copy(locked = newQueue.nonEmpty, waiting = newQueue)
          signal.succeed(())
          newState
        }
      } yield ()
  }
}


object MutexPlayground extends ZIOAppDefault {

  def workInCriticalRegion(): UIO[Int] =
    ZIO.sleep(1.second) *> Random.nextIntBounded(100)

  def demoNonLockingTasks() =
    ZIO.collectAllParDiscard((1 to 10).toList.map { i =>
      for {
        _ <- ZIO.succeed(s"[task ${i}] working...").debugThread
        result <- workInCriticalRegion()
        _ <- ZIO.succeed(s"[task ${i}] got result: $result").debugThread
      } yield ()
    })

  def createTask(id: Int, mutex: Mutex): UIO[Int] = {
    val task = for {
      _ <- ZIO.succeed(s"[task $id] waiting for mutex...").debugThread
      _ <- mutex.acquire
      // critical region start
      _ <- ZIO.succeed(s"[task $id] mutex acquired, working...").debugThread
      result <- workInCriticalRegion().onInterrupt(mutex.release)
      _ <- ZIO.succeed(s"[task $id] got result: $result, releasing mutex").debugThread
      // critical region end
      _ <- mutex.release
    } yield result

    task
      .onInterrupt(ZIO.succeed(s"[task $id] was interrupted.").debugThread)
      .onError(cause => ZIO.succeed(s"[task $id] ended in error: $cause"))
  }

  def demoLockingTasks() = for {
    mutex <- Mutex.make
    _ <- ZIO.collectAllParDiscard((1 to 10).toList.map(i => createTask(i, mutex)))
  } yield ()

  def createInterruptingTask(id: Int, mutex: Mutex): UIO[Int] =
    if (id % 2 == 0)
      createTask(id, mutex)
    else for {
      fib <- createTask(id, mutex).fork
      _ <- ZIO.sleep(2500.millis) *> ZIO.succeed(s"interrupting task $id").debugThread *> fib.interrupt
      result <- fib.join
    } yield result


  /*
    _ _ X _ _ _ _ _ _ _
    2.5s => all the odd tasks will be interrupted
   */
  def demoInterruptingTasks() = for {
    mutex <- Mutex.make
    fib1 <- createInterruptingTask(1, mutex).fork
    fib2 <- createInterruptingTask(2, mutex).fork
    fib3 <- createInterruptingTask(3, mutex).fork
    fib4 <- createInterruptingTask(4, mutex).fork
    fib5 <- createInterruptingTask(5, mutex).fork
    fib6 <- createInterruptingTask(6, mutex).fork
    fib7 <- createInterruptingTask(7, mutex).fork
    fib8 <- createInterruptingTask(8, mutex).fork
    fib9 <- createInterruptingTask(9, mutex).fork
    fib10 <- createInterruptingTask(10, mutex).fork
    _ <- fib1.await
    _ <- fib2.await
    _ <- fib3.await
    _ <- fib4.await
    _ <- fib5.await
    _ <- fib6.await
    _ <- fib7.await
    _ <- fib8.await
    _ <- fib9.await
    _ <- fib10.await
  } yield ()

  def run = demoLockingTasks()
}