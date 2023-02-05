package com.rockthejvm

import java.util.concurrent.Executors
import java.util.concurrent.atomic.AtomicReference

object Multithreading extends App {
  //val pool = Executors.newFixedThreadPool(8)

  def inceptionThreads(n: Int): Unit = {
    val t = new Thread(() => {
      if (n > 0) inceptionThreads(n - 1)

      println(s"hello from thread #${n}")
    })

    t.start()
    t.join()
  }

//  class MySyncObj(counter: Int) {
//    def counter: Int = counter
//  }
//
//  val syncObj = new MySyncObj(1)
//
//  syncObj.synchronized {
//    syncObj.counter += 1
//  }

  //inceptionThreads(50)

  // dead lock
  class Fighter(val name: String, var health: Int) {
    def counter(enemy: Fighter): Unit = {
      this.synchronized {
        println(s"${this.name} responding to ${enemy.name}" )
        enemy.health -= 5
      }
    }

    def hit(enemy: Fighter) = {
      this.synchronized{
        println(s"${this.name} hitting ${enemy.name}" )
        enemy.health -= 10

        enemy.counter(this)
      }
    }
  }

  val f1 = new Fighter("yevhen", 100)
  val f2 = new Fighter("enemy", 100)

  new Thread(() => { f1.hit(f2) }).start() // f1 lock and f2 lock
  new Thread(() => { f2.hit(f1) }).start()  // try f2 lock

  val atomic = new AtomicReference[Int](10)
  atomic.set(20)
}
