package com.rockthejvm

import scala.language.reflectiveCalls

object DuckTyping extends App {
  type SoundMaker = {
    def makeSound(): Unit
  }

  class Dog {
    def makeSound(): Unit = println("bark!")
  }

  class Car {
    def makeSound(): Unit = println("vroom!")
  }

  private val soundMaker: SoundMaker = new Car
  soundMaker.makeSound()
}
