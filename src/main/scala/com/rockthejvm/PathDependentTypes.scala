package com.rockthejvm

object PathDependentTypes extends App {

  trait ItemLike {
    type Key
  }

  trait Item[T] extends ItemLike{
    type Key = T
  }
  trait IntItem extends Item[Int]
  trait StringItem extends Item[String]

  def select[ItemType <: ItemLike](key: ItemType#Key): ItemType = ???

  select[IntItem](23)
  select[StringItem]("32")
}
