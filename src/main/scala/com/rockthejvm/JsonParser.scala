package com.rockthejvm

object JsonParser extends App {
  def quotes(v: Any): String = "\"" + v + "\""

  case class User(fullName: String, age: Int)

  trait JConverter[T] {
    def convert(value: T): JValue
  }

   implicit object IntConverter extends JConverter[Int] {
     override def convert(value: Int): JValue = JInt(value)
   }

  implicit object StringConverter extends JConverter[String] {
    override def convert(value: String): JValue = JString(value)
   }

  implicit object ArrayConverter extends JConverter[List[JValue]] {
     override def convert(value: List[JValue]): JValue = JArray(value)
   }

  implicit object UserConverter extends JConverter[User] {
    override def convert(value: User): JValue = JUser(value)
  }

  implicit class JValueOps[T](value: T) {
    def toJson()(implicit converter: JConverter[T]) = converter.convert(value).stringify
  }

  sealed trait JValue {
    def stringify: String
  }

  final case class JObject(map: Map[String, JValue]) extends JValue {
    override def stringify: String = {
      (for ((k, v) <- map) yield s"${quotes(k)}:${v.stringify}").mkString("{", ",", "}")
    }
  }

  final case class JInt(value: Int) extends JValue {
    override def stringify: String = value.toString
  }

  final case class JUser(value: User) extends JValue {
    override def stringify: String = JObject(Map(
      "fullName" -> JString(value.fullName),
      "age" -> JInt(value.age))
    ).stringify
  }

  final case class JString(value: String) extends JValue {
    override def stringify: String = quotes(value)
  }

  final case class JArray(value: List[JValue]) extends JValue {
    override def stringify: String = value.map(x => x.stringify).mkString("[", ",", "]")
  }

  object JObject {
    def apply(map: Map[String, JValue]): JObject = new JObject(map)
  }

  val obj = JObject(Map(
    "anyInt" -> JInt(32),
    "simpleString" -> JString("hello world"),
    "arr" -> JArray(List(JInt(1), JInt(2), JInt(3))),
    "user" -> JUser(User("yevhen", 34)),
    "nestedObj" -> JObject(Map(
      "nestedUser" -> JUser(User("ira", 18))
    ))
  ))

  println(obj.stringify)
  println(User("yev", 34).toJson())
}
