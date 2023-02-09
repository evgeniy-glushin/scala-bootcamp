package com.rockthejvm

object JsonParser extends App {
  trait JsonSerializer[A] {
    def serialize(obj: A): String
  }

  case class User(fullName: String, age: Int)

  implicit object UserSerializer extends JsonSerializer[User] {
    override def serialize(user: User): String = {
      ???
      //JObject("fullName", JsonSerializer(user.fullName))
    }
  }

  implicit object IntSerializer extends JsonSerializer[Int] {
    override def serialize(obj: Int): String = ???
  }

  implicit object StringSerializer extends JsonSerializer[String] {
    override def serialize(obj: String): String = ???
  }

  implicit object ListSerializer extends JsonSerializer[List[Int]]{
    override def serialize(obj: List[Int]): String = ???
  }

  object JsonSerializer {
    def apply[T](obj: T)(implicit serializer: JsonSerializer[T]): String = serializer.serialize(obj)
  }

  sealed trait JValue {
    def stringify: String
  }

  final case class JObject(map: Map[String, JValue]) extends JValue {
    override def stringify: String = {
      (for ((k, v) <- map) yield s"$k:${v.stringify}").mkString("{", ",", "}")
    }
  }

  final case class IntJson(value: Int) extends JValue {
    override def stringify: String = ???
  }

  final case class UserJson(value: User) extends JValue {
    override def stringify: String = ???
  }

  final case class StringJson(value: String) extends JValue {
    override def stringify: String = ???
  }

  object JObject {
    def apply(map: Map[String, JValue]): JObject = new JObject(map)
  }

  val obj = JObject(Map(
    "anyInt" -> IntJson(32),
    "simpleString" -> StringJson("hello world"),
    "user" -> UserJson(User("yevhen", 34)),
    "nestedObj" -> JObject(Map(
      "nestedUser" -> UserJson(User("ira", 18))
    ))
  ))

  //println(obj.stringify)

  val m = Map(
    "asxc" -> 1,
    "a" -> 2
  )

  val mm = for ((k, v) <- m) yield s"$k:$v"

  println(mm.mkString("{", ",", "}"))
  println(m.mkString("{", ",", "}"))

//  var userJson = JsonSerializer(User("yevhen", 34)) // {"fullName": "yevhen", "age": 34}
//  var intJson = JsonSerializer(34) // "34"
//  var stringJson = JsonSerializer("34") // "34"
//  var listJson = JsonSerializer(List(1,2,3)) // "[1,2,3]"
}
