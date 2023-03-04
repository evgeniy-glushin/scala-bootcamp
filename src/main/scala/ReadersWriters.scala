import cats.data.AndThen.andThen
import cats.data.{Reader, Writer}




object ReadersWriters extends App {
  import cats.data.Reader

  val add1 = (n: Int) => n + 1
  val toStr = (n: Int) => n.toString

  val add1Reader: Reader[Int, Int] = Reader(add1)
  val toStrReader: Reader[Int, String] = Reader(toStr)

  //add1Reader.traverse()
  val program = add1Reader.map(_ * 2).andThen(toStrReader)

  println(program.run(21))

  //val res = add1(1).and toString

  case class Configuration(dbUsername: String, dbPassword: String,  host: String, port: Int)
  case class DbConnection(username: String, password: String):
    def getOrderStatus(orderId: Long): String = "dispatched"

  val config = Configuration("yevhen", "13123", "localhost", 1234)

  val dbReader: Reader[Configuration, DbConnection] = Reader(conf => DbConnection(conf.dbUsername, conf.dbPassword))

  case class EmailService (emailToReply: String):
    def send(address: String, content: String) = s"From $emailToReply; to: $address >>> $content"

//  def emailUser(username: String, email: String) =
//    val emailServiceReader: Reader[Configuration, EmailService] = Reader(conf => EmailService(conf.dbUsername))
//
//
//    dbReader
//      .map(con => con.getOrderStatus(100))
//      .flatMap(status => EmailService("yevhen@gmail.com").send("ira@gmail.com", s"Your last order status: $status"))
//      .run(config)


  // Writers
  def countAndSay(n: Int): Writer[Vector[String], Int] =
    if (n <= 0) Writer(Vector("starting"), n)
    else countAndSay(n - 1).flatMap(_ => Writer(Vector(s"${n}"), n))

  countAndSay(10).written foreach println

  def sum(n: Int): Writer[List[String], Int] =
    if n <= 0 then Writer(List(), 0)
    else for
      _ <- Writer(List(s"Now at ${n}"), n)
      lowerSum <- sum(n - 1)
      _ <- Writer(List(s"computed sum(${n - 1}) = $lowerSum"), n)
    yield lowerSum + n
//    else sum(n - 1).flatMap(lowerSum =>
//      Writer(List(s"Now at ${n}"), n)
//        .bimap(_ :+ s"computed sum(${n - 1}) = $lowerSum", _ + lowerSum)
//    )

  val s = sum(5)

  s.written foreach println

  println(s.written)
  println(s.value)
}
