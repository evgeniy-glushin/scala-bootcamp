import scala.util.{Failure, Success, Try}

object HttpServiceExercise extends App {
  case class Connection(host: String, port: String)
  val config = Map(
    "host" -> "localhost",
    "port" -> "4040"
  )

  type ErrorOr[T] = Either[String, T]

  trait HttpService[M[_]]:
    def getConnection(cfg: Map[String, String]): M[Connection]
    def issueRequest(connection: Connection, payload: String): M[String]

  class MyHttpService extends HttpService[ErrorOr]:
    override def getConnection(cfg: Map[String, String]): ErrorOr[Connection] =
      val con = for
        host <- cfg.get("host")
        port <- cfg.get("port")
      yield Connection(host, port)

      con.toRight("config is incorrect")


    override def issueRequest(connection: Connection, payload: String): ErrorOr[String] =
      if (payload.length > 5) Right(payload)
      else Left("payload is invalid")


  val service = new MyHttpService
  val response = for
    con <- service.getConnection(config)
    resp <- service.issueRequest(con, "123")
  yield resp

  println(response)
  //Failure(new NotImplementedError("error"))

Option
  println(Map(
    "host" -> "localhost",
    "port" -> "4040"
  ) == Map(
    "host" -> "localhost",
    "port" -> "4040"
  ))
}
