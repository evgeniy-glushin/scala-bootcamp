object PartialFunctions extends App {
  val pf: PartialFunction[String, String] = {
    case "yevhen" => "me"
  }

  val orelse: PartialFunction[String, Any] = pf.orElse {
    case "yevhen2" => 1
  }


  println(orelse("yevhen2"))

  val s = Set(12,5)

  val bot: PartialFunction[String, String] = {
    case "hi" => "hi, how can I help you"
    case "q" => "Bye"
    case _ => "unknown"
  }

  scala.io.Source.stdin.getLines().map(bot).foreach(println)
}
