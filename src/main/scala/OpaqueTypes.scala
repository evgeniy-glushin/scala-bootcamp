import scala.concurrent.ExecutionContext.global
import scala.concurrent.Future

object OpaqueTypes extends App {
  trait Record:
    type Key

  def getIdentifier(r: Record): r.Key = ???

  val getIdentifierFunc = getIdentifier

  println("hello scala 3")

  val condition = if 2 > 3 then "bigger" else "smaller"

  val comprehension =
    for
      a <- List(1, 2, 3)
      b <- List("a", "b")
    yield s"$a$b"

//  val map = comprehension.map: x => "$x"

  println(comprehension)

  object MyDomain {
    opaque type Name = String | Int

    type Status = "new" | "deleted"

    def f(s: Status) = 1

    f("new")
  }
}
