object DarkSugars extends App {
  def singleArg(arg: Int) = s"This is my arg: $arg"

  val r = singleArg {
    val (a,b) = (2,5)
    a + b
  }

  println(r)


  class Composite[A,B]
  val c: Int Composite String = new Composite[Int, String]()


}
