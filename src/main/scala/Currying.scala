object Currying extends App {
  def formatList(format: String, nums: List[Double]): List[String] = {
    nums.map(n => format.format(n))
  }

  def curriedFormatList(format: String)(nums: List[Double]) =
    nums.map(n => format.format(n))

  val arr = List(13,4,5,6)
  //var laz = arr.with

  //val format42 = formatList("%4.2f", _)
  //println(format42(List(8.2342354)))

  val formatMyList = formatList(_, List(Math.PI, 9.001))
  val curriedFormatMyList = curriedFormatList(_: String)(List(Math.PI, 9.001))

  println(curriedFormatMyList("%4.2f"))
  println(curriedFormatMyList("%8.6f"))
  println(curriedFormatMyList ("%14.12f"))


  def byName(n: => Int) = n + 1
  def byFunction(f: () => Int) = f() + 1

  def method: Int = 1
  def parenMethod(): Int = 42




  //  val res = formatList("%4.2f", List(Math.PI, 9.001))
//  println(res)
}
