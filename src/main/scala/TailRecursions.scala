import scala.annotation.tailrec

object TailRecursions extends App {

  def repeat(s: String, n: Int): String = {

    @tailrec def repeatTailRec(curN: Int, acc: String): String ={
      if (curN == 0) acc
      else repeatTailRec(curN - 1, acc + s)
    }

    repeatTailRec(n, "")
    //    if (n == 1) s
//    else concut(s, n - 1) + s
  }

  println(repeat("x", 3))

  // TODO: fibonacci
}
