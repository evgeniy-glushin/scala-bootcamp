import scala.annotation.tailrec

object MyLazyStream extends App {
  abstract class MyStream[+A] {
    def isEmpty: Boolean
    def head: A
    def tail: MyStream[A]

    def toList(n: Int): List[A] = {
      @tailrec def iterate(curr: Int, stream: MyStream[A], acc: List[A]): List[A] = {
        if (curr > 1) iterate(curr - 1, stream.tail, acc :+ stream.head)
        else acc :+ stream.head
      }

      iterate(n, this, List.empty)
    }
    def foreach(f: A => Unit): Unit = {
      f(head)
      if(!tail.isEmpty) tail.foreach(f)
    }
    // prepend
    def #::[B >: A](element: B): MyStream[B]
    // concatenate
    def ++[B >: A](another: => MyStream[B]): MyStream[B]
    def map[B](f: A => B): MyStream[B]
    def flatMap[B](f: A => MyStream[B]): MyStream[B]
    def filter(predicate: A => Boolean): MyStream[A]
    def take(n: Int): MyStream[A]
  }

  class EmptyStream extends MyStream[Nothing] {
    override def isEmpty: Boolean = true

    override def head: Nothing = ???

    override def tail: MyStream[Nothing] = ???

    override def toList(n: Int): List[Nothing] = ???

    override def #::[B >: Nothing](element: B): MyStream[B] = new LazyStream(element, this)

    override def ++[B >: Nothing](another: => MyStream[B]): MyStream[B] = another

    override def map[B](f: Nothing => B): MyStream[B] = this

    override def flatMap[B](f: Nothing => MyStream[B]): MyStream[B] = this

    override def filter(predicate: Nothing => Boolean): MyStream[Nothing] = this

    override def take(n: Int): MyStream[Nothing] = this
  }

  class LazyStream[A](hd: A, tl: => MyStream[A])extends MyStream[A] {
    override def isEmpty: Boolean = false

    override def head: A = hd

    override lazy val tail: MyStream[A] = tl

    override def #::[B >: A](element: B): MyStream[B] = new LazyStream(element, this)

    override def ++[B >: A](another: => MyStream[B]): MyStream[B] = new LazyStream(head, tail ++ another)

    override def map[B](f: A => B): MyStream[B] = new LazyStream[B](f(head), tail.map(f))

    override def flatMap[B](f: A => MyStream[B]): MyStream[B] = f(head) ++ tail.flatMap(f)

    override def filter(predicate: A => Boolean): MyStream[A] =
      if(predicate(head)) new LazyStream(head, tail.filter(predicate))
      else tail.filter(predicate)

    override def take(n: Int): MyStream[A] =
      if (n == 0) new EmptyStream
      else new LazyStream(head, tail.take(n - 1))
  }

  object MyStream {
      def from[A](start: A)(generator: A => A): MyStream[A] =
        new LazyStream(start, MyStream.from(generator(start))(generator))
  }

//  val s = MyStream.from(1)(x => x + 1)
//  s take(3) filter(_ > 2) map (x => s"m${x}") foreach(println)

  // 0, 1, 1, 2, 3, 5, 8
  def fibonacci(first: Int, second: Int): MyStream[Int] =
    new LazyStream[Int](first, fibonacci(second, first + second))


  fibonacci(1,1) take 5 foreach(println)
}
