import scala.annotation.tailrec

object LazyEvaluation extends App {
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
      tail.foreach(f)
    }

    // prepend
    def #::[B >: A](element: B): MyStream[B] = new PrependIterator(element, this)
    // concatenate
    def ++[B >: A](stream: MyStream[B]): MyStream[B] = new ConcatenateIterator[B](this, stream)
    def map[B](f: A => B): MyStream[B] = new MapIterator(f, this)
    def filter(predicate: A => Boolean): MyStream[A] = new FilterIterator[A](predicate, this)

    def take(n: Int): MyStream[A] = {
      if (isEmpty) this
      else new TakeIterator(n, this)
    }

    // TODO: def flatMap[B](f: A => MyStream[B]): MyStream[B]
  }

  class EmptyStream[+A] extends MyStream[A] {
    override def isEmpty: Boolean = true

    override def head: A = ???

    override def tail: MyStream[A] = ???

    override def take(n: Int): MyStream[A] = ???

    override def toList(n: Int): List[A] = ???

    override def foreach(f: A => Unit): Unit = ()

    override def map[B](f: A => B): MyStream[B] = new EmptyStream[B]

    override def filter(predicate: A => Boolean): MyStream[A] = this
  }

  class ConcatenateIterator[A](a: MyStream[A], b: MyStream[A]) extends MyStream[A] {
    override def isEmpty: Boolean = a.isEmpty && b.isEmpty
    override def head: A = innerStream.head

    private val innerStream = {
      //if(!a.isEmpty && !b.isEmpty)
      if(isEmpty) new EmptyStream
      else if (a.isEmpty) b
      else a
    }

    override def tail: MyStream[A] = innerStream.tail
  }

  class PrependIterator[A] (newElement: A, stream: MyStream[A]) extends MyStream[A] {
    override def isEmpty: Boolean = false

    override def head: A = newElement

    override def tail: MyStream[A] = stream
  }

  class GenerateIterator[A](start: A, generator: A => A) extends MyStream[A] {
    override def isEmpty: Boolean = false

    override def head: A = start

    private lazy val innerTail = new GenerateIterator(generator(head), generator)

    override def tail: MyStream[A] = innerTail

    override def foreach(f: A => Unit): Unit = {
      f(head)
      tail.foreach(f)
    }
  }

  class MapIterator[A, B](mapper: A => B, stream: MyStream[A]) extends MyStream[B] {
    override def isEmpty: Boolean = stream.isEmpty

    private lazy val innerHead = mapper(stream.head)
    private lazy val innerTail = stream.tail.map(mapper)

    override def head: B = innerHead
    override def tail: MyStream[B] = innerTail
    override def toList(n: Int): List[B] = ???

    override def foreach(f: B => Unit): Unit = {
      f(head)
      if(!tail.isEmpty)
        tail.foreach(f)
    }
  }

  class FilterIterator[A](predicate: A => Boolean, stream: MyStream[A]) extends MyStream[A] {
    override def isEmpty: Boolean = filteredStream.isEmpty

    private lazy val innerTail = {
      if (filteredStream.tail.isEmpty) new EmptyStream
      else filteredStream.tail.filter(predicate)
    }

    private lazy val filteredStream: MyStream[A] = {
      if (!stream.isEmpty && predicate(stream.head)) stream
      else if (!stream.tail.isEmpty) stream.tail.filter(predicate)
      else new EmptyStream
    }

    override def head: A =
      filteredStream.head

    override def tail: MyStream[A] =
      if (innerTail.isEmpty) new EmptyStream[A]
      else innerTail

    override def toList(n: Int): List[A] = {
      if (predicate(head)) super.toList(n)
      tail.toList(n)
    }

    override def map[B](f: A => B): MyStream[B] = {
      if (predicate(head)) new MapIterator(f, this)
      else if(!tail.isEmpty) new MapIterator(f, tail)
      else new EmptyStream
    }

    override def foreach(f: A => Unit): Unit = {
      if (predicate(head)) f(head)
      if (!tail.isEmpty) tail.foreach(f)
    }
  }

  class TakeIterator[A](count: Int, stream: MyStream[A]) extends MyStream[A] {
    override def isEmpty: Boolean = count == 0 || stream.isEmpty

    override def head: A = stream.head

    private lazy val innerTail = stream.tail

    override def tail: MyStream[A] = {
      val n = count - 1
      if (n > 0 && !innerTail.isEmpty) new TakeIterator(n, innerTail)
      else new EmptyStream
    }

    override def toList(n: Int): List[A] = ???

    override def foreach(f: A => Unit): Unit = {
      f(head)
      tail.foreach(f)
    }
  }

  object MyStream {
    def from[A](start: A)(generator: A => A): MyStream[A] = {
      new GenerateIterator[A](start, generator)
    }

    def fromv2[A](start: A)(generator: Option[A] => A => A): MyStream[A] = {
      //new GenerateIterator[A](start, generator)
      new EmptyStream
    }
  }

//  val s = MyStream.fromv2(2)(prev => curr => if(prev.nonEmpty) curr else prev + curr)
//
//  s filter(_ <10) take 9 take 20  foreach println
//  ((s take 3) ++ (MyStream.from(10)(x => x + 1) take 3)) foreach println
//  100 #:: s take 3 filter(_ > 10) map(x => s"m$x") foreach println
//  s take 40 filter(_ > 3) map(x => s"m${x}") take 4 foreach println
//  s map(x => s"+$x") take 10 take 5 map(x => s"-$x") filter (_ == "-+4") map(x => s"mapped$x") foreach println

  MyStream.from(1)(x => x + 1) take 5 foreach println




  //println(s.toList(5))
  //LazyStream.from[Int](1)(x => x + 1).foreach(println)
}
