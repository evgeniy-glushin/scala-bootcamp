import scala.annotation.tailrec

object FunctionalCollections extends App {
  trait MySet[A] extends (A => Boolean) {
    def apply(elem: A): Boolean = contains(elem)
    def contains(elem: A): Boolean
    def +(elem: A): MySet[A]
    def ++(another: MySet[A]): MySet[A]
    def map[B](f: A => B): MySet[B]
    def flatMap[B](f: A => MySet[B]): MySet[B]
    def filter(predicate: A => Boolean): MySet[A]
    def foreach(f: A => Unit): Unit
    def - (elem: A): MySet[A] // remove
    def &(another: MySet[A]): MySet[A] // intersect
    def --(another: MySet[A]): MySet[A] // difference
    def unary_! : MySet[A]
  }

  class EmptyLinkedSet[A] extends MySet[A] {
    override def contains(elem: A): Boolean = false
    override def +(elem: A): MySet[A] = new LinkedSet(elem, new EmptyLinkedSet)
    override def ++(another: MySet[A]): MySet[A] = another
    override def map[B](f: A => B): MySet[B] = new EmptyLinkedSet[B]
    override def flatMap[B](f: A => MySet[B]): MySet[B] = new EmptyLinkedSet
    override def filter(predicate: A => Boolean): MySet[A] = this
    override def foreach(f: A => Unit): Unit = ()
    override def -(elem: A): MySet[A] = this
    override def &(another: MySet[A]): MySet[A] = this
     override def --(another: MySet[A]): MySet[A] = another.map(identity)

    override def unary_! : MySet[A] = ???
  }

  class LinkedSet[A](head: A, tail: MySet[A]) extends MySet[A] {
    override def contains(elem: A): Boolean =
      if (head == elem) true
      else tail.contains(elem)

    override def +(elem: A): MySet[A] =
      if (head == elem || tail.contains(elem)) this
      else new LinkedSet(elem, this)

    override def ++(another: MySet[A]): MySet[A] =
       tail ++ (another + head)

    override def map[B](f: A => B): MySet[B] =
      tail.map(f) + f(head)

    override def flatMap[B](f: A => MySet[B]): MySet[B] =
      f(head) ++ tail.flatMap(f)

    override def filter(predicate: A => Boolean): MySet[A] =
      if (predicate(head)) new LinkedSet(head, tail.filter(predicate))
      else tail.filter(predicate)

    override def foreach(f: A => Unit): Unit = {
      f(head)
      tail foreach f
    }

    override def -(elem: A): MySet[A] =
      this.filter(_ != elem)

    override def &(another: MySet[A]): MySet[A] =
      another.filter(this.contains)

    override def --(another: MySet[A]): MySet[A] =
      //another.filter(x => !this.contains(x))
      filter(!another)

    override def unary_! : MySet[A] = ???
  }

  // all elements of type A which satisfy a property
  // { x in A | property(x) }
  class PropertyBasedSet[A](property: A => Boolean) extends MySet[A] {
    override def contains(elem: A): Boolean = property(elem)

    override def +(elem: A): MySet[A] = ???

    override def ++(another: MySet[A]): MySet[A] = ???

    override def filter(predicate: A => Boolean): MySet[A] = ???

    override def map[B](f: A => B): MySet[B] = ???

    override def flatMap[B](f: A => MySet[B]): MySet[B] = ???

    override def foreach(f: A => Unit): Unit = ???

    override def -(elem: A): MySet[A] = ???

    override def &(another: MySet[A]): MySet[A] = ???

    override def --(another: MySet[A]): MySet[A] = ???

    override def unary_! : MySet[A] = ???
  }

  object LinkedSet {
    def apply[A](elems: A*): MySet[A] = {
      @tailrec def build (items: Seq[A], acc: MySet[A]): MySet[A] = {
        if (items.tail.nonEmpty) build(items.tail, acc + items.head)
        else acc + items.head
      }

      build(elems, new EmptyLinkedSet[A])
    }
  }

  val s = LinkedSet(1,2,3,4)
  val notS = !s


  s + 5 ++ LinkedSet(-1,-2) + 3 flatMap(x => LinkedSet(x, 10 * x)) filter (_ % 2 == 0) foreach println
}
