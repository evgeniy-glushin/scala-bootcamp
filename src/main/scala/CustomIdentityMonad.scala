import cats.Monad
import cats.data.EitherT

import scala.annotation.tailrec
import scala.concurrent.Future
import concurrent.ExecutionContext.Implicits.global

object CustomIdentityMonad extends App {
  type Identity[T] = T

  implicit object IdentityMonad extends Monad[Identity]:
    override def flatMap[A, B](fa: Identity[A])(f: A => Identity[B]): Identity[B] = f(fa)
    override def pure[A](x: A): Identity[A] = x
     override def tailRecM[A, B](a: A)(f: A => Identity[Either[A, B]]): Identity[B] = ???

  sealed trait Tree[+T]
  final case class Leaf[+T](value: T) extends Tree[T]
  final case class Branch[+T](left: Tree[T], right: Tree[T]) extends Tree[T]

  implicit object TreeMonad extends Monad[Tree]:
    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match
      case Leaf(v) => f(v)
      case Branch(lf, rg) => Branch(flatMap(lf)(f), flatMap(rg)(f))
    override def pure[A](x: A): Tree[A] = Leaf(x)

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = ???

  val bandwidths = Map(
    "server1.yev.com" -> 50,
    "server1.yev.com" -> 300,
    "server3.yev.com" -> 170
  )

  type AsyncResponse[T] = EitherT[Future, String, T]

  def getBandwidth(server: String): AsyncResponse[Int] = bandwidths.get(server) match {
    case None => EitherT(Future(Left(s"Server $server unreachable")))
    case Some(b) => EitherT.right(Future(b)) //EitherT(Future(Right(b)))
  }

  //import cats.instances.future._
  def canWithstandSurge(s1: String, s2: String): AsyncResponse[Boolean] =
    for
      b1 <- getBandwidth(s1)
      b2 <- getBandwidth(s2)
    yield b1 + b2 > 250


  def generateTrafficSpikeReport(s1: String, s2: String): AsyncResponse[String] =
    canWithstandSurge(s1,s2).transform {
      case Left(reason) => Left(reason)
      case Right(false) => Left("error")
      case Right(true) => Right("success")
    }


//  type AsyncResponseV2[T] = Future[Either[String, T]]
//
//  def test(s1: AsyncResponseV2[Int], s2: AsyncResponseV2[Int]) =
//    for
//      r1 <- s1
//      r2 <- s2
//    yield r1
}
