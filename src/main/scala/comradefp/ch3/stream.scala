package ch3.stream

sealed trait Stream[+A]

case object Nil extends Stream[Nothing]

case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A]

object Stream {
  def empty[A]: Stream[A] = Nil

  // lazy evaluation
  def cons[A](head: => A, tail: => Stream[A]): Stream[A] = {
    lazy val hd = head
    lazy val tl = tail
    Cons(() => hd, () => tl)
  }

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty[A]
    else cons(as.head, apply(as.tail: _*))

  object ops {
    implicit def streamOps[A](stream: Stream[A]): StreamOps[A] = new StreamOps(stream)
  }
}

private[stream] final class StreamOps[A](stream: Stream[A]) {
  import Stream.ops._

  // force to evaluate
  //@annotation.tailrec
  def toList(): List[A] = {
    @annotation.tailrec
    def go(xs: Stream[A], acc: List[A]): List[A] = xs match {
      case Nil => acc
      case x: Cons[A] =>
        go(x.tail(), acc :+ x.head())
    }
    go(stream, List[A]())
  }

  def foldLeft[B](z: B)(f: (B, A) => B): B =
    stream match {
      case Nil => z
      case Cons(h, t) =>
        t().foldLeft(f(z, h()))(f)
    }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = stream match {
    case Nil        => z
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
  }

  def exists(p: A => Boolean): Boolean =
    foldLeft(false)((acc, a) => p(a) || acc)
}
