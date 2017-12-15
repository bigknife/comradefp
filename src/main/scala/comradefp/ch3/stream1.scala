package stream1

sealed trait Stream[+A] {
  def uncons: Option[(A, Stream[A])]
  def isEmpty: Boolean = uncons.isEmpty
}

object Stream {
  def empty[A]: Stream[A] = new Stream[A] {
    def uncons: Option[(A, Stream[A])] = None

    override def toString: String = "Stream()"
  }

  def cons[A](head: ⇒ A, tail: ⇒ Stream[A]): Stream[A] = new Stream[A] {
    def uncons: Option[(A, Stream[A])] = {
      lazy val h = head
      lazy val t = tail
      Some((h, t))
    }

    override def toString: String = {
      lazy val h = head
      s"Stream($h, <tail>)"
    }
  }

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty[A]
    else cons(as.head, apply(as.tail: _*))

  // force to evaluate
  def toList[A](stream: Stream[A]): List[A] = {
    def go(acc: List[A], stream: Stream[A]): List[A] = stream.uncons match {
      case None            ⇒ acc
      case Some((a, tail)) ⇒ go(acc :+ a, tail)
    }

    go(Nil, stream)
  }

  // common constructor
  def unfold[A, S](z: S)(f: (S) ⇒ Option[(S, A)]): Stream[A] = f(z) match {
    case None ⇒ empty[A]
    case Some((s, a)) ⇒ cons(a, unfold(s)(f))
  }

  def foldRight[A, B](stream: Stream[A], z: B)(f: (A, ⇒ B) ⇒ B): B = stream.uncons match {
    case None ⇒ z
    case Some((h, t)) ⇒ f(h, foldRight(t, z)(f))
  }

  def headOption[A](stream: Stream[A]): Option[A] = stream.uncons match {
    case None ⇒ None
    case Some((h, t)) ⇒ Some(h)
  }

  def unsafeHead[A](stream: Stream[A]): A = headOption(stream).get

  def tail[A](stream: Stream[A]): Stream[A] = stream.uncons match {
    case Some((_, t)) ⇒ t
    case _ ⇒ stream
  }


  def take[A](stream: Stream[A], n: Int): Stream[A] =
    unfold((stream, n)) {
      case (s, _) if s.isEmpty ⇒ None
      case (_, i) if i == 0 ⇒ None
      case (s, i) ⇒ Some(((tail(s), i - 1), unsafeHead(s)))
    }

  def takeWhile[A](stream: Stream[A])(p: A ⇒ Boolean): Stream[A] =
    unfold(stream) {
      case s if s.isEmpty ⇒ None
      case s ⇒ s.uncons match {
        case Some((h, t)) if p(h) ⇒ Some((t, h))
        case _ ⇒ None
      }
    }

  def succeed[A](a: A)(f: A ⇒ A): Stream[A] =
    unfold(a)(x ⇒ Some((f(x), f(x))))

  def constant[A](a: A): Stream[A] =
    succeed(a)(x ⇒ x)

  def fib: Stream[Long] =
    unfold((0L, 1L)){case (a, b) ⇒ Some(((b, a + b),a))}


  

  object ops {
    implicit def streamOps[A](stream: Stream[A]): StreamOps[A] =
      new StreamOps(stream)
  }
}

private[stream1] final class StreamOps[A](stream: Stream[A]) {
  import Stream._

  def #::(a: A): Stream[A] = stream.uncons match {
    case None ⇒ cons(a, empty[A])
    case _    ⇒ cons(a, stream)
  }
}
