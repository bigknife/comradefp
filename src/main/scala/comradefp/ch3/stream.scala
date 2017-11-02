package ch3.stream

sealed trait Stream[+A]

case object Nil extends Stream[Nothing]

case class Cons[+A](head: () ⇒ A, tail: () ⇒ Stream[A]) extends Stream[A] {
  override def toString: String = "head() ~ tail())"
}

object Stream {
  def empty[A]: Stream[A] = Nil

  // lazy evaluation
  def cons[A](head: ⇒ A, tail: ⇒ Stream[A]): Stream[A] = {
    lazy val hd = head
    lazy val tl = tail
    Cons(() ⇒ hd, () ⇒ tl)
  }

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty[A]
    else cons(as.head, apply(as.tail: _*))

  def unfold[A, S](s: S)(f: S ⇒ Option[(A, S)]): Stream[A] = f(s) match {
    case Some((a, s1)) ⇒ Cons(() ⇒ a, () ⇒ unfold(s1)(f))
    case None          ⇒ empty[A]
  }

  object ops {
    implicit def streamOps[A](stream: Stream[A]): StreamOps[A] = new StreamOps(stream)
  }

  object syntax {
    implicit def streamSyntax[A](a: A): StreamSyntax[A] =
      new StreamSyntax(a)
  }
}

private[stream] final class StreamOps[A](stream: Stream[A]) {
  import Stream._, ops._

  // force to evaluate
  //@annotation.tailrec
  def toList(): List[A] = {
    @annotation.tailrec
    def go(xs: Stream[A], acc: List[A]): List[A] = xs match {
      case Nil        ⇒ acc
      case x: Cons[A] ⇒ go(x.tail(), acc :+ x.head())
    }
    go(stream, List[A]())
  }

  def tail: Stream[A] = stream match {
    case Nil        ⇒ Nil
    case Cons(h, t) ⇒ t()
  }

  def foldLeft[B](z: B)(f: (B, A) ⇒ B): B =
    stream match {
      case Nil ⇒ z
      case Cons(h, t) ⇒
        t().foldLeft(f(z, h()))(f)
    }

  def foldRight[B](z: ⇒ B)(f: (A, ⇒ B) ⇒ B): B = stream match {
    case Nil        ⇒ z
    case Cons(h, t) ⇒ f(h(), t().foldRight(z)(f))
  }

  def map[B](f: A ⇒ B): Stream[B] = stream match {
    case Nil        ⇒ Nil
    case Cons(h, t) ⇒ Cons(() ⇒ f(h()), () ⇒ t().map(f))
  }

  def product[B](b: Stream[B]): Stream[(A, B)] = (stream, b) match {
    case (Nil, _)                     ⇒ Nil
    case (_, Nil)                     ⇒ Nil
    case (Cons(h1, t1), Cons(h2, t2)) ⇒ Cons(() ⇒ (h1(), h2()), () ⇒ t1().product(t2()))
  }

  def map2[B, C](b: Stream[B])(f: (A, B) ⇒ C): Stream[C] =
    product(b).map { case (x, y) => f(x, y) }

  def headOption: Option[A] =
    foldRight(None: Option[A])((a, acc) => Some(a))

  def zip[B](streamB: Stream[B]): Stream[(A, B)] = map2(streamB)((_, _))

  def take(n: Int): Stream[A] = stream match {
    case Nil                  ⇒ Nil
    case Cons(h, t) if n > 1  ⇒ Cons(h, () ⇒ t().take(n - 1))
    case Cons(h, _) if n == 1 ⇒ Cons(h, () ⇒ Nil)
  }

  def takeWhile(p: A => Boolean): Stream[A] = stream match {
    case Cons(h, t) if p(h()) ⇒ Cons(h, () => t().takeWhile(p))
    case _                    ⇒ Nil
  }

  def exists(p: A ⇒ Boolean): Boolean =
    foldRight(false)((a, acc) ⇒ p(a) || acc)

  def forAll(p: A ⇒ Boolean): Boolean =
    foldRight(true)((a, acc) ⇒ p(a) && acc)

}

private[stream] final class StreamSyntax[A](a: A) {
  import Stream._, ops._, syntax._
  def repeat: Stream[A] =
    succeed(x ⇒ x)

  def succeed(f: A ⇒ A): Stream[A] =
    unfold(a)(a ⇒ Some((a, f(a))))

}

case object StreamCandy {
  import Stream._, ops._, syntax._

  def fib: Stream[Long] = unfold((0L, 0L)) {
    case (0, 0) ⇒ Some((0, (0, 1)))
    case (x, y) ⇒ Some((y, (y, x + y)))
  }
}
