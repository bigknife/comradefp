package either

sealed trait Either[+E, +A]

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def left[E, A](e: E): Either[E, A]  = Left(e)
  def right[E, A](a: A): Either[E, A] = Right(a)

  object syntax {
    implicit def eitherSyntax[A](a: A): EitherSyntax[A] = new EitherSyntax(a)
  }

  object ops {
    implicit def eitherOps[E, A](either: Either[E, A]): EitherOps[E, A] = new EitherOps(either)
  }
}

private[either] final class EitherSyntax[A](a: A) {
  def left[E]: Either[A, E]  = Either.left[A, E](a)
  def right[B]: Either[B, A] = Either.right[B, A](a)
}

private[either] final class EitherOps[E, A](either: Either[E, A]) {
  def map[B](f: A ⇒ B): Either[E, B] = either match {
    case Right(value) ⇒ Right(f(value)): Either[E, B]
    case Left(value)  ⇒ Left(value): Either[E, B]
  }

  def map2[EE >: E, B, C](eitherB: Either[EE, B])(f: (A, B) ⇒ C): Either[EE, C] =
    (either, eitherB) match {
      case (Right(va), Right(vb)) ⇒ Right(f(va, vb))
      case (Left(va), _)          ⇒ Either.left[EE, C](va)
      case (_, Left(vb))          ⇒ Either.left[EE, C](vb)
    }

  def flatMap[EE >: E, B](f: A ⇒ Either[EE, B]): Either[EE, B] = either match {
    case Right(value) ⇒ f(value)
    case Left(value)  ⇒ Either.left[EE, B](value)
  }

  def orElse[EE >: E, B >: A](b: ⇒ Either[EE, B]): Either[EE, B] = either match {
    case Left(_) ⇒ b
    case x       ⇒ x
  }
}
