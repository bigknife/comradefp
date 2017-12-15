package monoid

trait Monoid[A] {
  def zero: A
  def op(a1: A, a2: A): A
}

trait Foldable[F[_]] {
  def foldMap[A, B](as: F[A])(f: A ⇒ B)(mb: Monoid[B]): B

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) ⇒ B): B
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) ⇒ B): B

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)
}

object Monoid {

  def summon[A](z: A)(f: (A, A) ⇒ A): Monoid[A] =
    new Monoid[A] {
      def zero: A = z
      def op(a1: A, a2: A): A = f(a1, a2)
    }

  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {
      def zero: (A, B)                       = (A.zero, B.zero)
      def op(a1: (A, B), a2: (A, B)): (A, B) = (A.op(a1._1, a2._1), B.op(a1._2, a2._2))
    }

  def coproductMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[Either[A, B]] =
    new Monoid[Either[A, B]] {
      def zero: Either[A, B]                                   = Right(B.zero)
      def op(a1: Either[A, B], a2: Either[A, B]): Either[A, B] = ???
    }

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] = ???

  def functionMonoid[A, B](B: Monoid[B]): Monoid[A ⇒ B] = new Monoid[A ⇒ B] {
    def zero: A ⇒ B = (_: A) ⇒ B.zero
    def op(f1: A ⇒ B, f2: A ⇒ B): A ⇒ B = (a: A) ⇒ {
      B.op(f1(a), f2(a))
    }
  }
}
