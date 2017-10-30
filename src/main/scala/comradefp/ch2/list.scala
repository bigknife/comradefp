package ch2.list
import ch2.option._

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](a: A*): List[A] =
    if (a.isEmpty) Nil
    else Cons(a.head, apply(a.tail: _*))

  object ops {
    implicit def listOps[A](list: List[A]): ListOps[A] = new ListOps(list)
  }
}

private[list] final class ListOps[A](list: List[A]) {
  import List.ops._

  def foldRightAsPrimary[B](z: B)(f: (A, B) => B): B = list match {
    case Nil              => z
    case Cons(head, tail) => f(head, tail.foldRightAsPrimary(z)(f))
  }

  def foldRightViaFoldLeft[B](z: B)(f: (A, B) => B): B =
    foldLeft((x: B) => x)((accF, a) => b => accF(f(a, b)))(z)

  def foldRight[B](z: B)(f: (A, B) => B): B = foldRightViaFoldLeft(z)(f)

  @annotation.tailrec
  def foldLeft[B](z: B)(f: (B, A) => B): B = list match {
    case Nil              => z
    case Cons(head, tail) => tail.foldLeft(f(z, head))(f)
  }

  def map[B](f: A => B): List[B] = list match {
    case Nil              => Nil
    case Cons(head, tail) => Cons(f(head), tail.map(f))
  }

  def product[B](listB: List[B]): List[(A, B)] = (list, listB) match {
    case (Nil, _)                               => Nil
    case (_, Nil)                               => Nil
    case (Cons(head, tail), Cons(headB, tailB)) => Cons((head, headB), tail.product(tailB))
  }

  def map2[B, C](listB: List[B])(f: (A, B) => C): List[C] =
    product(listB).map(x => f(x._1, x._2))

  def headOption: Option[A] = list match {
    case Nil           => None
    case Cons(head, _) => Some(head)
  }
}
