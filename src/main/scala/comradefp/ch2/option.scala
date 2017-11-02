package option

import scala.language.higherKinds

sealed trait Option[+A]

case object None extends Option[Nothing]

case class Some[+A](get: A) extends Option[A]

object Option {
  import ops._
  // smart ctor
  def apply[A](a: A): Option[A] =
    if (a == null) None
    else Some(a)

  def apply[A](): Option[A] = None

  def lift[A, B](f: A ⇒ B): Option[A] ⇒ Option[B] = _.map(f)

  object syntax {
    implicit def optionSyntax[A](a: A): OptionSyntax[A] = new OptionSyntax(a)
  }
  object ops {
    implicit def optionOps[A](option: Option[A]): OptionOps[A] = new OptionOps(option)
  }
}

final class OptionSyntax[A](a: A) {
  def some: Option[A] = Option(a)
  def none: Option[A] = Option()
}

final class OptionOps[A](option: Option[A]) {
  import Option.syntax._
  import Option.ops._

  def isEmpty: Boolean = option match {
    case None ⇒ true
    case _    ⇒ false
  }

  def isDefined: Boolean = !isEmpty

  // a should be changed to by-name parameter
  def getOrElse(a: ⇒ A): A = option match {
    case None    ⇒ a
    case Some(x) ⇒ x
  }

  def orElse(a: ⇒ Option[A]): Option[A] = if (isEmpty) a else option

  def map[B](f: A ⇒ B): Option[B] = option match {
    case None    ⇒ None
    case Some(a) ⇒ Some(f(a))
  }

  def map2[B, C](optionB: Option[B])(f: (A, B) ⇒ C): Option[C] =
    (option, optionB) match {
      case (None, _)          ⇒ None
      case (_, None)          ⇒ None
      case (Some(a), Some(b)) ⇒ Some(f(a, b))
    }

  def filter(f: A ⇒ Boolean): Option[A] = option match {
    case Some(x) if f(x) ⇒ Some(x)
    case _               ⇒ None
  }

  def flatMap[B](f: A ⇒ Option[B]): Option[B] = option match {
    case None    ⇒ None
    case Some(a) ⇒ f(a)
  }

  def flatMapOverMap2[B](f: A ⇒ Option[B]): Option[B] =
    map2(().some)((a, _) ⇒ f(a)).getOrElse(None)
}
