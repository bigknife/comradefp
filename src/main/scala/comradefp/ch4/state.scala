package state

final case class State[S, A](run: S => (A, S))

object State {

  def unit[S, A](a: => A): State[S, A] = State { s ⇒
    (a, s)
  }

  object ops {
    implicit def stateOps[S, A](state: State[S, A]): StateOps[S, A] =
      new StateOps(state)
  }
}

private[state] final class StateOps[S, A](state: State[S, A]) {
  import State._, ops._

  def map[B](f: A ⇒ B): State[S, B] = State { s ⇒
    val (a, s1) = state.run(s)
    (f(a), s1)
  }

  def product[B](b: State[S, B]): State[S, (A, B)] = State { s ⇒
    val (xa, s1) = state.run(s)
    val (xb, s2) = b.run(s1)
    ((xa, xb), s2)
  }

  def map2[B, C](b: State[S, B])(f: (A, B) => C): State[S, C] =
    product(b).map { case (x, y) => f(x, y) }

  def flatMap[B](f: A ⇒ State[S, B]): State[S, B] = State { s ⇒
    val (xa, s1) = state.run(s)
    val sb       = f(xa)
    sb.run(s1)
  }
}
