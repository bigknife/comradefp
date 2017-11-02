package rng

import alg.lcg
import state._

sealed trait RNG {
  def nextInt: (Int, RNG)
}

private[rng] case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val seed2 = lcg(seed)
    ((seed2 >>> 16).asInstanceOf[Int], SimpleRNG(seed2))
  }
}

object RNG {
  def simple(seed: Long): RNG = SimpleRNG(seed)

  type Rand[A] = State[RNG, A]

  object Rand {
    import State._, ops._

    val int: Rand[Int] = State(_.nextInt)
    val double: Rand[Double] = int.map(_.toDouble / Double.MaxValue)
    val nonNegotiateInt: Rand[Int] = int.map {x =>
      (if (x == Int.MinValue) x + 1 else x).abs
    }
  }
}
