package gen

import state._, State.ops._
import rng._, RNG._

case class Gen[A](sample: Rand[A])

object Gen {
  import ops._

  def unit[A](a: => A): Gen[A] = Gen(Rand.unit(a))
  def boolean: Gen[Boolean] = Gen(Rand.int.map(_ % 2 == 0))
  def choose(start: Int, stopExeclusive: Int): Gen[Int] =
    Gen(Rand.nonNegotiateInt.map(x => start + (x % (stopExeclusive - start))))
  def listOfN[A](n: Int, gen: Gen[A]): Gen[List[A]] = {
    def go(n: Int, acc: Gen[List[A]]): Gen[List[A]] =
      if (n == 0) acc
      else {
        val next = for {
          a <- gen
          list <- acc
        } yield list :+ a
        go(n - 1, next)
      }
    go(n, unit(List[A]()))
  }

  object ops {
    implicit def genOps[A](gen: Gen[A]): GenOps[A] =
      new GenOps(gen)
  }
}

private[gen] class GenOps[A](gen: Gen[A]) {
  import Gen._, ops._

  def product[B](b: Gen[B]): Gen[(A, B)] =
    Gen(gen.sample.product(b.sample))

  def map[B](f: A => B): Gen[B] =
    Gen(gen.sample.map(f))

  def map2[B, C](b: Gen[B])(f: (A, B) => C): Gen[C] =
    product(b).map { case (x, y) => f(x, y) }

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(gen.sample.flatMap(x => f(x).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] = for {
    n <- size
    list <- Gen.listOfN(n, gen)
  } yield list

  def listOfN(n: Int): Gen[List[A]] = Gen.listOfN(n, gen)

  def union(a: Gen[A]): Gen[A] =
    Gen.boolean.flatMap(x => if (x) gen else a)
}
