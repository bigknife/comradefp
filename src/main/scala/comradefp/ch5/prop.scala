package prop

import gen._
import rng._
import Prop._

case class Prop(run: (TestCases, RNG) => Result)

object Prop {

  sealed trait Status
  case object Proven extends Status
  case object Unfalsified extends Status

  type SuccessCount = Int
  type FailedCase = String
  type TestCases = Int
  type Result = Either[FailedCase, (Status, SuccessCount)]

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = Prop { (n, rng) =>
    @annotation.tailrec
    def go(i: Int, n: Int, x: Option[Result]): Result = {
      if (i == n) Right((Proven, i))
      else if (x.isDefined) x.get
      else {
        val x = a.sample.run(rng)
        val r: Either[String, Boolean] = try {Right(f(x._1))}catch{case e: Throwable => Left(e.getMessage)}
        r match {
          case Left(msg) => go(i + 1, n, Some(Left(msg)))
          case Right(true) => go(i + 1, n, None)
          case Right(false) => go(i + 1, n, Some(Left(x._1.toString())))
        }
       }
    }
    go(0, n, None)
  }
}
