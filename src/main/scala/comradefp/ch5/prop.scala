package prop

import gen._
import rng._
import stream._, Stream.ops._, Stream.syntax._
import Prop._

case class Prop(run: (TestCases, RNG) => Result)

object Prop {
  // how many test cases will be generated
  type TestCases    = Int
  type FailedCase   = String
  type SuccessCount = Int

  // test result
  sealed trait Result {
    // is falsified ?
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    val isFalsified = false
  }

  case class Falsified(failed: FailedCase, succeses: SuccessCount) extends Result {
    val isFalsified: Boolean = true
  }

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = Prop { (n, rng) ⇒
    {
      def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
        Stream.unfold(rng)(rng ⇒ Some(g.sample.run(rng)))

      def buildFailedMessage[A](a: A, e: Throwable): String =
        s"test case: $a\n" +
          s"generated an exception: ${e.getMessage}\n\t" +
          e.getStackTrace.mkString("\n\t")

      randomStream(a)(rng)
        .product(0.succeed(x ⇒ x + 1))
        .take(n)
        .map {
          case (xa, i) ⇒
            try {
              if (f(xa)) Passed else Falsified(xa.toString, i)
            } catch {
              case e: Throwable ⇒ Falsified(buildFailedMessage(xa, e), i)
            }
        }
        .find(_.isFalsified)
        .getOrElse(Passed)
    }
  }

  object ops {
    implicit def propOps(p: Prop): PropOps = new PropOps(p)
  }

}

private[prop] final class PropOps(p: Prop) {
  def &&(p1: Prop): Prop = Prop { (n, rng) ⇒
    p.run(n, rng) match {
      case x: Falsified ⇒ x
      case Passed       ⇒ p1.run(n, rng)
    }
  }

  def ||(p1: Prop): Prop = Prop { (n, rng) ⇒
    p.run(n, rng) match {
      case Passed       ⇒ Passed
      case x: Falsified ⇒ p1.run(n, rng)
    }
  }
}

object PropCandy extends App {
  import gen._, Gen._, Gen.ops._
  import rng._, RNG._
  import Prop._

  def add(x: Int, y: Int): Int = {
    val r: Boolean = boolean.sample.run(simple(System.currentTimeMillis()))._1
    if (r) throw new Exception("random exception")
    else x + y
  }

  def addLaw(xy: (Int, Int)): Boolean = add(xy._1, xy._2) == add(xy._2, xy._1)

  def test(): Unit = {
    val gen: Gen[(Int, Int)] = Gen.choose(0, 100).product(Gen.choose(100, 200))
    val prop                 = forAll(gen)(addLaw(_))

    prop.run(10, rng.RNG.simple(System.currentTimeMillis())) match {
      case Passed ⇒ println("all test cases passed")
      case Falsified(failed, successes) ⇒
        System.err.println(s"only $successes cases passed, failed at: $failed")
    }

  }

  test()
}
