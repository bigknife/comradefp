package prop

import gen._
import Prop._

sealed trait Prop

object Prop {

  sealed trait Status
  case object Proven extends Status
  case object Unfalsified extends Status

  type SuccessCount = Int
  type FailedCase = String
  type TestCases = Int
  type Result = Either[FailedCase, (Status, SuccessCount)]

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = ???
}
