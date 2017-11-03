package parsers

import scala.language.higherKinds
import scala.language.implicitConversions

trait Parsers[ParseError, Parser[+ _]] { self ⇒
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  implicit def parserOps[A](p: Parser[A]): ParserOps[A] = ParserOps(p)

  def char(c: Char): Parser[Char] =
    string(c.toString).map(_.charAt(0))

  def string(s: String): Parser[String]

  def succeed[A](a: A): Parser[A] = string("").map(_ ⇒ a)

  def slice[A](p: Parser[A]): Parser[String]

  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
  def many[A](p: Parser[A]): Parser[List[A]]

  def map[A, B](p: Parser[A])(f: A ⇒ B): Parser[B]

  def product[A, B](p1: Parser[A], p2: Parser[B]): Parser[(A, B)]

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B]  = self.or(p, p2)
    def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def map[B](f: A ⇒ B): Parser[B] = self.map(p)(f)

    def **[B](p2: Parser[B]): Parser[(A, B)]      = self.product(p, p2)
    def product[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
  }
}
