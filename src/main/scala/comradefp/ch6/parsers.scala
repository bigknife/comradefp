package parsing

import scala.language.higherKinds
import scala.language.implicitConversions

trait Parsers[Parser[+ _]] { self ⇒
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  implicit def parserOps[A](p: Parser[A]): ParserOps[A] = ParserOps(p)

  def string(s: String): Parser[String]
  def slice[A](p: Parser[A]): Parser[String]
  def flatMap[A, B](s1: Parser[A])(f: A ⇒ Parser[B]): Parser[B]
  def succeed[A](a: A): Parser[A]
  def label[A](msg: String)(p: Parser[A]): Parser[A]
  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def map[A, B](p: Parser[A])(f: A ⇒ B): Parser[B] =
    flatMap(p)(f andThen succeed)

  def product[A, B](p1: Parser[A], p2: ⇒ Parser[B]): Parser[(A, B)] =
    flatMap(p1)(a ⇒ map(p2)(b ⇒ (a, b)))

  def map2[A, B, C](s1: Parser[A], s2: ⇒ Parser[B])(f: (A, B) ⇒ C): Parser[C] =
    flatMap(s1)(a ⇒ map(s2)(b ⇒ f(a, b)))

  def char(c: Char): Parser[Char] =
    string(c.toString).map(_.charAt(0))

  def or[A](s1: Parser[A], s2: ⇒ Parser[A]): Parser[A]

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(List())

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B]  = self.or(p, p2)
    def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def map[B](f: A ⇒ B): Parser[B] = self.map(p)(f)
    def map2[B, C](p2: Parser[B])(f: (A, B) => C): Parser[C] =
      self.map2(p, p2)(f)

    def **[B](p2: Parser[B]): Parser[(A, B)]      = self.product(p, p2)
    def product[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
  }
}

case class Location(input: String, offset: Int = 0) {
  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col = input.slice(0, offset + 1).lastIndexOf('\n') match {
    case -1 ⇒ offset + 1
    case x ⇒ offset - x
  }

  def advanceBy(n: Int): Location = copy(offset = offset + n)
  def currentLine: String =
    if (input.length > 1) input.lines.drop(line - 1).next
    else ""
  def columnCaret = (" " * (col-1)) + "^"
}

case class ParseError(stack: List[(Location, String)] = List())
