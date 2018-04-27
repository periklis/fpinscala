package org.fpinscala.chapters.nine

trait Parsers[ParseError, Parser[+ _]] { self =>

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  // Law: run(char(c))(c.toString) === Right(c)
  def char[A](c: Char): Parser[A]

  // Law: run(string(s))(s) === Right(s)
  def string(s: String): Parser[String]

  def or[A](p1: Parser[A], p2: Parser[A]): Parser[A]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

  // Law run(zeroOrMore(char(c)))(c.toString) === Right(1)
  // Law run(zeroOrMore(listOfN(2, char(c))))(s"${c.toString}${c.toString}") === Right(2)
  def zeroOrMore[A](p: Parser[A]): Parser[Int]

  // Law run(atLeastOne(char(c)))(c.toString) === Right(2)
  // Law run(atLeastOne(char(c)))(b.toString) === Left("Expected a least one: c")
  def atLeastOne[A](p: Parser[A]): Parser[Int]

  implicit def fromString(s: String): Parser[String] = string(s)
  implicit def operators[A](p: Parser[A])            = ParserOps(p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] =
    ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def *(): Parser[Int] = self.zeroOrMore(p)
    def +(): Parser[Int] = self.atLeastOne(p)
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
  }
}
