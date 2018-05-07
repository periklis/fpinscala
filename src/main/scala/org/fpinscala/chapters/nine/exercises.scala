package org.fpinscala.chapters.nine

import org.fpinscala.chapters.eight.Exercises.Prop.forAll
import org.fpinscala.chapters.eight.Exercises.{Gen, Prop}

trait Parsers[ParseError, Parser[+ _]] { self =>

  implicit def fromString(s: String): Parser[String] = string(s)
  implicit def operators[A](p: Parser[A])            = ParserOps(p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] =
    ParserOps(f(a))

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  // Law: run(char(c))(c.toString) === Right(c)
  def char[A](c: Char): Parser[Char] =
    string(c.toString).map(_.charAt(0))

  // Law: run(string(s))(s) === Right(s)
  def string(s: String): Parser[String]

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(List.empty)
    else map2(p, listOfN(n - 1, p))(_ :: _)

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(List.empty)

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  def map[A, B](p: Parser[A])(f: A => B): Parser[B]

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)]

  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    (p ** p2) map { case (a, b) => f(a, b) }

  def slice[A](p: Parser[A]): Parser[String]

  def succeed[A](a: A): Parser[A] =
    string("") map (_ => a)

  // Exercise 9.5
  object NonStrict {
    def nonStrict[A](p: => Parser[A]): Parser[A] = p

    def product[A, B](p: Parser[A], p2: Parser[B]): Parser[(A, B)] = ???

    def map2[A, B, C](p: Parser[A], p2: Parser[B])(f: (A, B) => C): Parser[C] =
      (p ** p2) map { case (a, b) => f(a, b) }

    def many[A](p: Parser[A]): Parser[List[A]] =
      map2(p, nonStrict(many(p)))(_ :: _) or succeed(List.empty)
  }

  val numA: Char => Parser[Int] = char(_).many.slice.map(_.size)

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B]  = self.or(p, p2)
    def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def many: Parser[List[A]]                = self.many(p)
    def map[B](f: A => B): Parser[B]         = self.map(p)(f)
    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def slice: Parser[String]                = self.slice(p)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

    def productLaw[A, B, C](pa: Parser[A], pb: Parser[B], pc: Parser[C])(in: Gen[String]): Prop = {
      val lhs = ((pa ** pb) ** pc) map { case ((a, b), c) => (a, b, c) }
      val rhs = (pa ** (pb ** pc)) map { case (a, (b, c)) => (a, b, c) }
      equal(lhs, rhs)(in)
    }
  }
}
