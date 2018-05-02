package org.fpinscala.chapters.nine

import org.fpinscala.chapters.eight.Exercises.Prop.forAll
import org.fpinscala.chapters.eight.Exercises.{Gen, Prop}

trait Parsers[ParseError, Parser[+ _]] { self =>

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  // Law: run(char(c))(c.toString) === Right(c)
  def char[A](c: Char): Parser[Char] =
    string(c.toString).map(_.charAt(0))

  // Law: run(string(s))(s) === Right(s)
  def string(s: String): Parser[String]

  def or[A](p1: Parser[A], p2: Parser[A]): Parser[A]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

  def many[A](p: Parser[A]): Parser[List[A]]

  def map[A, B](f: A => B): Parser[B]

  def slice[A](p: Parser[A]): Parser[String]

  implicit def fromString(s: String): Parser[String] = string(s)
  implicit def operators[A](p: Parser[A])            = ParserOps(p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] =
    ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B]  = self.or(p, p2)
    def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def many(): Parser[List[A]]              = self.many(p)
    def map[B](f: A => B): Parser[B]         = p.map(f)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)
  }
}
