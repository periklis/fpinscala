package org.fpinscala.chapters.four

import org.fpinscala.chapters.four.Exercises._
import org.scalacheck.Gen

object Generators {

  def someOf[T]: Gen[T] => Gen[Option[T]] =
    _.map(Some.apply)

  def optionOf[T]: Gen[T] => Gen[Option[T]] =
    g => Gen.frequency(1 -> None, 9 -> someOf(g))

  def traversableOf[T, U]: Gen[T] => (T => U) => Gen[(List[Option[T]], Option[List[U]])] =
    g =>
      f =>
        for {
          option   <- optionOf(g)
          actual   <- listOfOption(option)
          expected <- optionOfList(option)(actual)(f)
        } yield (actual, expected)

  private def listOfOption[T]: Option[T] => Gen[List[Option[T]]] =
    option =>
      option match {
        case Some(_) => Gen.nonEmptyListOf(option)
        case _       => Nil
    }

  private def optionOfList[T, U]: Option[T] => List[Option[T]] => (T => U) => Gen[Option[List[U]]] =
    option =>
      actual =>
        f =>
          option match {
            case Some(_) => Some(actual.map(_ match { case Some(v) => f(v) }))
            case _       => None
    }
}
