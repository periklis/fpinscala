package org.fpinscala.chapters.five

object Exercises {

  sealed trait Stream[+A]
  case object Empty                                   extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A]: (=> A) => (=> Stream[A]) => Stream[A] =
      h =>
        t => {
          lazy val head = h
          lazy val tail = t
          Cons(() => head, () => tail)
      }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty)
        empty
      else
        cons(as.head)(apply(as.tail: _*))

    def toList[A]: Stream[A] => List[A] =
      _ match {
        case Empty      => Nil
        case Cons(h, t) => h() :: toList(t())
      }

    def take[A]: Stream[A] => Int => Stream[A] =
      as =>
        n =>
          as match {
            case Cons(h, t) if n != 0 => Cons(h, () => take[A](t())(n - 1))
            case _                    => Empty
      }

    def drop[A]: Stream[A] => Int => Stream[A] =
      as =>
        n =>
          as match {
            case Cons(_, t) if n != 0 => drop[A](t())(n - 1)
            case _                    => as
      }

    def takeWhile[A]: Stream[A] => (A => Boolean) => Stream[A] =
      as =>
        f =>
          as match {
            case Cons(h, t) if f(h()) => Cons(h, () => takeWhile[A](t())(f))
            case _                    => Empty
      }

    def dropWhile[A]: Stream[A] => (A => Boolean) => Stream[A] =
      as =>
        f =>
          as match {
            case Cons(h, t) if f(h()) => dropWhile[A](t())(f)
            case _                    => as
      }
  }
}
