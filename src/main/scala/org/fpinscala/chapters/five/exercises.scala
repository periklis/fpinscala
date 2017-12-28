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

    def headOption[A]: Stream[A] => Option[A] =
      _ match {
        case Empty      => None
        case Cons(h, _) => Some(h())
      }

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

    def foldRight[A, B]: Stream[A] => (=> B) => ((A, => B) => B) => B =
      as =>
        z =>
          f =>
            as match {
              case Cons(h, t) => f(h(), foldRight(t())(z)(f))
              case _          => z
      }

    def exists[A]: Stream[A] => (A => Boolean) => Boolean =
      as => f => foldRight(as)(false)((a, b) => f(a) || b)

    def forAll[A]: Stream[A] => (A => Boolean) => Boolean =
      as =>
        f =>
          as match {
            case Empty => false
            case _     => foldRight(as)(true)((a, b) => f(a) && b)
      }

    object WithFoldRight {
      def takeWhile[A]: Stream[A] => (A => Boolean) => Stream[A] =
        as => f => foldRight(as)(Stream[A]())((a, b) => if (f(a)) Cons(() => a, () => b) else Empty)

      def headOption[A]: Stream[A] => Option[A] =
        foldRight(_)(None: Option[A]) { (a, b) =>
          (a, b) match {
            case (h, _) => Some(h)
            case _      => b
          }
        }
    }

    def map[A, B]: Stream[A] => (A => B) => Stream[B] =
      as =>
        f => {
          def mapf(a: A, b: => Stream[B]): Stream[B] = cons(f(a))(b)
          foldRight(as)(Empty: Stream[B])(mapf)
      }

    def filter[A]: Stream[A] => (A => Boolean) => Stream[A] =
      as =>
        f => {
          def filterf(a: A, b: => Stream[A]): Stream[A] =
            if (f(a))
              cons(a)(b)
            else
              b

          foldRight(as)(Empty: Stream[A])(filterf)
      }

    def append[A]: Stream[A] => Stream[A] => Stream[A] =
      as =>
        n => {
          def appendf(a: A, b: => Stream[A]): Stream[A] =
            b match {
              case Empty  => cons(a)(n)
              case cs @ _ => cons(a)(cs)
            }

          as match {
            case Empty => n
            case _     => foldRight(as)(Empty: Stream[A])(appendf)
          }
      }

    def flatMap[A, B]: Stream[A] => (A => Stream[B]) => Stream[B] =
      as =>
        f => {
          def mapf(a: A, b: => Stream[B]): Stream[B] = append(f(a))(b)
          foldRight(as)(Empty: Stream[B])(mapf)
      }

    // $COVERAGE-OFF$
    object Infinite {
      def constant[A]: A => Stream[A] =
        a => cons(a)(constant(a))

      def from: Int => Stream[Int] =
        n => cons(n)(from(n + 1))

      def fibs: Stream[BigInt] = {
        def fibsf(acc: Stream[BigInt], a: BigInt, b: BigInt): Stream[BigInt] = {
          val as = append(acc)(Stream(a + b))
          append(as)(fibsf(as, b, a + b))
        }

        fibsf(Empty, 0, 1)
      }
    }
    // $COVERAGE-ON$

  }
}
