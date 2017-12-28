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

    def unfold[A, S]: S => (S => Option[(A, S)]) => Stream[A] =
      s =>
        f =>
          f(s) match {
            case None         => Empty
            case Some((a, s)) => append(Stream(a))(unfold(s)(f))
      }

    // $COVERAGE-OFF$
    object InfiniteWithUnfold {
      def ones: Stream[Int] =
        constant(1)

      def constant[A]: A => Stream[A] =
        a => unfold(a)(s => Some((s, s)))

      def from: Int => Stream[Int] =
        n => unfold(n)(s => Some((s, s + 1)))

      def fibs: Stream[BigInt] =
        unfold((BigInt(0), BigInt(1))) { case (a, b) => Some((b, (b, a + b))) }
    }
    // $COVERAGE-ON$

    object WithUnfold {
      def take[A]: Stream[A] => Int => Stream[A] =
        as =>
          n =>
            unfold((as, n)) {
              case (as, n) =>
                as match {
                  case Cons(h, t) if n != 0 => Some((h(), (t(), n - 1)))
                  case s @ _                => None
                }
        }

      def takeWhile[A]: Stream[A] => (A => Boolean) => Stream[A] =
        as =>
          f =>
            unfold(as) {
              _ match {
                case Cons(h, t) if f(h()) => Some((h(), t()))
                case s @ _                => None
              }
        }

      def zipWith[A]: Stream[A] => Stream[A] => ((A, A) => A) => Stream[A] =
        as =>
          bs =>
            f =>
              unfold((as, bs)) {
                case (a, b) =>
                  (a, b) match {
                    case (Empty, Empty)               => None
                    case (Cons(h1, t1), Empty)        => Some((h1(), (t1(), Empty)))
                    case (Empty, Cons(h2, t2))        => Some((h2(), (Empty, t2())))
                    case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
                  }
        }

      def zipAll[A, B]: Stream[A] => Stream[B] => Stream[(Option[A], Option[B])] =
        as =>
          bs =>
            unfold((as, bs)) {
              case (a, b) =>
                (a, b) match {
                  case (Empty, Empty)               => None
                  case (Cons(h, t), Empty)          => Some(((Some(h()), None), (t(), Empty)))
                  case (Empty, Cons(h, t))          => Some(((None, Some(h())), (Empty, t())))
                  case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
                }
        }
    }
  }
}
