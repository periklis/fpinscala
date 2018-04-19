package org.fpinscala.chapters.five

object Exercises {

  sealed trait Stream[+A]
  case object Empty                                   extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
      lazy val head = h
      lazy val tail = t
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty)
        empty
      else
        cons(as.head, apply(as.tail: _*))

    def headOption[A](s: Stream[A]): Option[A] =
      s match {
        case Empty      => None
        case Cons(h, _) => Some(h())
      }

    def toList[A](s: Stream[A]): List[A] =
      s match {
        case Empty      => Nil
        case Cons(h, t) => h() :: toList(t())
      }

    def toListDeep[A](ss: Stream[Stream[A]]): List[A] =
      ss match {
        case Empty => Nil
        case Cons(h, t) =>
          h() match {
            case Empty        => Nil
            case Cons(h1, t1) => (h1() :: toList(t1())) ::: toListDeep(t())
          }
      }

    def take[A](as: Stream[A], n: Int): Stream[A] =
      as match {
        case Cons(h, t) if n != 0 => Cons(h, () => take[A](t(), n - 1))
        case _                    => Empty
      }

    def drop[A](as: Stream[A], n: Int): Stream[A] =
      as match {
        case Cons(_, t) if n != 0 => drop[A](t(), n - 1)
        case _                    => as
      }

    def takeWhile[A](as: Stream[A])(f: A => Boolean): Stream[A] =
      as match {
        case Cons(h, t) if f(h()) => Cons(h, () => takeWhile[A](t())(f))
        case _                    => Empty
      }

    def dropWhile[A](as: Stream[A])(f: A => Boolean): Stream[A] =
      as match {
        case Cons(h, t) if f(h()) => dropWhile[A](t())(f)
        case _                    => as
      }

    def foldRight[A, B](as: Stream[A])(z: => B)(f: (A, => B) => B): B =
      as match {
        case Cons(h, t) => f(h(), foldRight(t())(z)(f))
        case _          => z
      }

    def exists[A](as: Stream[A])(f: A => Boolean): Boolean =
      foldRight(as)(false)((a, b) => f(a) || b)

    def forAll[A](as: Stream[A])(f: A => Boolean): Boolean =
      foldRight(as)(true)((a, b) => f(a) && b)

    object WithFoldRight {
      def takeWhile[A](as: Stream[A])(f: A => Boolean): Stream[A] =
        foldRight(as)(Stream[A]())((a, b) => if (f(a)) cons(a, b) else empty)

      def headOption[A](as: Stream[A]): Option[A] =
        foldRight(as)(None: Option[A]) { (a, b) =>
          (a, b) match {
            case (h, _) => Some(h)
            case _      => b
          }
        }
    }

    def map[A, B](as: Stream[A])(f: A => B): Stream[B] = {
      def mapf(a: A, b: => Stream[B]): Stream[B] = cons(f(a), b)
      foldRight(as)(Empty: Stream[B])(mapf)
    }

    def filter[A](as: Stream[A])(f: A => Boolean): Stream[A] = {
      def filterf(a: A, b: => Stream[A]): Stream[A] =
        if (f(a)) cons(a, b) else b

      foldRight(as)(Empty: Stream[A])(filterf)
    }

    def append[A](as: Stream[A], bs: Stream[A]): Stream[A] = {
      def appendf(a: A, b: => Stream[A]): Stream[A] =
        b match {
          case Empty  => cons(a, bs)
          case cs @ _ => cons(a, cs)
        }

      foldRight(as)(bs)(appendf)
    }

    def flatMap[A, B](as: Stream[A])(f: A => Stream[B]): Stream[B] = {
      def mapf(a: A, b: => Stream[B]): Stream[B] = append(f(a), b)
      foldRight(as)(Empty: Stream[B])(mapf)
    }

    // $COVERAGE-OFF$
    object Infinite {
      def constant[A](a: A): Stream[A] =
        cons(a, constant(a))

      def from(n: Int): Stream[Int] =
        cons(n, from(n + 1))

      def fibs: Stream[BigInt] = {
        def fibsf(acc: Stream[BigInt], a: BigInt, b: BigInt): Stream[BigInt] = {
          val as = append(acc, Stream(a + b))
          append(as, fibsf(as, b, a + b))
        }

        fibsf(Empty, 0, 1)
      }
    }
    // $COVERAGE-ON$

    def unfold[A, S](s: S)(f: S => Option[(A, S)]): Stream[A] =
      f(s) match {
        case None         => Empty
        case Some((a, s)) => cons(a, unfold(s)(f))
      }

    // $COVERAGE-OFF$
    object InfiniteWithUnfold {
      def ones: Stream[Int] =
        constant(1)

      def constant[A](a: A): Stream[A] =
        unfold(a)(s => Some((s, s)))

      def from(n: Int): Stream[Int] =
        unfold(n)(s => Some((s, s + 1)))

      def fibs: Stream[BigInt] =
        unfold((BigInt(0), BigInt(1))) { case (a, b) => Some((b, (b, a + b))) }
    }
    // $COVERAGE-ON$

    object WithUnfold {
      def take[A](as: Stream[A], n: Int): Stream[A] =
        unfold((as, n)) {
          case (as, n) =>
            as match {
              case Cons(h, t) if n != 0 => Some((h(), (t(), n - 1)))
              case _                    => None
            }
        }

      def takeWhile[A](as: Stream[A])(f: A => Boolean): Stream[A] =
        unfold(as) {
          _ match {
            case Cons(h, t) if f(h()) => Some((h(), t()))
            case _                    => None
          }
        }

      def zipWith[A](as: Stream[A], bs: Stream[A])(f: (A, A) => A): Stream[A] =
        unfold((as, bs)) {
          case (a, b) =>
            (a, b) match {
              case (Empty, Empty)               => None
              case (Cons(h1, t1), Empty)        => Some((h1(), (t1(), Empty)))
              case (Empty, Cons(h2, t2))        => Some((h2(), (Empty, t2())))
              case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
            }
        }

      def zipAll[A, B](as: Stream[A], bs: Stream[B]): Stream[(Option[A], Option[B])] =
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

    def startsWith[A](as: Stream[A], bs: Stream[A]): Boolean =
      (as, bs) match {
        case (Empty, Empty)               => true
        case (Cons(_, _), Empty)          => false
        case (Empty, Cons(_, _))          => true
        case (Cons(h1, t1), Cons(h2, t2)) => (h1() == h2() && startsWith(t1(), t2()))
      }

    def tails[A](as: Stream[A]): Stream[Stream[A]] =
      unfold(as) {
        _ match {
          case Empty => None
          case s @ _ => Some((s, drop(s, 1)))
        }
      }

    def hasSubsequence[A](as: Stream[A], ns: Stream[A]): Boolean =
      exists(tails(as))((ss: Stream[A]) => startsWith(ns, ss))

    def scanRight[A, B](as: Stream[A])(z: B)(f: (A, => B) => B): Stream[Stream[B]] =
      unfold(as) {
        _ match {
          case Empty                      => None
          case Cons(h, t) if t() == Empty => Some((Stream(f(h(), z), z), t()))
          case s @ _                      => Some((Stream(foldRight(s)(z)(f)), drop(s, 1)))
        }
      }
  }
}
