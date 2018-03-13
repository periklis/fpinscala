package org.fpinscala.chapters.eight

import org.fpinscala.chapters.six.Exercises.{State, RNG}

object Exercises {

  def sum(as: List[Int]): Int =
    as.foldLeft(0)(_ + _)

  def max(as: List[Int]): Int =
    as.max

  trait SimpleProp {
    var res: Boolean
    def check: Boolean = res
    def &&(o: SimpleProp): SimpleProp = {
      res = this.check && o.check
      this
    }
  }

  trait Prop {
    type FailedCase   = String
    type SuccessCount = Int

    def check: Either[(FailedCase, SuccessCount), SuccessCount] = ???

    def &&(o: Prop): Prop = ???
  }

  case class Gen[T](sample: State[RNG, T])

  object Gen {
    def choose(start: Int, stopExclusive: Int): Gen[Int] =
      Gen(State(r => {
        RNG.nonNegativeLessThan(stopExclusive)(r) match {
          case s @ (a, _) if a > start => s
          case _                       => (start, r)
        }
      }))

    def unit[A](a: A): Gen[A] =
      Gen(State((a, _)))

    def boolean[A]: Gen[Boolean] =
      Gen(State(s => {
        RNG.nonNegativeLessThan(2)(s) match {
          case (t, r) if t > 0 => (true, r)
          case (_, r2)         => (false, r2)
        }
      }))

    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
      Gen(State.sequence(List.fill(n)(g.sample)))

    def flatMap[A, B](g: Gen[A])(f: A => Gen[B]): Gen[B] =
      Gen(State.flatMap(g.sample) { a =>
        f(a).sample
      })

    def listOfN[A](g: Gen[A], size: Gen[Int]): Gen[List[A]] =
      flatMap(size)(listOfN(_, g))

    def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
      flatMap(boolean)(if (_) g1 else g2)

    def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
      flatMap(Gen.choose(1, 101)) { w =>
        val wD = (w.toDouble / 100)
        (g1, g2) match {
          case ((l, w1), (_, _)) if wD <= w1 => l
          case ((_, _), (r, w2)) if wD <= w2 => r
        }
      }
  }

  object Prop {
    def forAll[A](a: Gen[A])(pred: A => Boolean): Prop = ???
  }
}
