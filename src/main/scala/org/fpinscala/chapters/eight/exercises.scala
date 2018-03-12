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
          case (t2, r2)        => (false, r2)
        }
      }))

    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
      Gen(State.sequence(List.fill(n)(g.sample)))
  }

  object Prop {
    def forAll[A](a: Gen[A])(pred: A => Boolean): Prop = ???
  }
}
