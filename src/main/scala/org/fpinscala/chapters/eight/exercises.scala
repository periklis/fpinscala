package org.fpinscala.chapters.eight

import org.fpinscala.chapters.six.Exercises.{State, RNG}
import scala.util.control.NonFatal

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

  sealed trait Result {
    val isFalsified: Boolean
  }
  case object Passed extends Result {
    val isFalsified = false
  }
  case class Failed(failure: Prop.FailedCase, successes: Prop.SuccessCount) extends Result {
    val isFalsified = true
  }

  case class Prop(run: (Prop.TestCases, RNG) => Result) {}

  object Prop {
    type TestCases    = Int
    type FailedCase   = String
    type SuccessCount = Int

    def forAll[A](ga: Gen[A])(pred: A => Boolean): Prop = Prop {
      case (testcases, rng) =>
        val values = Gen.listOfN(testcases, ga).sample.run(rng)._1

        values
          .zip(Range(1, testcases))
          .map {
            case (a, c) =>
              try {
                if (pred(a)) Passed
                else Failed(s"Failed: ${a.toString()} Passed: $c", c)
              } catch {
                case NonFatal(e) => Failed(s"Failed: ${a.toString}\n Passed: $c\n Msg: ${e.getMessage} ", c)
              }
          }
          .find(_.isFalsified)
          .getOrElse(Passed)
    }

    private def &&(lhs: Prop, rhs: Prop): Prop = Prop {
      case (testcases, rng) =>
        (lhs.run(testcases, rng), rhs.run(testcases, rng)) match {
          case (Passed, Passed)                 => Passed
          case (lf @ Failed(_, _), Passed)      => lf
          case (Passed, rf @ Failed(_, _))      => rf
          case (Failed(lf, lc), Failed(rf, rc)) => Failed(s"Left: $lf Right: $rf", lc + rc)
        }
    }

    private def ||(lhs: Prop, rhs: Prop): Prop = Prop {
      case (testcases, rng) =>
        (lhs.run(testcases, rng), rhs.run(testcases, rng)) match {
          case (Passed, _)                      => Passed
          case (_, Passed)                      => Passed
          case (Failed(lf, lc), Failed(rf, rc)) => Failed(s"Left: $lf Right: $rf", lc + rc)
        }
    }

    object Implicits {
      implicit class PropWithOps(p: Prop) {
        def &&(o: Prop): Prop = Prop.&&(p, o)
        def ||(o: Prop): Prop = Prop.||(p, o)
      }
    }
  }
}
