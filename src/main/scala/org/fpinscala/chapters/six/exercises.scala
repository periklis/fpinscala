package org.fpinscala.chapters.six

object Exercises {

  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val nextSeed = (seed * 0x5DEECE66DL) & 0xFFFFFFFFFFFFL
      val nextRNG  = SimpleRNG(nextSeed)
      val n        = (nextSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  object RNG {
    def nonNegativeNextInt: RNG => (Int, RNG) = rng(_, a => a >= 0 && a <= Int.MaxValue, _.toInt)
    def double: RNG => (Double, RNG)          = rng(_, a => a >= 0 && a < 1, _.toDouble)

    def intDouble: RNG => ((Int, Double), RNG) = g => {
      val r1 = g.nextInt
      val r2 = double(r1._2)
      ((r1._1, r2._1), r2._2)
    }

    def doubleInt: RNG => ((Double, Int), RNG) =
      g => {
        val r = intDouble(g)
        (r._1.swap, r._2)
      }

    def double3: RNG => ((Double, Double, Double), RNG) =
      g => {
        val d1 = double(g)
        val d2 = double(d1._2)
        val d3 = double(d2._2)
        ((d1._1, d2._1, d3._1), d3._2)
      }

    def ints: Int => RNG => (List[Int], RNG) =
      n =>
        g => {
          val r = nonNegativeNextInt(g)
          if (n > 0) (r._1 :: ints(n - 1)(r._2)._1, r._2) else (Nil, g)
      }

    private def rng[A](g: RNG, f: Int => Boolean, c: Int => A): (A, RNG) = {
      g.nextInt match {
        case (v, r) if f(v) => (c(v), r)
        case (_, r)         => rng(r, f, c)
      }
    }

    type Rand[+A] = RNG => (A, RNG)

    def unit[A]: A => Rand[A] =
      a => rng => (a, rng)

    def map[A, B]: Rand[A] => (A => B) => Rand[B] =
      r =>
        f =>
          g => {
            val (v, rng2) = r(g)
            (f(v), rng2)
      }

    def map2[A, B, C]: Rand[A] => Rand[B] => ((A, B) => C) => Rand[C] =
      a =>
        b =>
          f =>
            rng => {
              val (v1, rng2) = a(rng)
              val (v2, rng3) = b(rng2)
              (f(v1, v2), rng3)
      }

    def sequence[A]: List[Rand[A]] => Rand[List[A]] =
      fs =>
        rng => {
          fs match {
            case Nil => (Nil, rng)
            case x :: xs => {
              val (v, s) = x(rng)
              map(sequence(xs))(v :: _)(s)
            }
          }
      }

    def flatMap[A, B]: Rand[A] => (A => Rand[B]) => Rand[B] =
      g =>
        f =>
          rng => {
            val (v, rng2) = g(rng)
            f(v)(rng2)
      }

    def nonNegativeNextIntViaRand: Rand[Int] =
      nonNegativeNextInt(_)

    def nonNegativeLessThan: Int => Rand[Int] =
      n =>
        flatMap(nonNegativeNextIntViaRand)(a => {
          val mod = a % n
          if (a + (n - 1) - mod > 0)
            unit(mod)
          else
            nonNegativeLessThan(n)
        })(_)

    object WithFlatMap {
      def map[A, B]: Rand[A] => (A => B) => Rand[B] =
        g => f => flatMap(g)(a => unit(f(a)))(_)

      def map2[A, B, C]: Rand[A] => Rand[B] => ((A, B) => C) => Rand[C] =
        g => h => f => flatMap(g)(a => map(h)(b => f(a, b)))(_)
    }

    object WithRand {
      val double: Rand[Double] =
        map(nonNegativeNextInt(_))(a => a / (Int.MaxValue.toDouble + 1))
    }
  }

  case class State[S, +A](run: S => (A, S))

  object State {
    def unit[S, A]: A => State[S, A] =
      a => State(s => (a, s))

    def flatMap[S, A, B]: State[S, A] => (A => State[S, B]) => State[S, B] =
      s =>
        f =>
          State(s1 => {
            val (v, s2) = s.run(s1)
            f(v).run(s2)
          })

    def map[S, A, B]: State[S, A] => (A => B) => State[S, B] =
      s => f => flatMap(s)(a => unit(f(a)))

    def map2[S, A, B, C]: State[S, A] => State[S, B] => ((A, B) => C) => State[S, C] =
      s1 => s2 => f => flatMap(s1)(a => map(s2)(b => f(a, b)))

    def sequence[S, A]: List[State[S, A]] => State[S, List[A]] =
      fs =>
        State(s => {
          fs match {
            case Nil => (Nil, s)
            case x :: xs => {
              val (v, s1) = x.run(s)
              map(sequence(xs))(v :: _).run(s1)
            }
          }
        })
  }

  type RNG2[A] = State[RNG, A]

  object RNG2 {
    def nonNegativeNextInt: RNG2[Int] =
      State(RNG.nonNegativeNextInt)
  }
}
