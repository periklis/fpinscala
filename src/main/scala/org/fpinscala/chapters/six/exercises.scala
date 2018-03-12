package org.fpinscala.chapters.six

import scala.annotation.tailrec

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

    @tailrec
    private def rng[A](g: RNG, f: Int => Boolean, c: Int => A): (A, RNG) = {
      g.nextInt match {
        case (v, r) if f(v) => (c(v), r)
        case (_, r)         => rng(r, f, c)
      }
    }

    type Rand[+A] = RNG => (A, RNG)

    def unit[A](a: A): Rand[A] =
      rng => (a, rng)

    def map[A, B](r: Rand[A])(f: A => B): Rand[B] =
      g => {
        val (v, rng2) = r(g)
        (f(v), rng2)
      }

    def map2[A, B, C](a: Rand[A], b: Rand[B])(f: (A, B) => C): Rand[C] =
      rng => {
        val (v1, rng2) = a(rng)
        val (v2, rng3) = b(rng2)
        (f(v1, v2), rng3)
      }

    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
      rng => {
        fs match {
          case Nil => (Nil, rng)
          case x :: xs => {
            val (v, s) = x(rng)
            map(sequence(xs))(v :: _)(s)
          }
        }
      }

    def flatMap[A, B](g: Rand[A])(f: A => Rand[B]): Rand[B] =
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
      def map[A, B](g: Rand[A])(f: A => B): Rand[B] =
        flatMap(g)(a => unit(f(a)))(_)

      def map2[A, B, C](g: Rand[A], h: Rand[B])(f: (A, B) => C): Rand[C] =
        flatMap(g)(a => map(h)(b => f(a, b)))(_)
    }

    object WithRand {
      val double: Rand[Double] =
        map(nonNegativeNextInt(_))(a => a / (Int.MaxValue.toDouble + 1))
    }
  }

  case class State[S, +A](run: S => (A, S))

  object State {
    def unit[S, A](a: A): State[S, A] =
      State(s => (a, s))

    def flatMap[S, A, B](s: State[S, A])(f: A => State[S, B]): State[S, B] =
      State(s1 => {
        val (v, s2) = s.run(s1)
        f(v).run(s2)
      })

    def map[S, A, B](s: State[S, A])(f: A => B): State[S, B] =
      flatMap(s)(a => unit(f(a)))

    def map2[S, A, B, C](s1: State[S, A], s2: State[S, B])(f: (A, B) => C): State[S, C] =
      flatMap(s1)(a => map(s2)(b => f(a, b)))

    def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
      State(s => {
        fs match {
          case Nil => (Nil, s)
          case x :: xs => {
            val (v, s1) = x.run(s)
            map(sequence(xs))(v :: _).run(s1)
          }
        }
      })

    // $COVERAGE-OFF$
    def get[S]: State[S, S] = State(s => (s, s))

    def set[S](s: S): State[S, Unit] = State(_ => ((), s))

    def modify[S](f: S => S): State[S, Unit] =
      flatMap(get[S])(s => set(f(s)))
    // $COVERAGE-ON$

    def toA[S, A](s: S, st: State[S, A]): A =
      st.run(s)._1
  }

  type RNG2[A] = State[RNG, A]

  object RNG2 {
    def nonNegativeNextInt: RNG2[Int] =
      State(RNG.nonNegativeNextInt)
  }

  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input
  case class Machine(locked: Boolean, coins: Int, candies: Int)

  def simulateMachine(is: List[Input]): State[Machine, (Int, Int)] =
    is match {
      case Nil             => State(s => ((s.coins, s.candies), s))
      case input :: inputs => State.flatMap(nextState(input))(_ => simulateMachine(inputs))
    }

  private def nextState(i: Input): State[Machine, (Int, Int)] =
    i match {
      case Coin => insertCoin
      case Turn => turnKnob
    }

  private def insertCoin: State[Machine, (Int, Int)] =
    State(
      m =>
        if (m.locked && m.candies > 0)
          ((m.coins + 1, m.candies), m.copy(locked = false, coins = m.coins + 1))
        else
          ((m.coins, m.candies), m)
    )

  private def turnKnob: State[Machine, (Int, Int)] =
    State(
      m =>
        if (!m.locked)
          ((m.coins, m.candies - 1), m.copy(locked = true, candies = m.candies - 1))
        else
          ((m.coins, m.candies), m)
    )
}
