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
    def nonNegativeNextInt: RNG => (Int, RNG) =
      g => {
        def genRandom(g: RNG): (Int, RNG) = {
          g.nextInt match {
            case r @ (v, _) if (v >= 0 && v <= Int.MaxValue) => r
            case (_, r)                                      => genRandom(r)
          }
        }

        genRandom(g)
      }
  }
}
