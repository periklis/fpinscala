package org.fpinscala.chapters.eight

object Exercises {

  def sum(as: List[Int]): Int =
    as.foldLeft(0)(_ + _)

  def max(as: List[Int]): Int =
    as.max

  trait Gen[T]

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

  object Gen {
    def choose(start: Int, stopExclusive: Int): Gen[Int] = ???

    def listOf[A](a: Gen[A]): Gen[List[A]] = ???

    def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] = ???
  }

  object Prop {
    def forAll[A](a: Gen[A])(pred: A => Boolean): Prop = ???
  }
}
