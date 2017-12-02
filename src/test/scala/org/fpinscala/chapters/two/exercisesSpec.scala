package org.fpinscala.chapters.two

import org.fpinscala.chapters.two.Exercises._
import org.scalatest.{FlatSpec, Matchers}

class ExercisesSpec extends FlatSpec with Matchers {

  behavior of "Exercise 2.1"

  it should "for 1st fibonacci return 0" in {
    fibonacci(0) shouldBe 0
  }

  it should "for 2nd fibonacci return 1" in {
    fibonacci(1) shouldBe 1
  }

  it should "for 3rd fibonacci return 1" in {
    fibonacci(2) shouldBe 1
  }

  it should "for 4th fibonacci return 2" in {
    fibonacci(3) shouldBe 2
  }

  it should "for nthFibonacci return the sum of the previous two fibonacci numbers" in {
    val sum = fibonacci(7) + fibonacci(8)

    sum shouldBe 34
    fibonacci(9) shouldBe sum
  }

  behavior of "Exercise 2.2"

  it should "return true on a asc sorted array of ints" in {
    val sorted = Array(1, 2, 3)

    val ascCheck: (Int, Int) => Boolean =
      (a, b) => a <= b

    isSorted(sorted, ascCheck) shouldBe true
  }

  it should "return false on a unsorted array of strings" in {
    val unsorted = Array("zzz", "aaa", "bbb")

    val strCheck: (String, String) => Boolean =
      (a, b) => a <= b

    isSorted(unsorted, strCheck) shouldBe false
  }

  behavior of "Exercise 2.3"

  it should "return a curried function with a single argument" in {
    val twoArgFunc: (Int, Int) => Int =
      (a, b) => a + b

    curry(twoArgFunc) shouldBe a[Function1[_, Function1[Int, Int]]]
    curry(twoArgFunc)(1)(1) shouldBe twoArgFunc(1, 1)
  }

  behavior of "Exercise 2.4"

  it should "return an uncurried function with two arguments" in {
    val singleArgFunc: Int => Int => Int =
      a => b => a + b

    uncurry(singleArgFunc) shouldBe a[Function2[_, _, Int]]
    uncurry(singleArgFunc)(1, 1) shouldBe singleArgFunc(1)(1)
  }

  behavior of "Exercise 2.5"

  it should "return the result of f(g(x))" in {
    val f: Int => Int = a => a * a
    val g: Int => Int = a => a + 2

    compose(f, g) shouldBe a[Function1[_, Int]]
    compose(f, g)(2) shouldBe 16
  }

  behavior of "Exercise 2.6"

  it should "return the result of g(f(x))" in {
    val f: Int => Int = a => a * a
    val g: Int => Int = a => a + 2

    andThen(f, g) shouldBe a[Function1[_, Int]]
    andThen(f, g)(2) shouldBe 6
  }
}
