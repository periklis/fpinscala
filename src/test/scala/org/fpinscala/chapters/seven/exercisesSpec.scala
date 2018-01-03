package org.fpinscala.chapters.seven

import scala.concurrent._
import scala.concurrent.duration._

import org.fpinscala.chapters.seven.Exercises._
import org.scalatest.{FlatSpec, Matchers}

class ExercisesSpec extends FlatSpec with Matchers {

  implicit val ec: ExecutionContext = ExecutionContext.Implicits.global
  val d                             = 10.seconds

  behavior of "exercise 7.1"

  it should "return f-mapped result of the two parallel computations" in {
    Par.run(Par.map2(Par.unit(2))(Par.unit(3))(_ + _)) shouldBe Par.run(Par.unit(5))
  }

  behavior of "exercise 7.2 - bad approach"

  it should "return f-mapped result of the two parallel computations" in {
    val p1 = Par2.map2(Par2.unit(2), Par2.unit(3))(_ + _)
    val p2 = Par2.lazyUnit(5)

    Par2.run(p1)(d) shouldBe Par2.run(p2)(d)
  }

  behavior of "exercise 7.2 - good approach"

  it should "return f-mapped result of the two parallel computations" in {
    val p = Par3.map2(Par3.unit(2))(Par3.unit(3))(_ + _)
    val e = Par3.lazyUnit(5)

    Await.result(Par3.run(ec)(p), d) shouldBe Await.result(Par3.run(ec)(e), d)
  }

  behavior of "exercise 7.4"

  it should "return the result of asyncF after running Par" in {
    val af = Par3.asyncF((a: Int) => a + 2)

    Await.result(Par3.run(ec)(af(2)), d) shouldBe 4
  }

  behavior of "exercise 7.5"

  it should "return the empty list when the empty list of parallel computations" in {
    val as = Par3.sequence(Nil)

    Await.result(Par3.run(ec)(as), d) shouldBe Nil
  }

  it should "return the computed list when a non empty list of parallel computations" in {
    val as = Par3.sequence(Par3.unit(2) :: Par3.unit(3) :: Nil)

    Await.result(Par3.run(ec)(as), d) shouldBe 2 :: 3 :: Nil
  }

  it should "return the empty list when mapping f with parallel computations" in {
    val p = Par3.parMap(Nil: List[Int])(_ + 1)

    Await.result(Par3.run(ec)(p), d) shouldBe Nil
  }

  it should "return the mapped list when mapping f with parallel computations" in {
    val p = Par3.parMap(2 :: 3 :: Nil)(_ + 1)

    Await.result(Par3.run(ec)(p), d) shouldBe 3 :: 4 :: Nil
  }

  behavior of "exercise 7.6"

  it should "return the list with elements passing the predicate using parallel computations" in {
    val as = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: 10 :: Nil
    val f  = (a: Int) => a % 2 == 0
    val p  = Par3.parFilter(as)(f)

    Await.result(Par3.run(ec)(p), d) shouldBe 2 :: 4 :: 6 :: 8 :: 10 :: Nil
  }

  it should "return f-mapped result of the three parallel computations" in {
    val p = Par3.map3(Par3.unit(2))(Par3.unit(3))(Par3.unit(4))(_ + _ + _)
    val e = Par3.lazyUnit(9)

    Await.result(Par3.run(ec)(p), d) shouldBe Await.result(Par3.run(ec)(e), d)
  }

  it should "return f-mapped result of the four parallel computations" in {
    val p = Par3.map4(Par3.unit(2))(Par3.unit(3))(Par3.unit(4))(Par3.unit(1))(_ + _ + _ + _)
    val e = Par3.lazyUnit(10)

    Await.result(Par3.run(ec)(p), d) shouldBe Await.result(Par3.run(ec)(e), d)
  }

  behavior of "exercise 7.11"

  it should "return the n-th parallel computation when Par of n given" in {
    val pn = Par3.unit(2)
    val ps = Par3.unit(1) :: Par3.unit(2) :: Par3.unit(3) :: Par3.unit(4) :: Nil
    val cN = Par3.choiceN(pn)(ps)

    Await.result(Par3.run(ec)(cN), d) shouldBe Await.result(Par3.run(ec)(Par3.unit(3)), d)
  }

  it should "return the first computation when Par cond evaluates to false" in {
    val cond = Par3.unit(false)
    val pa   = Par3.unit(1)
    val pb   = Par3.unit(2)
    val c    = Par3.choice(cond)(pa)(pb)

    Await.result(Par3.run(ec)(c), d) shouldBe Await.result(Par3.run(ec)(pa), d)
  }

  it should "return the second computation when Par cond evaluates to false" in {
    val cond = Par3.unit(true)
    val pa   = Par3.unit(1)
    val pb   = Par3.unit(2)
    val c    = Par3.choice(cond)(pa)(pb)

    Await.result(Par3.run(ec)(c), d) shouldBe Await.result(Par3.run(ec)(pb), d)
  }

  behavior of "exercise 7.14"

  it should "return the result of the inner computation when nested parallel computation given " in {
    val p                   = Par3.unit(2)
    val pp: Par3[Par3[Int]] = Future(p)(_)

    Await.result(Par3.run(ec)(Par3.join(pp)), d) shouldBe Await.result(Par3.run(ec)(p), d)
  }
}
