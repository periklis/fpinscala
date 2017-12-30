package org.fpinscala.chapters.seven

import scala.concurrent._
import scala.concurrent.duration._

import org.fpinscala.chapters.seven.Exercises._
import org.scalatest.{FlatSpec, Matchers}

class ExercisesSpec extends FlatSpec with Matchers {

  behavior of "exercise 7.1"

  it should "return f-mapped result of the two parallel computations" in {
    Par.run(Par.map2(Par.unit(2))(Par.unit(3))(_ + _)) shouldBe Par.run(Par.unit(5))
  }

  behavior of "exercise 7.2 - bad approach"

  it should "return f-mapped result of the two parallel computations" in {
    import ExecutionContext.Implicits.global
    val d = 10.seconds

    val p1 = Par2.map2(Par2.unit(2), Par2.unit(3))(_ + _)
    val p2 = Par2.lazyUnit(5)

    Par2.run(p1)(d) shouldBe Par2.run(p2)(d)
  }

  behavior of "exercise 7.2 - good approach"

  it should "return f-mapped result of the two parallel computations" in {
    val ec: ExecutionContext = ExecutionContext.Implicits.global

    val d = 10.seconds
    val p = Par3.map2(Par3.unit(2))(Par3.unit(3))(_ + _)
    val e = Par3.lazyUnit(5)

    Await.result(Par3.run(ec)(p), d) shouldBe Await.result(Par3.run(ec)(e), d)
  }
}
