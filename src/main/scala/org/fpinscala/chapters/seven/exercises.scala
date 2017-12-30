package org.fpinscala.chapters.seven

import scala.concurrent._
import scala.concurrent.duration._

object Exercises {

  case class Par[A](run: () => A)

  object Par {
    def unit[A]: A => Par[A] =
      a => Par(() => a)

    def fork[A]: (=> Par[A]) => Par[A] =
      p => p

    def lazyUnit[A]: A => Par[A] =
      a => fork(unit(a))

    def run[A]: Par[A] => A =
      _.run()

    def map2[A, B, C]: Par[A] => Par[B] => ((A, B) => C) => Par[C] =
      pa => pb => f => lazyUnit(f(run(pa), run(pb)))
  }

  /* Exercise 7.2 - Bad approach
     Matching Par2[A] representation directly to Future[A] using scala's concurrent API
     fails to decouble computation from description, because the implicits force Future[A] to
     run on parameter evaluation order and not when calling run on Par2[A] (e.g. map2 in tests)

     Thus, Par[A] has to be a function of ExecutionContext to Future[A] to have the benefits:
     - Provide lightweight computation description by using and lazyUnit and eval only on run()
     - Make computation startpoint and parallel execution strategy explicit in our API
   */
  type Par2[A] = Future[A]

  object Par2 {
    def unit[A](a: A)(implicit ec: ExecutionContext): Par2[A] =
      Future(a)

    def fork[A](p: => Par2[A]): Par2[A] = p

    def lazyUnit[A](a: A)(implicit ec: ExecutionContext): Par2[A] =
      fork(unit(a))

    def run[A]: Par2[A] => Duration => A =
      f => d => Await.result(f, d)

    def map2[A, B, C](pa: Par2[A], pb: Par2[B])(f: (A, B) => C)(implicit ec: ExecutionContext): Par2[C] =
      pa.flatMap(a => pb.map(b => f(a, b)))
  }

  // Exercise 7.2 - Good Approach
  type Par3[A] = ExecutionContext => Future[A]

  object Par3 {
    def unit[A]: A => Par3[A] =
      a => Future(a)(_)

    def fork[A](p: => Par3[A]): Par3[A] = p

    def lazyUnit[A]: A => Par3[A] =
      a => fork(unit(a))

    def run[A]: ExecutionContext => Par3[A] => Future[A] =
      ec => p => p(ec)

    def flatMap[A, B]: Par3[A] => (A => Par3[B]) => Par3[B] =
      pa => f => ec => pa(ec).flatMap(v => f(v)(ec))(ec)

    def map[A, B]: Par3[A] => (A => B) => Par3[B] =
      pa => f => flatMap(pa)(a => unit(f(a)))

    def map2[A, B, C]: Par3[A] => Par3[B] => ((A, B) => C) => Par3[C] =
      pa => pb => f => flatMap(pa)(a => map(pb)(b => f(a, b)))
  }

}
