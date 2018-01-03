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

    def fork[A](p: => Par3[A]): Par3[A] =
      ec => p(ec)

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

    def map3[A, B, C, D]: Par3[A] => Par3[B] => Par3[C] => ((A, B, C) => D) => Par3[D] =
      pa => pb => pc => f => map2(map2(pa)(pb)((f.curried)(_)(_)))(pc)((g, c) => g(c))

    def map4[A, B, C, D, E]: Par3[A] => Par3[B] => Par3[C] => Par3[D] => ((A, B, C, D) => E) => Par3[E] =
      pa =>
        pb =>
          pc =>
            pd =>
              f => {
                val ab = map2(pa)(pb)((f.curried)(_)(_))
                val c  = map2(ab)(pc)((g, c) => g(c))
                map2(c)(pd)((h, d) => h(d))
      }

    def asyncF[A, B]: (A => B) => A => Par3[B] =
      f => a => lazyUnit(f(a))

    def sequence[A]: List[Par3[A]] => Par3[List[A]] =
      _ match {
        case Nil       => unit(Nil)
        case p :: pars => flatMap(sequence(pars))(as => map(p)(_ :: as))
      }

    def parMap[A, B]: List[A] => (A => B) => Par3[List[B]] =
      as => f => fork(sequence(as.map(asyncF(f))))

    def parFilter[A]: List[A] => (A => Boolean) => Par3[List[A]] =
      as =>
        f =>
          fork {
            val ps = as.map(asyncF((a: A) => List(a).filter(f)))
            ps.foldRight(unit(Nil: List[A]))(map2(_)(_)(_ ::: _))
      }

    def choiceN[A]: Par3[Int] => List[Par3[A]] => Par3[A] =
      pn => ps => flatMap(pn)(n => ps(n))

    def choice[A]: Par3[Boolean] => Par3[A] => Par3[A] => Par3[A] =
      c => pa => pb => choiceN(map(c)(r => if (!r) 0 else 1))(pa :: pb :: Nil)

    def join[A]: Par3[Par3[A]] => Par3[A] =
      p => ec => p(ec).flatMap(v => v(ec))(ec)
  }

  /* Exercise 7.7

     map(map(y)(g))(f) == map(y)(f compose g)

     because the laws
     - map(x)(id) == x
     - (f compose id)(x) == f(id(x)) == f(x)
     hold, assuming g = id the above becomes:

     map(map(y)(id))(f) == map(y)(f compose id)
     map(y)(f) == map(y)(f)
   */

  /* Exercise 7.8 & 7.9

     According to the implementation in the book, that uses
     java.util.concurrent.Executors, in listing 7.5, the
     call on Future.get inside callable is blocking the outer
     Callable from proceeding further. Thus, in the case of a
     FixedThreadPool of size 1 or SingleThreadExecutor the
     execution of Callable will die of starvation.

 */
}
