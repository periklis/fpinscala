package org.fpinscala.chapters.seven

object Exercises {

  case class Par[A](run: () => A)

  object Par {
    def unit[A]: (=> A) => Par[A] =
      a => Par(() => a)

    def get[A]: Par[A] => A =
      _.run()

    def map2[A, B, C]: Par[A] => Par[B] => ((A, B) => C) => Par[C] =
      pa => pb => f => unit(f(get(pa), get(pb)))
  }

}
