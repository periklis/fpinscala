package org.fpinscala.chapters.four

object Exercises {

  sealed trait Option[+A]
  case object None            extends Option[Nothing]
  case class Some[+A](get: A) extends Option[A]

  object Option {
    def map[A, B](ob: Option[A])(f: A => B): Option[B] =
      ob match {
        case None    => None
        case Some(v) => Some(f(v))
      }

    def flatMap[A, B](ob: Option[A])(f: A => Option[B]): Option[B] =
      ob match {
        case None    => None
        case Some(v) => f(v)
      }

    def getOrElse[A, B >: A](ob: Option[A])(default: => B): B =
      ob match {
        case None    => default
        case Some(v) => v
      }

    def orElse[A, B >: A](ob: Option[A])(oe: => Option[B]): Option[B] =
      ob match {
        case None    => oe
        case Some(v) => Some(v)
      }

    def filter[A](ob: Option[A])(f: A => Boolean): Option[A] =
      ob match {
        case None            => None
        case Some(v) if f(v) => Some(v)
        case _               => None
      }

    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
      (a, b) match {
        case (Some(av), Some(bv)) => Some(f(av, bv))
        case (_, _)               => None
      }

    def sequence[A](as: List[Option[A]]): Option[List[A]] =
      as match {
        case Nil            => None
        case None :: _      => None
        case Some(x) :: Nil => Some(List(x))
        case Some(v) :: xs  => flatMap(sequence(xs))(a => Some(v :: a))
      }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    Option.flatMap(mean(xs))(m => mean(xs.map(x => math.pow(x - m, 2))))
}
