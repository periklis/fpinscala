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
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    Option.flatMap(mean(xs))(m => mean(xs.map(x => math.pow(x - m, 2))))

  def sequence[A](as: List[Option[A]]): Option[List[A]] =
    as match {
      case Nil            => None
      case None :: _      => None
      case Some(x) :: Nil => Some(List(x))
      case Some(v) :: xs  => Option.flatMap(sequence(xs))(a => Some(v :: a))
    }

  def traverse[A, B](as: List[Option[A]])(f: A => Option[B]): Option[List[B]] =
    sequence(as map (i => Option.flatMap(i)(f)))

  def traverseSinglePass[A, B](as: List[Option[A]])(f: A => Option[B]): Option[List[B]] =
    as match {
      case Nil            => None
      case None :: _      => None
      case Some(x) :: Nil => Option.map(f(x))(a => List(a))
      case Some(v) :: xs =>
        Option.flatMap(traverseSinglePass(xs)(f))(
          a => Option.flatMap(f(v))(b => Some(b :: a))
        )
    }

  sealed trait Either[+E, +A]
  case class Left[+E](value: E)  extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  object Either {
    def map[E, A, B]: Either[E, A] => (A => B) => Either[E, B] =
      e =>
        f =>
          e match {
            case Left(e)  => Left(e)
            case Right(a) => Right(f(a))
      }

    def flatMap[E, EE >: E, A, B]: Either[E, A] => (A => Either[EE, B]) => Either[EE, B] =
      e =>
        f =>
          e match {
            case Left(e)  => Left(e)
            case Right(a) => f(a)
      }

    def orElse[E, EE >: E, A, B >: A]: Either[E, A] => (=> Either[EE, B]) => Either[EE, B] =
      e =>
        o =>
          e match {
            case Left(_)  => o
            case Right(_) => e
      }

    def map2[E, EE >: E, A, B, C]: Either[E, A] => Either[EE, B] => ((A, B) => C) => Either[EE, C] =
      lhs =>
        rhs =>
          f =>
            (lhs, rhs) match {
              case (Right(a), Right(b)) => Right(f(a, b))
              case (Left(e), _)         => Left(e)
              case (_, Left(e))         => Left(e)

      }

    def sequence[E, A]: List[Either[E, A]] => Either[E, List[A]] =
      _ match {
        case Nil => Right(Nil)
        case x :: xs =>
          x match {
            case Right(v) => flatMap(sequence(xs))(a => Right(v :: a))
            case Left(e)  => Left(e)
          }
      }

    def traverse[E, A, B]: List[A] => (A => Either[E, B]) => Either[E, List[B]] =
      es =>
        f =>
          es match {
            case Nil      => Right(Nil)
            case x :: Nil => map(f(x))(_ :: Nil)
            case x :: xs  => flatMap(f(x))(a => flatMap(traverse(xs)(f))(b => Right(a :: b)))
      }
  }
}
