package fpis.chapter4

sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] = flatMap(x => Right(f(x)))

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(l)  => Left(l)
    case Right(r) => f(r)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case Right(_) => this
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    flatMap(x => b map (y => f(x,y)))
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as match {
      case Nil    => Right(Nil)
      case x :: y => for {
        z <- f(x)
        b <- traverse(y)(f) map ( z :: _)
      } yield b
    }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(x => x)
}
