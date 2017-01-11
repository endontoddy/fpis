package fpis.chapter4

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = flatMap(x => Some(f(x)))

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(x)  => f(x)
    case None     => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(x)  => x
    case None     => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None     => ob
    case _        => this
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(x) if f(x)  => this
    case _                => None
  }

}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =
    a flatMap {x => b map {y => f(x,y)} }

//  def sequence[A](a: List[Option[A]]): Option[List[A]] =
//    a match {
//      case Some(x) :: Nil => Some(x :: Nil)
//      case Some(x) :: y   => sequence(y).map( x :: _ )
//      case _              => None
//    }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil    => Some(Nil)
      case x :: y => f(x) match {
        case Some(z)  => traverse(y)(f) map ( z :: _)
        case None     => None
      }
    }

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)



}