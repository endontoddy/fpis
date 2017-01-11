package fpis.chapter5

sealed trait Stream[+A] {
  import Stream._

  def toList: List[A] = this match {
    case Empty      => Nil
    case Cons(h, t) => h() :: t().toList
  }

//  def take(n: Int): Stream[A] = this match {
//    case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
//    case _ => empty[A]
//  }

  def take(n: Int): Stream[A] =
    unfold(n, this) {
      case (x, Cons(h, t)) if x > 0 => Some((h(), (n - 1, t())))
      case _ => None
    }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

//  def takeWhile(f: A => Boolean): Stream[A] = this match {
//    case Cons(h, t) if f(h()) => cons(h(), t().takeWhile(f))
//    case _ => empty[A]
//  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

//  def takeWhile(f: A => Boolean): Stream[A] =
//    foldRight(empty[A])((a, b) => if(f(a)) cons(a, b) else empty[A])

  def takeWhile(f: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if f(h()) => Some(h(), t())
      case _ => None
    }

  def headOption: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

//  def map[B](f: A => B): Stream[B] =
//    foldRight(empty[B])((h, t) => cons(f(h), t))

  def map[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _          => None
    }

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if(f(h)) cons(h, t) else t)

  def append[B>:A](a: => Stream[B]): Stream[B] =
    foldRight(a)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h).append(t) )

  def zipWith[B,C](b: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, b)) {
      case (Cons(h, t), Cons(h2, t2)) => Some(f(h(), h2()), (t(), t2()))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold(this, s2) {
      case (Cons(h, t), Cons(h2, t2)) => Some((Some(h()), Some(h2())), (t(), t2()))
      case (Cons(h, t), e)            => Some((Some(h()), None), (t(), e))
      case (e, Cons(h, t))            => Some((None, Some(h())), (e, t()))
      case _                          => None
    }

  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile(_._2.isDefined).forAll {
      case (a, b) => a == b
    }

  def tails: Stream[Stream[A]] = this match {
    case Empty      => empty
    case c: Cons[A] => Stream(c) append c.t().tails
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if(as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

//  def constant[A](a: A): Stream[A] = {
//    val x = cons(a, x)
//    x
//  }
//
//  def from(n: Int): Stream[Int] =
//    cons(n, from(n+1))
//
//  def fibs(x:Int = 0, y: Int = 1): Stream[Int] =
//    cons(x, fibs(y, x + y))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None         => empty
    }

  def fibs(): Stream[Int] =
    unfold((0,1))(a => Some(a._1, (a._2, a._1 + a._2)))

  def from(n: Int): Stream[Int] =
    unfold(n)(m => Some(m, m + 1))

  def constant(x: Int): Stream[Int] =
    unfold(x)(y => Some(y, y))

  def ones: Stream[Int] =
    constant(1)

}
