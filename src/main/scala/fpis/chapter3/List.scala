package fpis.chapter3

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  // Exercise 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("No tail for an empty list")
    case Cons(_, t) => t
  }

  // Exercise 3.3
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("No head for an empty list")
    case Cons(_, t) => Cons(h, t)
  }

  // Exercise 3.4
  @tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if(n == 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }

  // Exercise 3.5
  @tailrec
  def dropWhile[A](l: List[A])( f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t)(f)
    case _ => l
  }

  // Exercise 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("No init for an empty list")
    case Cons(_, Nil) => l
    case Cons(h, t)   => Cons(h, init(t))
  }

  def foldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(x, z))(f)
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)(f)

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, y) => y + 1)

  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)

  def length2[A](as: List[A]): Int =
    foldLeft(as, 0)((_, y) => y + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil:List[A])((x, a) => Cons(x, a))

  // Exercise 3.14
  def append[A](l: List[A], x: List[A]): List[A] =
    foldRight(l, x)(Cons(_,_))

  // Exercise 3.15
  def concat[A](ll: List[List[A]]): List[A] =
    foldRight(ll, Nil: List[A])(append)

  // Exercise 3.16
  def addOne(l: List[Int]): List[Int] =
    map(l)(_ + 1)

  // Exercise 3.17
  def toString(l: List[Double]): List[String] =
    map(l)(_.toString)

  // Exercise 3.18
  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil:List[B])((x, a) => Cons(f(x), a))

  // Exercise 3.19
//  def filter[A](as: List[A])(f: A => Boolean): List[A] =
//    foldRight(as, Nil:List[A])((x, a) => if(f(x)) Cons(x, a) else a
//    )

  def filterOdd(l: List[Int]): List[Int] =
    filter(l)(_ % 2 == 0)

  // Exercise 3.20
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  // Exercise 3.21
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(x => if(f(x)) List(x) else Nil)

  // Exercise 3.22
  def addLists(a: List[Int], b: List[Int]): List[Int] =
    zipWith(a, b)(_ + _)

  // Exercise 3.23
  def zipWith[A,B,C](a: List[A], b: List[B])(f: (A, B) => C): List[C] =
    (a,b) match {
      case (Cons(h,t), Cons(h2, t2)) => Cons(f(h, h2), zipWith(t, t2)(f))
      case _ => Nil
    }

  // Exercise 3.24
  def hasSubsequence[A](sup: scala.collection.immutable.List[A], sub: scala.collection.immutable.List[A]): Boolean =
    if(sup.length >= sub.length)
      if(sup.take(sub.length) == sub) true
      else hasSubsequence(sup.tail, sub)
    else false

  def apply[A](as: A*): List[A] =
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}
