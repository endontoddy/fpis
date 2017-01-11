package fpis.chapter3

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // Exercise 3.25
//  def size[A](t: Tree[A]): Int = t match {
//   case Leaf(_) => 1
//   case Branch(l, r) => 1 + size(l) + size(r)
//  }

  // Exercise 3.26
//  def maximum(t: Tree[Int]): Int = t match {
//    case Leaf(x) => x
//    case Branch(l, r) => maximum(l) max maximum(r)
//  }

  // Exercise 2.37
//  def depth[A](t: Tree[A]): Int = t match {
//    case Leaf(_) => 1
//    case Branch(l, r) => 1 + depth(l) max depth(r)
//  }

  // Exercise 3.28
//  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
//    case Leaf(x) => Leaf(f(x))
//    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
//  }

  // exercise 3.29
  def fold[A,B](t: Tree[A])(f: A => B)(g: (B, B) => B): B =
    t match {
      case Leaf(x)        => f(x)
      case Branch(l, r)   => g(fold(l)(f)(g), fold(r)(f)(g))
    }

  def size[A](t: Tree[A]): Int =
    fold(t)(_ => 1)(_ + _ + 1)

  def maximum(t: Tree[Int]): Int =
    fold(t)(x => x)(_ max _)

  def depth[A](t: Tree[A]): Int =
    fold(t)(_ => 1)((l, r) => 1 + ( l max r))

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] =
    fold[A, Tree[B]](t)(x => Leaf(f(x)))(Branch(_, _))
}
