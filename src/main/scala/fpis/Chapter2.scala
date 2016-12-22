package fpis

import scala.annotation.tailrec

object Chapter2 {

  // Exercise 1
  def fib(n: Int): Int = {
    @tailrec
    def loop(x: Int = 0, y: Int = 1, c: Int = 2): Int =
      if (c == n) y
      else loop(y, x + y,  c + 1)

    loop()
  }

  // Exercise 2
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @tailrec
    def loop(n: Int = 0): Boolean =
      if (n == as.length - 2)
        true
      else if (ordered(as(n), as(n + 1)))
        loop(n + 1)
      else
        false

    loop()
  }

  // Exercise 3
  def curry[A,B,C](f: (A,B) => C): A => (B => C) =
    a => b => f(a,b)

  // Exercise 4
  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a,b) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    a => f(g(a))
}
