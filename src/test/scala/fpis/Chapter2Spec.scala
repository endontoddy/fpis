package fpis

import org.scalatest.{FlatSpec, Matchers}
import fpis.Chapter2.{fib, isSorted}

class Chapter2Spec extends FlatSpec with Matchers {

  "fib" should "return the correct result" in {
    fib(6) should be (5)
  }

  "isSorted" should "report an array sorted as required" in {
    isSorted[Int](Array(1, 2, 3, 5, 7, 9), (a,b) => a <= b ) should be (true)
  }

  "isSorted" should "report an array not sorted as required" in {
    isSorted[Int](Array(1, 2, 3, 7, 6, 9), (a,b) => a <= b ) should be (false)
  }

}
