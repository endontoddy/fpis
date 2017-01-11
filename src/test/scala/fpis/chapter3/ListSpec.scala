package fpis.chapter3

import org.scalatest.{FlatSpec, Matchers}

class ListSpec extends FlatSpec with Matchers {

  "length" should "return the length of a list" in {
    List.length(List(1,2,3,5)) should be (4)
  }

  "reverse" should "return a list with the order of elements reversed" in {
    List.reverse(List(1,2,3,4)) should be (List(4,3,2,1))
  }

  "append" should "append a value to the end of a list" in {
    List.append(List(1,2,3), List(4)) should be (List(1,2,3,4))
  }

  "concat" should "flatten a list of lists in to a single list" in {
    List.concat(List(List(1,2), List(3,4), List(5,6))) should be (List(1,2,3,4,5,6))
  }

  "addOne" should "add 1 to every element of a List[Int]" in {
    List.addOne(List(2,4,8,9)) should be (List(3,5,9,10))
  }

  "toString" should "convert each element of a List[Double] to a string" in {
    List.toString(List(1.5, 2.3, 7.8)) should be (List("1.5", "2.3", "7.8"))
  }

  "filterOdd" should "Return a list with the odd numbers removed" in {
    List.filterOdd(List(1,2,4,5,7,9,10)) should be (List(2,4,10))
  }

  "flatMap" should "work properly!" in {
    List.flatMap(List(1,2,3))(i => List(i,i)) should be (List(1,1,2,2,3,3))
  }

  "addLists" should "add the corresponding elements of two lists" in {
    List.addLists(List(1,2,3), List(4,5,6)) should be (List(5,7,9))
  }

  "hasSubsequence" should "find a subsequence (length 2) correctly" in {
    List.hasSubsequence(scala.collection.immutable.List(1,2,3,4), scala.collection.immutable.List(2,3)) should be (true)
  }

  it should "find a subsequence (length 1) correctly" in {
    List.hasSubsequence(scala.collection.immutable.List(1,2,3,4), scala.collection.immutable.List(4)) should be (true)
  }

  it should "identify a non-existent subsquence" in {
    List.hasSubsequence(scala.collection.immutable.List(1,2,3,4), scala.collection.immutable.List(1,3)) should be (false)
  }

}
