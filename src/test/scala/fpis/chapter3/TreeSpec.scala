package fpis.chapter3

import org.scalatest.{FlatSpec, Matchers}

class TreeSpec extends FlatSpec with Matchers {

  val myTree = Branch(
    left = Branch(
      left = Leaf(1),
      right = Leaf(2)
    ),
    right = Leaf(3)
  )

  "size" should "report the number of nodes in the tree" in {
    Tree.size(myTree) should be (5)
  }

  "maximum" should "report the highest value from a Tree[Int]" in {
    Tree.maximum(myTree) should be (3)
  }

  "depth" should "report the longest path length from the root node to a leaf" in {
    Tree.depth(myTree) should be (3)
  }

  "map" should "use a function to convert each Leaf in a Tree" in {
    Tree.map(myTree)(_ + 1) should be (Branch(
      left = Branch(
        left = Leaf(2),
        right = Leaf(3)
      ),
      right = Leaf(4)
    ))
  }
}
