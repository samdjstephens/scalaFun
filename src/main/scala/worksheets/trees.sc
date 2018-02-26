import java.security.KeyStore.TrustedCertificateEntry

sealed abstract class Tree[+T]
case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
}
case object End extends Tree[Nothing] {
  override def toString = "."
}
object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}

val tree = Node(1.5, Node(1.0), Node(2.0))

object Tree {
  def cBalanced[T](numNodes: Int, value: T): List[Tree[T]] = {
    if (numNodes == 1) List(Node(value))
    else if (numNodes == 0) List(End)
    else if (numNodes % 2 == 0) {
      for {
        largerSubTree <- Tree.cBalanced(numNodes / 2, value)
        smallerSubTree <- Tree.cBalanced((numNodes / 2) - 1, value)
        largerFirst <- List(true, false)
      } yield {
        if (largerFirst) Node(value, largerSubTree, smallerSubTree)
        else Node(value, smallerSubTree, largerSubTree)
      }
    }
    else {
      for {
        subTree <- Tree.cBalanced((numNodes - 1) / 2, value)
      } yield Node(value, subTree, subTree)
    }
  }

  def countNodes(tree: Tree[Any]): Int = tree match {
    case End => 0
    case Node(_, left, right) => countNodes(left) + countNodes(right)
  }

  def isBalanced(tree: Tree[Any]): Boolean = tree match {
    case End => true
    case Node(_, left, right) => Math.abs(countNodes(left) - countNodes(right)) == 0 && isBalanced(left) && isBalanced(right)
  }
}

val solutions = for (sol <- Tree.cBalanced(50, 'x')) yield sol
solutions.forall(tree => Tree.isBalanced(tree))
solutions.distinct.length == solutions.length