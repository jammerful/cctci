import scala.math

object binarytree {

  trait Tree[+T]

  case class BinaryTree[+T <% Ordered[T]](left: Tree[T],
                                          value: T,
                                          right: Tree[T]) extends Tree[T]
  case class Leaf[+T <% Ordered[T]](value: T) extends Tree[T]
  case object Empty extends Tree[Nothing]

  object BinaryTree {
    def isBalanced[T](root: Tree[T]): Boolean = {
      def balanced(root: Tree[T]): Int = root match {
        case BinaryTree(left, _, right) => {
          val l = balanced(left)
          val r = balanced(right)
          if (l < 0 || r < 0 || math.abs(l - r) > 1) -1 else math.max(l, r) + 1
        }
        case leaf: Leaf[T] => 1
        case Empty => 0
      }

      balanced(root) != -1
    }

    def inOrderTraversal[T, S](root: Tree[T], action: T => S): Unit = root match {
      case Empty =>
      case Leaf(v) => action(v)
      case BinaryTree(l, v, r) => {
        inOrderTraversal(l, action)
        action(v)
        inOrderTraversal(r, action)
      }
    }

    def preOrderTraversal[T, S](root: Tree[T], action: T => S): Unit = root match {
      case Empty =>
      case Leaf(v) => action(v)
      case BinaryTree(l, v, r) => {
        action(v)
        preOrderTraversal(l, action)
        preOrderTraversal(r, action)
      }
    }

    def postOrderTraversal[T, S](root: Tree[T], action: T => S): Unit = root match {
      case Empty =>
      case Leaf(v) => action(v)
      case BinaryTree(l, v, r) => {
        postOrderTraversal(l, action)
        postOrderTraversal(r, action)
        action(v)
      }
    }
  }

  assert(BinaryTree.isBalanced(BinaryTree(Leaf(2), 1, Leaf(3))) == true)
  val testTree = BinaryTree(Leaf(2), 1, BinaryTree(Empty, 3, BinaryTree(Empty, 4, Leaf(5))))
  assert(BinaryTree.isBalanced(testTree) == false)
  BinaryTree.inOrderTraversal(testTree, println)
  BinaryTree.preOrderTraversal(testTree, println)
  BinaryTree.postOrderTraversal(testTree, println)
}