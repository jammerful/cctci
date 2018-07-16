/* Immutable singly linked list */

object linkedlist {

  // TODO: Covariant type parameter
  sealed trait LinkedList[T] {
    def map[S](f: T => S): LinkedList[S] = {
      this match {
        case Empty => Empty
        case Node(head, tail) => Node(f(head), tail.map(f))
      }
    }

    def insert(item: T): LinkedList[T] = {
      this match {
        case Empty => Node(item, Empty)
        case Node(head, tail) => Node(item, Node(head, tail))
      }
    }

    def remove(item: T): LinkedList[T] = {
      ???
    }

    private def findPrevItem(item: T) = {
      this match {
        case Empty =>
      }
    }
  }

  case class Node[+T](head: T, tail: LinkedList[T]) extends LinkedList[T]

  case object Empty extends LinkedList[Nothing]

  object LinkedList {
    def apply[T](items: T*): LinkedList[T] = {
      if (items.isEmpty) {
        return Empty
      }
      else {
        Node(items.head, apply(items.tail: _*))
      }
    }
  }

  val ll = LinkedList[Int](1, 2, 3, 4)
  ll.map(x=> x + 1)
}