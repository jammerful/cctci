import scala.util.control.Breaks.break


object chapter2 {
  case class Node[T](var value: T, var next: Option[Node[T]] = None)

  object Node {
    def size[T](node: Option[Node[T]], count: Int = 0): Int =
      if (node.isDefined) {
        size(node.get.next, count + 1)
      } else {
        count
      }
  }

  case class LinkedList[T](var head: Option[Node[T]] = None) {

    def add(value: T) = {
      val newHead = new Node(value)
      newHead.next = head
      head = Some(newHead)
      this
    }

    def isEmpty = head.isDefined

    def size = Node.size(head)
  }


  /* Question 1 */
  def removeDuplicates[T](ll: LinkedList[T]): LinkedList[T] = {
    var set = Set[T]()
    var currNode = ll.head
    if (ll.isEmpty) ll
    else {
      set += currNode.get.value
      while (currNode.get.next.isDefined) {
        val n = currNode.get.next.get

        if (set.contains(n.value)) {
          currNode.get.next = n.next
        } else {
          set = set + n.value
        }
        currNode = currNode.get.next
      }
      ll
    }
  }

  var ll = LinkedList()
  removeDuplicates(ll)
  print(ll)

  /* Question 3 */
  def removeMiddleNode[T](node: Node[T]): Unit = {
    if (node.next.isDefined) {
      node.value = node.next.get.value
      node.next = node.next.get.next
    }
  }

  /* Question 8 */
  def detectLoop[T](ll: LinkedList[T]): Option[Node[T]] = {
    var slow = ll.head
    var fast = ll.head

    while (fast.get.next.isDefined && slow.get.next.isDefined) {
      slow = slow.get.next
      fast = fast.get.next.get.next

      if (slow == fast) {
        break
      }
    }

    if (fast == None || fast.get.next == None) {
      return None
    }

    slow = ll.head
    while (slow != fast) {
      slow = slow.get.next
      fast = fast.get.next
    }

    fast
  }
}