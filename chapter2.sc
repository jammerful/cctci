import java.util.LinkedList

import scala.collection.immutable.HashSet

object chapter2 {
  case class Node(val d: Int, var next: Node = null)

  def createNodeList(arr: Array[Int]) = if(arr.length > 0){
    var head = Node(arr(0), null)
    val ret = head
    for(i<-1 until arr.length){
      head.next = Node(arr(i))
      head = head.next
    }
    ret
  }
  else null

  def printNodeList(src: Node) = {
    var head = src;
    while(head != null){
      print(head.d + "  ")
      head = head.next
    }
    println("!")
  }


  /* Question 1 */
  def removeDuplicates(n: Node): Unit = {
    var set = new HashSet[Int]()
    var currNode: Node = n
    var prev: Node = null
    while (currNode != null) {
      if (set.contains(currNode.d)) {
        prev.next = n.next
      }
      else {
        set += currNode.d
        prev = currNode
      }
      currNode = currNode.next
    }
  }

  var ll = Node(1, Node(1, null))
  removeDuplicates(ll)
  print(ll)
}