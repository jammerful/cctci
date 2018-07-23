object queue {

  class Queue[T] {
    private case class QueueNode[T](data: T, var next: QueueNode[T] = null)

    private var first: QueueNode[T] = null
    private var last: QueueNode[T] = null

    def add(item: T): Unit = {
      val newLast = QueueNode[T](item)
      if (last != null) last.next = newLast
      last = newLast
      if (first == null) first = newLast
    }

    def remove(): T = {
      if (first == null) throw new Exception("EmptyQueueException")
      val item = first.data
      first = first.next
      if (first == null) last = null
      item
    }

    def peek(): T = {
      if (first == null) throw new Exception("EmptyQueueException")
      first.data
    }

    def isEmpty: Boolean = first == null
  }

  val queue = new Queue[Int]()
  queue.add(1)
  queue.add(2)
  queue.add(3)
  queue.add(4)
  queue.peek()
  queue.remove()
  queue.peek()
  queue.isEmpty
}