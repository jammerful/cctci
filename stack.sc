object stack {

  class Stack[T] {
    private case class StackNode[T](data: T, var next: StackNode[T])

    private var top: StackNode[T] = null

    def pop(): T = {
      if (top == null) throw new Exception("EmptyStackException")
      val item = top.data
      top = top.next
      item
    }

    def push(item: T): Unit = {
      val newTop = StackNode(item, top)
      top = newTop
    }

    def peek(): T = {
      if (top == null) throw new Exception("EmptyStackException")
      top.data
    }

    def isEmpty: Boolean = top == null
  }

  val stack = new Stack[Int]()
  stack.push(1)
  stack.push(2)
  stack.push(3)
  stack.push(4)
  stack.peek()
  stack.isEmpty
  stack.pop()
  stack.peek()
}