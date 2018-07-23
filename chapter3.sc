import scala.collection.mutable
import scala.collection.mutable.{Queue, Stack}

object chapter3 {

  /* Question 1 */
  class FixedMulitStack(stackCapacity: Int) {
    private val numberOfStacks = 3
    private var values = List[Int](stackCapacity * numberOfStacks)
    private var sizes = List[Int](stackCapacity * numberOfStacks)

    // ...
  }

  /* Question 2 */
  class StackWithMin[T <% Ordered[T]] {

    // min stores the minimum of the stack up to including that node
    private case class NodeWithMin[T](data: T, min: T, next: NodeWithMin[T] = null)

    private var top: NodeWithMin[T] = null

    def push(item: T): Unit = {
      val newMin: T = if (top != null && top.min < item) top.min else item
      top = NodeWithMin[T](item, newMin, top)
    }

    def pop(): T = {
      if (top == null) throw new Exception("EmptyStackException")
      val item = top.data
      top = top.next
      item
    }

    def min(): T = {
      if (top == null) throw new Exception("EmptyStackException")
      top.min
    }
  }

  val stackWithMin = new StackWithMin[Int]()
  stackWithMin.push(1)
  stackWithMin.push(2)
  stackWithMin.push(3)
  stackWithMin.min()
  stackWithMin.push(0)
  stackWithMin.min()
  stackWithMin.pop()
  stackWithMin.min()

  /* Question 4 */
  class MyQueue[T] {
    private case class QueueNode(data: T, var next: QueueNode = null)

    private val stackNewest = new Stack[T]()
    private val stackOldest = new Stack[T]()

    def add(item: T): Unit = {
      stackNewest.push(item)
    }

    private def shiftStacks(): Unit = {
      if (stackOldest.isEmpty) {
        while (!stackNewest.isEmpty) {
          stackOldest.push(stackNewest.pop())
        }
      }
    }

    def remove(): T = {
      shiftStacks()
      stackOldest.pop()
    }
  }

  /* Question 5 */
  def sortStack[T <% Ordered[T]](stack: Stack[T]): Unit = {
    val sortedStack = new Stack[T]()
    while (!stack.isEmpty) {
      val tmp = stack.pop()
      while (!sortedStack.isEmpty && sortedStack.top > tmp) {
        stack.push(sortedStack.pop())
      }
      sortedStack.push(tmp)
    }

    while (!sortedStack.isEmpty) {
      stack.push(sortedStack.pop())
    }
  }

  val testStack = new Stack[Int]()
  testStack.push(3)
  testStack.push(1)
  testStack.push(2)
  testStack.push(4)
  sortStack(testStack)
  testStack.top
}