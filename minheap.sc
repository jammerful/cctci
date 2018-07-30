object minheap {

  // trait can't have view or context bounds due to being syntactic sugar for implicits
  abstract class Heap[+T <% Ordered[T]] {
    def min: T
    def left: Heap[T]
    def right: Heap[T]
    def isEmpty: Boolean

    val size: Int
    val height: Int

    def insert[S >: T : Ordering](x: S): Heap[S] = {
      if (isEmpty) Branch(x, Leaf, Leaf)
      else if (left.size < math.pow(2, left.height) - 1)
        Heap.bubbleUp(min, left.insert(x), right)
      else if (right.size < math.pow(2, right.height) - 1)
        Heap.bubbleUp(min, left, right.insert(x))
      else if (right.height < left.height)
        Heap.bubbleUp(min, left, right.insert(x))
      else
        Heap.bubbleUp(min, left.insert(x), right)
    }

    def remove() = Heap[T] = {
      ???
    }
  }

  object Heap {

    private[Heap] def bubbleUp[T <% Ordered[T]](x: T, left: Heap[T], right: Heap[T]): Heap[T] ={
      (left, right) match {
        case (Branch(y, lt, rt), _) if x > y => Branch(y, Branch(x, lt, rt), right)
        case (_, Branch(z, lt, rt)) if x > z => Branch(z, left, Branch(x, lt, rt))
        case (_, _) => Branch(x, left , right)
      }
    }

  }

  case object Leaf extends Heap[Nothing] {
    def min = throw new Exception("NoSuchElementException")
    def left = throw new Exception("NoSuchElementException")
    def right = throw new Exception("NoSuchElementException")
    def isEmpty = true
    val size = 0
    val height = 0
  }

  case class Branch[+T](min: T, left: Heap[T], right: Heap[T]) extends Heap[T] {
    def isEmpty: Boolean = false
    val size: Int = left.size + right.size + 1
    val height: Int = math.max(left.height, right.height) + 1
  }

}