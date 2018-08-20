object heapsort {

  @annotation.tailrec
  def heapify(list: Array[Int], loc: Int, lastLeaf: Int) = {
    val l = left(loc)
    val r = right(loc)

    var max = loc

    if (l <= lastLeaf && list(l) > list(max)) max = l
    if (r <= lastLeaf && list(r) > list(max)) max = r

    if (max != loc) {
      swap(list, max, loc)
      heapify(list, max, lastLeaf)
    }
  }

  def left(loc: Int) = {
    2*loc
  }

  def right(loc: Int) = {
    2*loc + 1
  }

  def swap(a: Array[Int], i: Int, j:Int): Unit = {
    val staging = a(i)
    a(i) = a(j)
    a(j) = staging
  }

  def buildHeap(a: Array[Int], m: Int): Unit = {
    for (i <- m/2 to 0 by -1) {
      heapify(a, i, m)
    }
  }

  def heapSort(list: Array[Int]): Unit = {
    var l = list.length - 1
    buildHeap(list, l)
    while (l >= 1) {
      swap(list, 0, l)
      l -= 1
      heapify(list, 0, l)
    }
  }

  heapSort(Array(1, 2, 3))
}