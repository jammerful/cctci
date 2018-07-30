object mergesort {

  def mergeSort(list: List[Int]): List[Int] = {
    def merge(left: List[Int], right: List[Int]): List[Int] = (left, right) match {
      case (Nil, Nil) => Nil
      case (l, Nil) => l
      case (Nil, r) => r
      case (leftHead :: leftTail, rightHead :: rightTail) => {
        if (leftHead < rightHead)
          leftHead :: merge(leftTail, right)
        else
          rightHead :: merge(left, rightTail)
      }
    }

    val middle = list.size/2
    if (middle == 0)
      list
    else {
      val left = list.take(middle)
      val right = list.takeRight(list.size - middle)
      merge(mergeSort(left), mergeSort(right))
    }
  }

  mergeSort(List())
  mergeSort(List(1))
  mergeSort(List(1, 5, 6, 7, 11, 2, 9, 69, 37))
}
