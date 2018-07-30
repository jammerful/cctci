object insertionsort {

  /*
      O(n^2) time
      O(n)   space
   */
  def insertionSort(list: List[Int]): List[Int] = {
    def insert(x: Int, list: List[Int]): List[Int] = list match {
      case Nil => List(x)
      case head :: tail => {
        if (x < head)
          x :: insert(head, tail)
        else
          head :: insert(x, tail)
      }
    }

    list match {
      case Nil => Nil
      case head :: tail => insert(head, insertionSort(tail))
    }
  }

  insertionSort(List())
  insertionSort(List(2, 4, 5, 8, 9 , 1, 11, 3))
}