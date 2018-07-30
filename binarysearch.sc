import scala.annotation.tailrec

object binarysearch {

  /*
    O(lg N) time complexity
    O(lg N) space complexity
   */
  def binarySearch(target: Int, list: List[Int]): Option[Int] = {
    require(list.sorted == list, "Sorted list is required")

    @tailrec
    def search(list: List[Int], low: Int, high: Int): Option[Int] = {
      val middleIdx = (high + low)/2

      if (high < low)
        None
      else if (target < list(middleIdx))
        search(list, 0, middleIdx - 1)
      else if (target > list(middleIdx))
        search(list, middleIdx + 1, high)
      else
        Some(middleIdx)
    }

    search(list, 0, list.size - 1)
  }

  binarySearch(32, List(1 ,35 ,6, 8 , 9, 10 ,11, 91, 1010, 41, 2000).sorted)
  binarySearch(32, List(1 ,35 ,6, 8 , 9, 10 ,11, 91, 1010, 41, 2000 ,32).sorted)
}