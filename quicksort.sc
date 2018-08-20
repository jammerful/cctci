object quicksort {

  def quickSort(list: List[Int]): List[Int] = {
    if (list.length <= 1 ) list
    else {
      val pivot = list(list.length / 2)
      List.concat(
        quickSort(list.filter(x => x < pivot)),
        list.filter(x => x == pivot),
        quickSort(list.filter(x => x > pivot))
      )
    }
  }

  quickSort(List(1))
  quickSort(List(1, 5, 3, 11, 87, 4 , 2, 3, 101))
  quickSort(List(1, 1, 1, 1))
}