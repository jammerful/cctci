object tree {

  sealed trait TreeInterface[+T]

  /* Basic Tree */
  case class GenericNode[+T](value: T, var children: List[GenericNode] = List())

  class Tree[+T] extends TreeInterface[T] {
    val root: GenericNode[T] = null
  }

}