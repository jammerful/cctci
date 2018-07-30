

object chapter4 {

  type Graph = Map[Int, Set[Int]]

  def DFS(start: Int, graph: Graph): List[Int] = {

    def DFS0(v: Int, visited: List[Int]): List[Int] = {
      if (visited.contains(v))
        visited
      else {
        val unvisitedNeighbors = graph(v).filterNot(visited.contains)
        unvisitedNeighbors.foldLeft(v :: visited)((b,a) => DFS0(a, b))
      }
    }

    DFS0(start, List()).reverse
  }

  trait Tree[+T]

  case class BinaryTree[+T <% Ordered[T]](left: Tree[T],
                                          value: T,
                                          right: Tree[T]) extends Tree[T]
  case class Leaf[+T <% Ordered[T]](value: T) extends Tree[T]
  case object Empty extends Tree[Nothing]

  /* Question 1 */
  def isConnected(x: Int, y: Int, graph: Graph): Boolean = {
    val connectedComponent = DFS(x, graph)
    connectedComponent.contains(y)
  }

  /*
      1 -> 2 -> 3
      |
      \/
      4 -> 5
   */
  val graph1: Graph = Map(1 -> Set(2, 4), 2 -> Set(3), 3 -> Set[Int](), 4 -> Set(5), 5 -> Set[Int]())
  isConnected(1, 5, graph1)
  isConnected(3, 5, graph1)


  /* Question 2 */
  def listToBST(list: List[Int]): Tree[Int] = {
    def insert(i: Int, tree: Tree[Int]): Tree[Int] = tree match {
        case Empty => Leaf(i)
        case Leaf(a) if a <= i => BinaryTree(Leaf(a), i, Empty)
        case Leaf(b) if b > i => BinaryTree(Leaf(i), b, Empty)
        case BinaryTree(left, x, right) => {
          if (x <= i) BinaryTree(insert(x, left), i, right)
          else if (x > i) BinaryTree(left, x, insert(i, right))
          else throw new Exception("Unknown error!")
        }
      }

    list match {
      case Nil => Empty
      case head :: tail => insert(head, listToBST(tail))
    }
  }

  listToBST(List(4,2,3,1))

  def minimalBst(list: List[Int]): Tree[Int] = {
    require(list.sorted == list, "Sorted list is required!")

    def minimalBst(list: List[Int], startIdx: Int, lastIdx: Int): Tree[Int] = {
      if (lastIdx < startIdx) Empty

      val middleIdx: Int = (startIdx + lastIdx)/2
      val left = minimalBst(list, startIdx, middleIdx - 1)
      val right = minimalBst(list, middleIdx + 1, lastIdx)
      BinaryTree(left, list(middleIdx), right)
    }

    minimalBst(list, 0, list.length - 1)
  }


  /* Question 4 */
  def height(tree: Tree[Int]): Int = tree match {
    case Empty => 0
    case Leaf(_) => 1
    case BinaryTree(left, _, right) => math.max(height(left), height(right)) + 1
  }

  def isBalanced(tree: Tree[Int]): Boolean = tree match {
    case Empty => true
    case Leaf(_) => true
    case BinaryTree(left, _, right) => math.abs(height(left) - height(right)) <= 1
  }

  /*
          1
         / \
        2   3
             \
              4
               \
                5
   */
  val testTree = BinaryTree(Leaf(2), 1, BinaryTree(Empty, 3, BinaryTree(Empty, 4, Leaf(5))))
  height(testTree)
  isBalanced(testTree)
  val balancedTree = BinaryTree(Leaf(1), 2, Leaf(3))
  height(balancedTree)
  isBalanced(balancedTree)
  isBalanced(Empty)
  isBalanced(Leaf(1))


  /* Question 5 */
  def isBST(tree: Tree[Int]): Boolean = {
    tree match {
      case Empty => true
      case Leaf(_) => true
      case bt: BinaryTree[Int] => isBST(bt, Int.MinValue, Int.MaxValue)
    }
    def isBST(tree: Tree[Int], min: Int, max: Int): Boolean = tree match {
      case Empty => true
      case Leaf(_) => true
      case BinaryTree(left, x, right) => {
        if (x <= min || x > max) false
        if (!isBST(left, min, x) || !isBST(right, x, max)) false else true
      }
    }
  }

  val notBST = BinaryTree(Leaf(3), 2, Leaf(1))
  isBST(notBST)
  val bst = BinaryTree(Leaf(1), 2, Leaf(3))
  isBST(bst)
  val bst2 = BinaryTree(BinaryTree(Leaf(1), 2, Leaf(3)), 4, BinaryTree(Leaf(6), 5, Leaf(7)))
  isBST(bst2)
  val notBST2 = BinaryTree(BinaryTree(Leaf(1), 2, Leaf(4)), 3, BinaryTree(Leaf(6), 5, Leaf(7)))
  isBST(notBST2)


  /* Question 10 */
  def isSubTree(parent: Tree[Int], child: Tree[Int]): Boolean = {
    ???
  }
}